# ideea_levcost.R
# Levelized cost of energy (production) for an energyRt technology object.
# Creates a minimal single-technology energyRt model with unit demand, solves it
# with GLPK (or a user-supplied solver), and returns a per-year LCOE table plus
# an NPV-weighted scalar, together with a structured cost-component breakdown.

# ── S3 classes ─────────────────────────────────────────────────────────────────
# Results for a single technology are returned as class "ideea_levcost" (list).
# Results for a list of technologies are returned as class "ideea_levcost_list".

#' Levelized Cost of Energy for a Technology
#'
#' Builds a minimal \code{energyRt} model around a single \code{technology}
#' object (or a list of technologies), solves it, and returns the levelized cost
#' of producing one unit of the specified output commodity per year, together
#' with a structured breakdown of cost components.
#'
#' @param object   An energyRt \code{technology} S4 object, or a named/unnamed
#'   \code{list} of technology objects (each evaluated independently).
#' @param comm     Character vector or \code{NULL}. Output commodity/ies for
#'   which to compute LCOE.  Default \code{NULL} uses all commodities in the
#'   resolved output group.
#' @param group    Character or \code{NULL}. Output group name.  If the
#'   technology has multiple output groups and \code{group = NULL}, an
#'   informative error is raised listing available groups.
#' @param repo     A \code{repository} object or named list of energyRt objects
#'   (supplies, commodities, …) to supplement the mini model.  Supply objects
#'   whose \code{commodity} slot matches a technology input are reused; all
#'   unmatched inputs receive auto-created zero-cost supply.
#' @param discount Numeric (0–1). Weighted average cost of capital (WACC)
#'   applied to all years / regions.  Default \code{0.05}.
#' @param base_year Integer or \code{NULL}.  Base / first milestone year.
#'   \code{NULL} derives it from the resolved horizon or the current year.
#' @param horizon  A \code{horizon} object, a numeric vector of years, or
#'   \code{NULL} (default).  When \code{NULL} the operational life
#'   (\code{tech@olife}) is used; falls back to 20 years if empty.
#' @param calendar A \code{calendar} object or \code{NULL} (→ \code{ANNUAL}).
#' @param region   Character or \code{NULL}. Region name for the mini model.
#'   \code{NULL} takes the first non-\code{NA} entry from \code{object@region}
#'   or \code{object@capacity$region}; falls back to \code{"REGION"}.
#' @param weather  A \code{weather} object, a list of \code{weather} objects, or
#'   \code{NULL}.  \strong{Required} when the technology or any matched supply
#'   from \code{repo} has a non-empty \code{@@weather} slot.
#' @param solver   Solver specification list.
#'   Default \code{energyRt::solver_options$glpk}.
#' @param full_output Logical. \code{TRUE} returns the full solved
#'   \code{scenario} with all LCOE tables in \code{scenario@@misc}.
#'   \code{FALSE} (default) returns a \code{list} of class
#'   \code{"ideea_levcost"}.
#' @param verbose  Logical.  Print messages for auto-created objects.
#'   Default \code{TRUE}.
#' @param ...      Additional arguments forwarded to
#'   \code{energyRt::interpolate()}.
#'
#' @return
#'   \describe{
#'     \item{\code{$levcost}}{data.frame – total levelized cost by year
#'       (columns: tech, group, comm, region, year, levcost).}
#'     \item{\code{$levcost_npv}}{Named numeric – NPV-weighted average LCOE.}
#'     \item{\code{$cost_breakdown}}{data.frame – tidy cost components by year
#'       (columns: tech, group, comm, region, year, component, value).
#'       Components: \code{eac}, \code{fixom}, \code{varom}, \code{supply},
#'       \code{import}, \code{export} (negative, a credit), and optionally
#'       \code{overnight_inv}.}
#'     \item{\code{$cost_breakdown_npv}}{data.frame – NPV-weighted sum of each
#'       component (columns: tech, group, comm, component, value).}
#'     \item{\code{$cost_yearly}}{data.frame – wide table of raw undiscounted
#'       costs by component (eac, fixom, varom, supply, import, export),
#'       total cost, activity, capacity, and per-commodity outputs (out_*)
#'       and inputs (inp_*), one row per year.}
#'     \item{\code{$levcost_per_act}}{data.frame or \code{NULL} – for
#'       technologies with grouped output: cost per unit of activity
#'       (\code{vTechAct}) by year.}
#'     \item{\code{$scenario}}{The solved \code{scenario} object.}
#'   }
#'   When \code{object} is a list of technologies, a named list of class
#'   \code{"ideea_levcost_list"} is returned (one element per technology).
#'
#' @examples
#' \dontrun{
#' # Single technology:
#' lc <- ideea_levcost(LDVG, group = "o",
#'                     repo   = list(SUP_GSL, SUP_BIO, GSL, BIO),
#'                     discount = 0.07, base_year = 2024)
#' lc$levcost
#' lc$levcost_npv
#' lc$cost_breakdown
#' autoplot(lc)
#' autoplot(lc, type = "npv")
#'
#' # List of technologies (each solved independently):
#' lc_list <- ideea_levcost(list(LDVG, LDVE), group = "o",
#'                          repo = list(SUP_GSL, GSL, ELC),
#'                          discount = 0.07, base_year = 2024)
#' autoplot(lc_list, type = "npv")
#' }
#'
# ── tech_share_frontier ────────────────────────────────────────────────────────
# Standalone helper: extract feasible share ranges for ALL grouped inputs and
# outputs of a technology.  No solve required – purely geometric.
#
# Returns a data.frame with columns:
#   tech, direction ("input"/"output"), group, comm, others,
#   share_lo, share_hi           – direct per-commodity constraints
#   share_lo_eff, share_hi_eff   – effective range after intersecting all group
#                                  member constraints (the true feasible band)
#
# Only groups where at least one commodity has an explicit share.up or
# share.lo > 0 are included (skip fully-unconstrained groups).
#' @export
tech_share_frontier <- function(object) {
  stopifnot(inherits(object, "technology"))
  tech_name <- if (nzchar(object@name)) object@name else "TECH"

  # ── helper: extract share constraints for a vector of comms from @ceff ──
  .extract_shares <- function(comms) {
    su <- setNames(rep(NA_real_, length(comms)), comms)
    sl <- setNames(rep(0,        length(comms)), comms)
    if (nrow(object@ceff) == 0) return(list(up = su, lo = sl))
    ci <- object@ceff[object@ceff$comm %in% comms, , drop = FALSE]
    if ("share.up" %in% names(ci))
      for (cm in comms) {
        r <- ci[ci$comm == cm & !is.na(ci$share.up), , drop = FALSE]
        if (nrow(r) > 0) su[[cm]] <- r$share.up[1]
      }
    if ("share.lo" %in% names(ci))
      for (cm in comms) {
        r <- ci[ci$comm == cm & !is.na(ci$share.lo), , drop = FALSE]
        if (nrow(r) > 0) sl[[cm]] <- r$share.lo[1]
      }
    if ("share.fx" %in% names(ci))
      for (cm in comms) {
        r <- ci[ci$comm == cm & !is.na(ci$share.fx), , drop = FALSE]
        if (nrow(r) > 0) { su[[cm]] <- r$share.fx[1]; sl[[cm]] <- r$share.fx[1] }
      }
    list(up = su, lo = sl)
  }

  # ── helper: compute effective range for each comm in a group ─────────────
  .eff_range <- function(su, sl) {
    comms <- names(su)
    # treat NA share_up as 1 for sum purposes
    hi <- ifelse(is.finite(su), su, 1)
    lo <- sl
    lo_eff <- setNames(numeric(length(comms)), comms)
    hi_eff <- setNames(numeric(length(comms)), comms)
    for (cm in comms) {
      others_hi_sum <- sum(hi[names(hi) != cm])
      others_lo_sum <- sum(lo[names(lo) != cm])
      raw_lo <- if (is.finite(sl[[cm]])) sl[[cm]] else 0
      raw_hi <- if (is.finite(su[[cm]])) su[[cm]] else 1
      lo_eff[[cm]] <- max(raw_lo, 1 - others_hi_sum)
      hi_eff[[cm]] <- min(raw_hi, 1 - others_lo_sum)
    }
    list(lo_eff = lo_eff, hi_eff = hi_eff)
  }

  all_rows <- list()

  # ── Input groups ──────────────────────────────────────────────────────────
  in_df     <- object@input
  in_groups <- character(0)
  if ("group" %in% names(in_df))
    in_groups <- unique(in_df$group[!is.na(in_df$group) & nzchar(as.character(in_df$group))])

  for (grp in in_groups) {
    gc <- in_df$comm[!is.na(in_df$group) & as.character(in_df$group) == grp]
    sh <- .extract_shares(gc)
    # skip if nothing is constrained
    if (!any(is.finite(sh$up[gc]) | (sh$lo[gc] > 0))) next
    eff <- .eff_range(sh$up, sh$lo)
    for (cm in gc) {
      all_rows[[paste("in", grp, cm, sep = "_")]] <- data.frame(
        tech         = tech_name,
        direction    = "input",
        group        = grp,
        comm         = cm,
        others       = paste(setdiff(gc, cm), collapse = "+"),
        n_in_group   = length(gc),
        share_lo     = if (is.finite(sh$lo[[cm]])) sh$lo[[cm]] else 0,
        share_hi     = if (is.finite(sh$up[[cm]])) sh$up[[cm]] else 1,
        share_lo_eff = max(0, eff$lo_eff[[cm]]),
        share_hi_eff = min(1, eff$hi_eff[[cm]]),
        stringsAsFactors = FALSE
      )
    }
  }

  # ── Output groups ─────────────────────────────────────────────────────────
  out_df <- object@output
  out_groups <- character(0)
  if ("group" %in% names(out_df))
    out_groups <- unique(out_df$group[!is.na(out_df$group) & nzchar(as.character(out_df$group))])

  for (grp in out_groups) {
    gc <- out_df$comm[!is.na(out_df$group) & as.character(out_df$group) == grp]
    if (length(gc) < 2) next          # single-output group: share is always 1
    sh <- .extract_shares(gc)
    if (!any(is.finite(sh$up[gc]) | (sh$lo[gc] > 0))) next
    eff <- .eff_range(sh$up, sh$lo)
    for (cm in gc) {
      all_rows[[paste("out", grp, cm, sep = "_")]] <- data.frame(
        tech         = tech_name,
        direction    = "output",
        group        = grp,
        comm         = cm,
        others       = paste(setdiff(gc, cm), collapse = "+"),
        n_in_group   = length(gc),
        share_lo     = if (is.finite(sh$lo[[cm]])) sh$lo[[cm]] else 0,
        share_hi     = if (is.finite(sh$up[[cm]])) sh$up[[cm]] else 1,
        share_lo_eff = max(0, eff$lo_eff[[cm]]),
        share_hi_eff = min(1, eff$hi_eff[[cm]]),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(all_rows) == 0L) return(NULL)
  df <- do.call(rbind, all_rows)
  rownames(df) <- NULL
  df
}

#' @export
ideea_levcost <- function(
    object,
    comm           = NULL,
    group          = NULL,
    repo           = NULL,
    fuel_costs     = NULL,
    discount       = 0.05,
    base_year      = NULL,
    horizon        = NULL,
    calendar       = NULL,
    region         = NULL,
    weather        = NULL,
    frontier       = TRUE,
    solver         = energyRt::solver_options$glpk,
    full_output    = FALSE,
    verbose        = TRUE,
    ...
) {

  # ── 0. Handle list of technologies ─────────────────────────────────────────
  if (is.list(object) && !inherits(object, "technology")) {
    if (length(object) == 0) stop("`object` is an empty list.")
    if (!all(sapply(object, inherits, "technology")))
      stop("All elements of `object` must be energyRt 'technology' objects.")

    results <- lapply(object, function(tech) {
      ideea_levcost(
        tech,
        comm           = comm,
        group          = group,
        repo           = repo,
        discount       = discount,
        base_year      = base_year,
        horizon        = horizon,
        calendar       = calendar,
        region         = region,
        weather        = weather,
        frontier       = frontier,
        solver         = solver,
        full_output    = full_output,
        verbose        = verbose,
        ...
      )
    })

    tech_names <- sapply(object, function(t) {
      nm <- t@name
      if (length(nm) == 1 && nzchar(nm)) nm else NA_character_
    })
    names(results) <- tech_names
    class(results) <- c("ideea_levcost_list", "list")
    return(results)
  }

  # ── 1. Validate input ───────────────────────────────────────────────────────
  if (!inherits(object, "technology")) {
    stop("`object` must be an energyRt 'technology' object or a list thereof.")
  }
  tech_name <- if (nzchar(object@name)) object@name else "TECH"

  # ── 1b. Strip capacity constraints for LCOE ────────────────────────────────
  # The LCOE mini-model uses unit demand; pre-existing stock and capacity bounds
  # would distort costs (e.g. fixom proportional to full fleet, not per-unit).
  # Clear all constraint columns, keeping only region + year for interpolation.
  if (nrow(object@capacity) > 0) {
    cap_constraint_cols <- c("stock", "cap.lo", "cap.up", "cap.fx",
                             "ncap.lo", "ncap.up", "ncap.fx",
                             "ret.lo", "ret.up", "ret.fx")
    for (col in intersect(cap_constraint_cols, names(object@capacity))) {
      object@capacity[[col]] <- NA_real_
    }
    if (verbose) message("Capacity constraints stripped for LCOE mini-model.")
  }

  # ── 2. Resolve region ───────────────────────────────────────────────────────
  if (is.null(region)) {
    reg_candidates <- unique(na.omit(object@region))
    # Also try capacity slot
    if (length(reg_candidates) == 0 && nrow(object@capacity) > 0 &&
        "region" %in% names(object@capacity)) {
      reg_candidates <- unique(na.omit(object@capacity$region))
    }
    reg_candidates <- reg_candidates[nzchar(as.character(reg_candidates)) &
                                       !is.na(reg_candidates)]
    if (length(reg_candidates) == 0) {
      region <- "REGION"
      if (verbose) message("No region found in technology; using 'REGION'.")
    } else {
      region <- reg_candidates[1]
      if (length(reg_candidates) > 1 && verbose)
        message("Multiple regions in technology; using first: '", region, "'.")
    }
  }

  # ── 3. Resolve output group / commodities ───────────────────────────────────
  out_df        <- object@output
  if (nrow(out_df) == 0) stop("Technology '", tech_name, "' has no output commodities.")

  avail_groups  <- unique(out_df$group)
  avail_groups  <- avail_groups[!is.na(avail_groups) & nzchar(as.character(avail_groups))]

  if (!is.null(group)) {
    if (!(group %in% avail_groups))
      stop("Output group '", group, "' not found in technology '", tech_name, "'.\n",
           "Available groups: ", paste(avail_groups, collapse = ", "))
    out_df_grp <- out_df[out_df$group == group, , drop = FALSE]
  } else if (length(avail_groups) > 1) {
    stop("Technology '", tech_name, "' has multiple output groups: ",
         paste(avail_groups, collapse = ", "),
         ".\nPlease specify one via the `group = ` parameter.")
  } else if (length(avail_groups) == 1) {
    group      <- avail_groups
    out_df_grp <- out_df[!is.na(out_df$group) & out_df$group == avail_groups, , drop = FALSE]
    if (nrow(out_df_grp) == 0) out_df_grp <- out_df
  } else {
    out_df_grp <- out_df
  }

  # Filter to requested comms
  if (!is.null(comm)) {
    miss_comm <- setdiff(comm, out_df_grp$comm)
    if (length(miss_comm) > 0)
      stop("Commodity/ies not found in resolved output: ",
           paste(miss_comm, collapse = ", "),
           ".\nAvailable: ", paste(out_df_grp$comm, collapse = ", "))
    out_comms <- comm
  } else {
    out_comms <- out_df_grp$comm
  }

  # All actual outputs in the selected group. May be a superset of out_comms when
  # comm= restricts which commodity to use for LCOE normalisation.  The model
  # must include commodity objects and demands for every group output so that
  # energyRt set-validation does not complain about undefined commodities.
  all_out_comms <- out_df_grp$comm

  # Whether the technology uses grouped output (relevant for per-activity LCOE)
  has_grouped_output <- !is.null(group)

  # ── 3.5. Extract output share constraints from tech@ceff ───────────────────
  # share_up[i] = max fraction of group total that comm_i may produce (NA = unconstrained)
  # share_lo[i] = min fraction (NA / 0 = no lower bound)
  share_up <- setNames(rep(NA_real_, length(out_comms)), out_comms)
  share_lo <- setNames(rep(0,        length(out_comms)), out_comms)
  if (has_grouped_output && nrow(object@ceff) > 0) {
    ceff_out <- object@ceff[object@ceff$comm %in% out_comms, , drop = FALSE]
    if ("share.up" %in% names(ceff_out)) {
      for (cm in out_comms) {
        rows <- ceff_out[ceff_out$comm == cm & !is.na(ceff_out$share.up), , drop = FALSE]
        if (nrow(rows) > 0) share_up[[cm]] <- rows$share.up[1]
      }
    }
    if ("share.lo" %in% names(ceff_out)) {
      for (cm in out_comms) {
        rows <- ceff_out[ceff_out$comm == cm & !is.na(ceff_out$share.lo), , drop = FALSE]
        if (nrow(rows) > 0) share_lo[[cm]] <- rows$share.lo[1]
      }
    }
    # share.fx overrides both bounds
    if ("share.fx" %in% names(ceff_out)) {
      for (cm in out_comms) {
        rows <- ceff_out[ceff_out$comm == cm & !is.na(ceff_out$share.fx), , drop = FALSE]
        if (nrow(rows) > 0) {
          share_up[[cm]] <- rows$share.fx[1]
          share_lo[[cm]] <- rows$share.fx[1]
        }
      }
    }
  }

  # Frontier analysis is meaningful when >= 2 output comms have share.up defined
  do_frontier <- frontier &&
    has_grouped_output &&
    length(out_comms) >= 2 &&
    any(is.finite(share_up))

  # ── 4. Collect all input commodities ────────────────────────────────────────
  in_comms  <- unique(object@input$comm)

  # ── 4.5. Extract input group share constraints ───────────────────────────────
  # Detect grouped inputs and their share.up/share.lo constraints for the
  # feasible input-mix visualisation (no extra solves — purely geometric).
  in_df     <- object@input
  in_groups <- if ("group" %in% names(in_df))
    unique(in_df$group[!is.na(in_df$group) & nzchar(as.character(in_df$group))])
  else character(0)

  in_group_comms <- list()
  in_share_up    <- list()
  in_share_lo    <- list()

  for (.grp in in_groups) {
    .gc <- in_df$comm[!is.na(in_df$group) & as.character(in_df$group) == .grp]
    in_group_comms[[.grp]] <- .gc
    .su <- setNames(rep(NA_real_, length(.gc)), .gc)
    .sl <- setNames(rep(0,        length(.gc)), .gc)
    if (nrow(object@ceff) > 0) {
      .ci <- object@ceff[object@ceff$comm %in% .gc, , drop = FALSE]
      if ("share.up" %in% names(.ci))
        for (.cm in .gc) {
          .r <- .ci[.ci$comm == .cm & !is.na(.ci$share.up), , drop = FALSE]
          if (nrow(.r) > 0) .su[[.cm]] <- .r$share.up[1]
        }
      if ("share.lo" %in% names(.ci))
        for (.cm in .gc) {
          .r <- .ci[.ci$comm == .cm & !is.na(.ci$share.lo), , drop = FALSE]
          if (nrow(.r) > 0) .sl[[.cm]] <- .r$share.lo[1]
        }
      if ("share.fx" %in% names(.ci))
        for (.cm in .gc) {
          .r <- .ci[.ci$comm == .cm & !is.na(.ci$share.fx), , drop = FALSE]
          if (nrow(.r) > 0) { .su[[.cm]] <- .r$share.fx[1]; .sl[[.cm]] <- .r$share.fx[1] }
        }
    }
    in_share_up[[.grp]] <- .su
    in_share_lo[[.grp]] <- .sl
  }
  rm(.grp, .gc, .su, .sl, .ci, .cm, .r)

  # ── 5. Resolve horizon ──────────────────────────────────────────────────────
  if (inherits(horizon, "horizon")) {
    hor       <- horizon
    hor_years <- sort(hor@period)
  } else if (is.numeric(horizon) || is.integer(horizon)) {
    hor_years <- sort(as.integer(horizon))
    hor       <- energyRt::newHorizon(period = hor_years)
  } else {
    # Auto-derive from olife slot
    olife_val <- NULL
    if (nrow(object@olife) > 0) {
      ol_col <- intersect(c("olife", "value"), names(object@olife))
      if (length(ol_col) > 0)
        olife_val <- max(object@olife[[ol_col[1]]], na.rm = TRUE)
    }
    if (is.null(olife_val) || !is.finite(olife_val) || olife_val <= 0) {
      olife_val <- 20
      if (verbose) message("No olife found in technology; using default horizon of 20 years.")
    }
    by        <- if (!is.null(base_year)) as.integer(base_year) else
      as.integer(format(Sys.Date(), "%Y"))
    hor_years <- seq(by, by + as.integer(olife_val) - 1L)
    hor       <- energyRt::newHorizon(period = hor_years)
    if (verbose)
      message("Auto-created horizon: ", min(hor_years), "\u2013", max(hor_years),
              " (", length(hor_years), " years) from technology olife.")
  }

  if (is.null(base_year)) base_year <- min(hor_years)

  # ── 6. Calendar ─────────────────────────────────────────────────────────────
  if (is.null(calendar)) calendar <- energyRt::newCalendar()

  # ── 7. Pull existing supplies / commodities from repo ───────────────────────
  repo_supplies <- list()
  repo_comms    <- list()

  if (!is.null(repo)) {
    objs <- if (inherits(repo, "repository")) repo@data else
      if (is.list(repo)) repo else
        stop("`repo` must be a 'repository' object or a named list.")

    for (obj in objs) {
      if (inherits(obj, "supply")) {
        key     <- if (nzchar(obj@commodity)) obj@commodity else obj@name
        repo_supplies[[key]] <- obj
      }
      if (inherits(obj, "commodity")) {
        repo_comms[[obj@name]] <- obj
      }
    }
  }

  # ── 7b. Weather validation ──────────────────────────────────────────────────
  # Check whether the technology or any matched supply requires a weather object.
  tech_has_weather <- nrow(object@weather) > 0 &&
    any(sapply(object@weather, function(col) any(nzchar(as.character(col)))))

  supply_has_weather <- any(sapply(repo_supplies, function(s) {
    nrow(s@weather) > 0 &&
      any(sapply(s@weather, function(col) any(nzchar(as.character(col)))))
  }))

  if ((tech_has_weather || supply_has_weather) && is.null(weather)) {
    who <- character(0)
    if (tech_has_weather) who <- c(who, paste0("technology '", tech_name, "'"))
    if (supply_has_weather) {
      wnames <- names(Filter(function(s) {
        nrow(s@weather) > 0 &&
          any(sapply(s@weather, function(col) any(nzchar(as.character(col)))))
      }, repo_supplies))
      who <- c(who, paste0("supply '", wnames, "'"))
    }
    stop(
      "The following objects have a non-empty @weather slot and require a ",
      "weather object:\n  ", paste(who, collapse = "\n  "),
      "\nPlease supply one via `weather = `."
    )
  }

  # Normalise weather to a list for inclusion in the model repository
  weather_objects <- list()
  if (!is.null(weather)) {
    if (inherits(weather, "weather")) {
      weather_objects <- list(weather)
    } else if (is.list(weather)) {
      weather_objects <- weather
    } else {
      stop("`weather` must be a 'weather' object or a list of 'weather' objects.")
    }
  }

  # ── 8. Auto-create commodity objects (if not in repo) ───────────────────────
  # Include @aux$acomm so auxiliary commodities (e.g. PETRO_REFUEL, CNG_LEAK)
  # referenced in @aeff are declared in the mini-model commodity set.
  aux_comms        <- unique(object@aux$acomm)
  all_comms_needed <- unique(c(in_comms, all_out_comms, aux_comms))
  commodity_objects <- list()

  for (cm in all_comms_needed) {
    if (!is.null(repo_comms[[cm]])) {
      # Strip emission factors — the mini-model doesn't track emissions and
      # energyRt would require every referenced emission commodity (e.g. CO2)
      # to be registered as a commodity object, which is unnecessary here.
      cm_obj <- repo_comms[[cm]]
      if (isS4(cm_obj) && .hasSlot(cm_obj, "emis") && nrow(cm_obj@emis) > 0) {
        cm_obj@emis <- cm_obj@emis[0L, , drop = FALSE]
      }
      commodity_objects[[cm]] <- cm_obj
    } else {
      unit_val <- ""
      in_row   <- object@input [object@input$comm  == cm, , drop = FALSE]
      out_row  <- object@output[object@output$comm == cm, , drop = FALSE]
      aux_row  <- object@aux   [object@aux$acomm   == cm, , drop = FALSE]
      if (nrow(in_row)  > 0 && "unit" %in% names(in_row))  unit_val <- in_row$unit[1]
      if (nrow(out_row) > 0 && "unit" %in% names(out_row)) unit_val <- out_row$unit[1]
      if (nrow(aux_row) > 0 && "unit" %in% names(aux_row) && !nzchar(unit_val))
        unit_val <- aux_row$unit[1]
      commodity_objects[[cm]] <- energyRt::newCommodity(
        name      = cm,
        timeframe = "ANNUAL",
        unit      = if (!is.na(unit_val)) unit_val else ""
      )
      if (verbose) message("Created commodity '", cm, "'.")
    }
  }

  # Determine which aux commodities are consumed as inputs (cinp2ainp or
  # act2ainp-family rows non-NA in @aeff) — these need zero-cost supply objects.
  inp_aux_cols <- c("cinp2ainp", "cout2ainp", "act2ainp", "cap2ainp", "ncap2ainp",
                    "sinp2ainp", "sout2ainp", "stg2ainp")
  aux_inp_comms <- character(0)
  if (nrow(object@aeff) > 0 && length(aux_comms) > 0) {
    aeff_inp_cols <- intersect(inp_aux_cols, names(object@aeff))
    if (length(aeff_inp_cols) > 0) {
      for (ac in aux_comms) {
        rows <- object@aeff[!is.na(object@aeff$acomm) & object@aeff$acomm == ac,
                            aeff_inp_cols, drop = FALSE]
        if (any(!is.na(rows))) aux_inp_comms <- c(aux_inp_comms, ac)
      }
    }
  }

  # ── 9. Auto-create zero-cost supply for unmatched inputs ────────────────────
  supply_objects <- list()

  for (cm in in_comms) {
    if (!is.null(repo_supplies[[cm]])) {
      # Remap any region-specific data to the mini-model's resolved region so
      # that supplies defined for e.g. "IND" work in a model whose region was
      # resolved as "REGION" (or vice-versa).  Both the @region slot and any
      # region column in @availability must be updated.
      sup <- repo_supplies[[cm]]
      if (isS4(sup)) {
        upd_args <- list()
        if (.hasSlot(sup, "region") && length(sup@region) > 0)
          upd_args$region <- region
        if (.hasSlot(sup, "availability") &&
            nrow(sup@availability) > 0 &&
            "region" %in% names(sup@availability)) {
          new_ava <- sup@availability
          new_ava$region <- region
          upd_args$availability <- new_ava
        }
        if (length(upd_args) > 0)
          sup <- do.call(update, c(list(sup), upd_args))
      }
      supply_objects[[cm]] <- sup
    } else {
      # fuel_costs: named numeric vector [commodity -> cost per unit]
      fc_val <- if (!is.null(fuel_costs) && !is.null(fuel_costs[[cm]]) &&
                    is.finite(as.numeric(fuel_costs[[cm]])))
        as.numeric(fuel_costs[[cm]]) else 0
      supply_objects[[cm]] <- energyRt::newSupply(
        name         = paste0("SUP_", cm),
        commodity    = cm,
        region       = region,
        availability = data.frame(
          region = region,
          year   = as.integer(base_year),
          cost   = fc_val,
          stringsAsFactors = FALSE
        )
      )
      if (verbose) message("Created supply for '", cm, "' (cost = ", fc_val, ").")
    }
  }

  # Zero-cost supplies for input-type aux commodities (e.g. PETRO_REFUEL
  # consumed via cinp2ainp).  Reuse repo supply if one exists, otherwise create
  # unconstrained zero-cost supply so the mini-model balances.
  for (cm in aux_inp_comms) {
    if (!is.null(repo_supplies[[cm]])) {
      sup <- repo_supplies[[cm]]
      if (isS4(sup)) {
        upd_args <- list()
        if (.hasSlot(sup, "region") && length(sup@region) > 0)
          upd_args$region <- region
        if (.hasSlot(sup, "availability") &&
            nrow(sup@availability) > 0 &&
            "region" %in% names(sup@availability)) {
          new_ava <- sup@availability; new_ava$region <- region
          upd_args$availability <- new_ava
        }
        if (length(upd_args) > 0) sup <- do.call(update, c(list(sup), upd_args))
      }
      supply_objects[[cm]] <- sup
    } else {
      supply_objects[[cm]] <- energyRt::newSupply(
        name         = paste0("SUP_", cm),
        commodity    = cm,
        region       = region,
        availability = data.frame(region = region, year = as.integer(base_year),
                                  cost = 0, stringsAsFactors = FALSE)
      )
      if (verbose) message("Created zero-cost aux supply for '", cm, "'.")
    }
  }

  # ── 10. Inner helpers: demand creation, model assembly, solve ───────────────
  # Create demand objects from a named [comm -> quantity] vector.
  make_demands_ <- function(dvals) {
    lapply(setNames(names(dvals), names(dvals)), function(cm) {
      dv      <- max(dvals[[cm]], 1e-9)   # avoid zero demand (some solvers reject it)
      unit_val <- ""
      out_row  <- object@output[object@output$comm == cm, , drop = FALSE]
      if (nrow(out_row) > 0 && "unit" %in% names(out_row)) unit_val <- out_row$unit[1]
      energyRt::newDemand(
        name      = paste0("DEM_", cm),
        commodity = cm,
        unit      = if (!is.na(unit_val)) unit_val else "",
        region    = region,
        dem       = data.frame(region = region, year = as.integer(base_year),
                               dem = dv, stringsAsFactors = FALSE)
      )
    })
  }

  # Assemble a mini-model from given demand objects and solve it.
  build_and_solve_ <- function(demand_objs, suffix = "") {
    mdl <- energyRt::newModel(
      name     = paste0("levcost_", tech_name, suffix),
      desc     = paste0("Mini model for levelized cost of '", tech_name, "'"),
      data     = newRepository("repo_lc",
                   c(unname(commodity_objects), list(object),
                     unname(supply_objects), unname(demand_objs),
                     weather_objects)),
      region   = region,
      discount = discount,
      calendar = calendar,
      horizon  = hor
    )
    sn   <- paste0("lc_", tech_name, suffix)
    scen <- energyRt::interpolate(mdl, name = sn, ...)
    scen <- energyRt::write_sc(scen)
    scen <- energyRt::solve_scenario(scen)
    energyRt::read_solution(scen)
  }

  # ── 11. Inner closure: extract LCOE results from a solved scenario ─────────
  # primary_comm: which commodity's demand to use for LCOE normalisation.
  # NULL → normalise by total demand (base scenario, combined LCOE).
  comm_label <- paste(out_comms, collapse = "+")
  group_val  <- if (!is.null(group)) group else NA_character_

  levcost_extract_ <- function(sc, dem_vals, primary_comm = NULL) {
    sfget <- function(v) tryCatch({
      d <- energyRt::getData(sc, name = v, merge = TRUE, drop.zeros = FALSE)
      if (is.null(d) || nrow(d) == 0) return(NULL); as.data.frame(d)
    }, error = function(e) NULL)

    agg_yr <- function(df) {
      if (is.null(df) || nrow(df) == 0 || !"year" %in% names(df)) return(NULL)
      df$year <- as.integer(df$year)
      out <- aggregate(df[["value"]], by = list(year = df$year),
                       FUN = sum, na.rm = TRUE)
      names(out)[2] <- "value"; out
    }

    # Normalisation strategy:
    #   frontier scenario (primary_comm set) → scalar: demand of primary commodity
    #   single-output base                   → scalar: 1 (unit demand)
    #   grouped-output base (primary_comm NULL, has_grouped_output) →
    #       year-specific vTechAct, so mixed output units are avoided
    use_act_norm <- is.null(primary_comm) && has_grouped_output
    comm_lbl <- if (!is.null(primary_comm)) primary_comm else
                  if (use_act_norm) "activity" else comm_label

    tc <- sfget("vTotalCost")

    if (is.null(tc) || nrow(tc) == 0) {
      lc_tbl <- data.frame(tech = tech_name, group = group_val, comm = comm_lbl,
                            region = region, year = NA_integer_, levcost = NA_real_,
                            stringsAsFactors = FALSE)
      return(list(levcost = lc_tbl,
                  levcost_npv        = NA_real_,
                  cost_breakdown     = NULL,
                  cost_breakdown_npv = NULL,
                  cost_yearly        = NULL,
                  levcost_per_act    = NULL,
                  scen               = sc))
    }

    tc_df      <- as.data.frame(tc)
    tc_df$year <- as.integer(tc_df$year)
    tc_df[["scenario"]] <- NULL

    # Build year-specific normaliser vector (norm_yr) and scalar fallback (prim_dem)
    norm_yr  <- NULL   # NULL → use scalar prim_dem
    prim_dem <- 1

    if (use_act_norm) {
      act_ag <- agg_yr(sfget("vTechAct"))
      if (!is.null(act_ag) && nrow(act_ag) > 0) {
        norm_yr <- setNames(act_ag$value, as.character(act_ag$year))
      }
      # prim_dem stays 1 as fallback if no activity data
    } else if (!is.null(primary_comm)) {
      prim_dem <- max(dem_vals[[primary_comm]], 1e-9)
    } else {
      prim_dem <- max(sum(unlist(dem_vals)), 1e-9)
    }

    # Helper: divide a year-keyed value vector by the appropriate normaliser
    normalise_ <- function(yr_int, val) {
      if (!is.null(norm_yr)) {
        nrm <- norm_yr[as.character(yr_int)]
        ifelse(is.finite(nrm) & nrm > 0, val / nrm, NA_real_)
      } else {
        val / prim_dem
      }
    }

    lc_tbl        <- tc_df
    names(lc_tbl)[names(lc_tbl) == "value"] <- "levcost"
    lc_tbl$levcost <- normalise_(lc_tbl$year, lc_tbl$levcost)
    lc_tbl$tech  <- tech_name; lc_tbl$group <- group_val; lc_tbl$comm <- comm_lbl
    cf <- c("tech", "group", "comm", "region", "year", "levcost")
    lc_tbl <- lc_tbl[, c(cf, setdiff(names(lc_tbl), cf)), drop = FALSE]

    # Standard NPV LCOE: NPV(total_cost) / NPV(quantity)
    # Aggregate raw costs by year (guards against multi-region rows)
    tc_agg <- aggregate(value ~ year,
                        data.frame(year = as.integer(tc_df$year), value = tc_df$value),
                        sum, na.rm = TRUE)
    yr_int  <- tc_agg$year
    te_npv  <- yr_int - as.integer(base_year)
    dsc_tc  <- (1 + discount)^te_npv
    npv_num <- sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0, tc_agg$value / dsc_tc, 0))
    # npv_act_for_act: NPV(vTechAct) alone — used for per-activity cost breakdown.
    # NULL when vTechAct is unavailable or not meaningful.
    npv_act_for_act <- NULL
    if (!is.null(norm_yr)) {
      # Activity-normalised (has_grouped_output, primary_comm = NULL)
      qty_yr  <- norm_yr[as.character(yr_int)]
      npv_den <- sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0 & is.finite(qty_yr),
                            qty_yr / dsc_tc, 0))
      npv_act_for_act <- npv_den   # norm_yr IS vTechAct for base scenario
    } else if (!is.null(primary_comm)) {
      # levcost_output = levcost_activity / cact2cout[primary_comm]
      # so npv_den = NPV(vTechAct) * cact2cout[primary_comm]
      cact2cout_val <- NA_real_
      if (nrow(object@ceff) > 0 && "cact2cout" %in% names(object@ceff)) {
        r <- object@ceff$cact2cout[object@ceff$comm == primary_comm &
                                   !is.na(object@ceff$cact2cout)]
        if (length(r) > 0) cact2cout_val <- as.numeric(r[1])
      }
      act_fr <- agg_yr(sfget("vTechAct"))
      if (!is.null(act_fr) && nrow(act_fr) > 0 &&
          is.finite(cact2cout_val) && cact2cout_val > 0) {
        dsc_fr  <- (1 + discount)^(act_fr$year - as.integer(base_year))
        npv_act <- sum(ifelse(is.finite(dsc_fr) & dsc_fr > 0 & is.finite(act_fr$value),
                              act_fr$value / dsc_fr, 0))
        npv_den <- npv_act * cact2cout_val
        npv_act_for_act <- npv_act   # store NPV(vTechAct) for per-activity breakdown
      } else {
        npv_den <- prim_dem * sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0, 1 / dsc_tc, 0))
      }
    } else {
      # Scalar normaliser: total demand constant over all years
      npv_den <- prim_dem * sum(ifelse(is.finite(dsc_tc) & dsc_tc > 0, 1 / dsc_tc, 0))
    }
    lc_npv <- if (is.finite(npv_den) && npv_den > 0) npv_num / npv_den else NA_real_

    # Cost components via getData().
    # energyRt dev has separate vTechFixom / vTechVarom solver variables;
    # the installed v0.50.1-beta combines them into vTechOMCost.
    # Try solver variables first, fall back to parameter × variable products.
    param_var_cost_ <- function(pname, vname) {
      p <- sfget(pname);  v <- sfget(vname)
      if (is.null(p) || is.null(v) || nrow(p) == 0 || nrow(v) == 0) return(NULL)
      jc  <- intersect(c("year", "region", "tech", "slice"),
                       intersect(names(p), names(v)))
      m   <- merge(p[, c(jc, "value"), drop = FALSE],
                   v[, c(jc, "value"), drop = FALSE],
                   by = jc, suffixes = c("_p", "_v"))
      if (nrow(m) == 0) return(NULL)
      agg_yr(data.frame(year = m$year, value = m$value_p * m$value_v,
                        stringsAsFactors = FALSE))
    }

    fixom_val <- agg_yr(sfget("vTechFixom"))
    varom_val <- agg_yr(sfget("vTechVarom"))
    if (is.null(fixom_val)) fixom_val <- param_var_cost_("pTechFixom", "vTechCap")
    if (is.null(varom_val)) varom_val <- param_var_cost_("pTechVarom", "vTechAct")

    cmp_raw <- list(
      eac    = agg_yr(sfget("vTechEac")),
      fixom  = fixom_val,
      varom  = varom_val,
      supply = agg_yr(sfget("vSupCost"))
    )
    im <- sfget("vImportRowCost")
    if (!is.null(im)) cmp_raw[["import"]] <- agg_yr(im)
    ex <- sfget("vExportRowCost")
    if (!is.null(ex)) {
      ea <- agg_yr(ex)
      if (!is.null(ea)) { ea$value <- -ea$value; cmp_raw[["export"]] <- ea }
    }

    cbd <- do.call(rbind, Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
      d <- cmp_raw[[nm]]; if (is.null(d)) return(NULL)
      data.frame(tech = tech_name, group = group_val, comm = comm_lbl,
                 region = region, year = as.integer(d$year),
                 component = nm, value = normalise_(as.integer(d$year), d$value),
                 stringsAsFactors = FALSE)
    })))
    if (!is.null(cbd)) rownames(cbd) <- NULL

    # ── Wide yearly table of raw undiscounted costs + activity + outputs ───────
    cost_yearly <- data.frame(tech = tech_name, group = group_val,
                              region = region, year = as.integer(tc_df$year),
                              total = tc_df$value, stringsAsFactors = FALSE)
    for (cnm in c("eac", "fixom", "varom", "supply", "import", "export")) {
      d <- cmp_raw[[cnm]]
      if (!is.null(d) && nrow(d) > 0) {
        dd <- data.frame(year = as.integer(d$year), v__ = d$value,
                         stringsAsFactors = FALSE)
        names(dd)[2] <- cnm
        cost_yearly <- merge(cost_yearly, dd, by = "year", all.x = TRUE)
      }
    }
    # Activity & capacity
    act_raw <- agg_yr(sfget("vTechAct"))
    if (!is.null(act_raw) && nrow(act_raw) > 0) {
      da <- data.frame(year = as.integer(act_raw$year), activity = act_raw$value,
                       stringsAsFactors = FALSE)
      cost_yearly <- merge(cost_yearly, da, by = "year", all.x = TRUE)
    }
    cap_raw <- agg_yr(sfget("vTechCap"))
    if (!is.null(cap_raw) && nrow(cap_raw) > 0) {
      dc <- data.frame(year = as.integer(cap_raw$year), capacity = cap_raw$value,
                       stringsAsFactors = FALSE)
      cost_yearly <- merge(cost_yearly, dc, by = "year", all.x = TRUE)
    }
    # Per-commodity outputs
    out_raw <- sfget("vTechOut")
    if (!is.null(out_raw) && nrow(out_raw) > 0) {
      out_raw$year <- as.integer(out_raw$year)
      for (cm in unique(out_raw$comm)) {
        oa <- aggregate(value ~ year, out_raw[out_raw$comm == cm, ], sum, na.rm = TRUE)
        names(oa)[2] <- paste0("out_", cm)
        cost_yearly <- merge(cost_yearly, oa, by = "year", all.x = TRUE)
      }
    }
    # Per-commodity inputs
    inp_raw <- sfget("vTechInp")
    if (!is.null(inp_raw) && nrow(inp_raw) > 0) {
      inp_raw$year <- as.integer(inp_raw$year)
      for (cm in unique(inp_raw$comm)) {
        ia <- aggregate(value ~ year, inp_raw[inp_raw$comm == cm, ], sum, na.rm = TRUE)
        names(ia)[2] <- paste0("inp_", cm)
        cost_yearly <- merge(cost_yearly, ia, by = "year", all.x = TRUE)
      }
    }
    # Reorder: tech, group, region, year first
    cy_front <- c("tech", "group", "region", "year")
    cost_yearly <- cost_yearly[, c(cy_front, setdiff(names(cost_yearly), cy_front)),
                               drop = FALSE]
    rownames(cost_yearly) <- NULL

    # NPV LCOE cost-component breakdown: NPV(raw_component_cost) / npv_den
    # Uses the same denominator as lc_npv so components sum to lc_npv.
    cbd_npv <- NULL
    if (is.finite(lc_npv) && is.finite(npv_den) && npv_den > 0) {
      npv_parts <- Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
        d <- cmp_raw[[nm]]
        if (is.null(d) || nrow(d) == 0) return(NULL)
        d$te_c  <- as.integer(d$year) - as.integer(base_year)
        d$dsc_c <- (1 + discount)^d$te_c
        vld_c   <- is.finite(d$value) & is.finite(d$dsc_c) & d$dsc_c > 0
        pv      <- if (any(vld_c)) sum(d$value[vld_c] / d$dsc_c[vld_c]) else NA_real_
        data.frame(tech = tech_name, group = group_val, comm = comm_lbl,
                   component = nm, value = pv / npv_den,
                   stringsAsFactors = FALSE)
      }))
      if (length(npv_parts) > 0) {
        cbd_npv <- do.call(rbind, npv_parts)
        rownames(cbd_npv) <- NULL
      }
    }

    # Per-activity LCOE (grouped-output techs only).
    # When use_act_norm is TRUE, norm_yr already holds activity by year — reuse it.
    lc_act <- NULL
    if (has_grouped_output) {
      act_ag2 <- if (!is.null(norm_yr)) {
        data.frame(year = as.integer(names(norm_yr)), value = unname(norm_yr),
                   stringsAsFactors = FALSE)
      } else {
        agg_yr(sfget("vTechAct"))
      }
      if (!is.null(act_ag2) && nrow(act_ag2) > 0) {
        tc_yr      <- tc_df[, c("year", "value"), drop = FALSE]
        mrg <- merge(tc_yr, act_ag2, by = "year", suffixes = c("_cost", "_act"))
        mrg$lca <- ifelse(is.finite(mrg$value_act) & mrg$value_act != 0,
                          mrg$value_cost / mrg$value_act, NA_real_)
        lc_act <- data.frame(
          tech = tech_name, group = group_val, comm = comm_lbl,
          region = region, year = as.integer(mrg$year),
          activity = mrg$value_act, cost = mrg$value_cost,
          levcost_per_act = mrg$lca, stringsAsFactors = FALSE
        )
      }
    }

    # Per-activity NPV cost-component breakdown.
    # Uses npv_act_for_act as denominator (NPV of vTechAct) so the Activity bars
    # in the NPV chart can show the same component colours as per-comm bars.
    # For the base scenario, npv_act_for_act == npv_den, so this equals cbd_npv.
    cbd_npv_per_act <- NULL
    if (!is.null(npv_act_for_act) && is.finite(npv_act_for_act) && npv_act_for_act > 0) {
      if (isTRUE(all.equal(npv_act_for_act, npv_den, tolerance = 1e-9))) {
        # Same denominator (base scenario with activity normalisation): reuse cbd_npv.
        cbd_npv_per_act <- cbd_npv
      } else {
        # Frontier scenario: recompute with pure NPV(vTechAct) denominator.
        pa_parts <- Filter(Negate(is.null), lapply(names(cmp_raw), function(nm) {
          d <- cmp_raw[[nm]]
          if (is.null(d) || nrow(d) == 0) return(NULL)
          d$te_c  <- as.integer(d$year) - as.integer(base_year)
          d$dsc_c <- (1 + discount)^d$te_c
          vld_c   <- is.finite(d$value) & is.finite(d$dsc_c) & d$dsc_c > 0
          pv      <- if (any(vld_c)) sum(d$value[vld_c] / d$dsc_c[vld_c]) else NA_real_
          data.frame(tech = tech_name, group = group_val, comm = comm_lbl,
                     component = nm, value = pv / npv_act_for_act,
                     stringsAsFactors = FALSE)
        }))
        if (length(pa_parts) > 0) {
          cbd_npv_per_act <- do.call(rbind, pa_parts)
          rownames(cbd_npv_per_act) <- NULL
        }
      }
    }

    list(levcost = lc_tbl, levcost_npv = lc_npv,
         cost_breakdown = cbd, cost_breakdown_npv = cbd_npv,
         cost_breakdown_npv_per_act = cbd_npv_per_act,
         cost_yearly = cost_yearly,
         levcost_per_act = lc_act, scen = sc)
  }

  # ── 12. Base solve ───────────────────────────────────────────────────────────
  # Build demands for ALL group outputs so the model is well-defined, but pass
  # only the out_comms subset to levcost_extract_() for LCOE normalisation.
  energyRt::set_default_solver(solver)
  base_model_dvals <- setNames(rep(1, length(all_out_comms)), all_out_comms)
  base_lcoe_dvals  <- base_model_dvals[out_comms]
  demand_objects   <- make_demands_(base_model_dvals)
  if (verbose) for (cm in all_out_comms) message("Created unit demand for '", cm, "'.")
  scen <- build_and_solve_(demand_objects)
  if (!isTRUE(scen@status$optimal)) {
    warning("Mini model for '", tech_name, "' did not solve to optimality. ",
            "LCOE results may be unreliable.")
  }
  base_res           <- levcost_extract_(scen, base_lcoe_dvals)
  levcost_tbl        <- base_res$levcost
  levcost_npv        <- base_res$levcost_npv
  cost_breakdown     <- base_res$cost_breakdown
  cost_breakdown_npv <- base_res$cost_breakdown_npv
  cost_yearly        <- base_res$cost_yearly
  levcost_per_act    <- base_res$levcost_per_act

  # ── 12.5. Frontier solves: one per output commodity ──────────────────────────
  # For output comm_i with share_up[i] = s_i:
  #   demand[i] = s_i  → binds the upper-share constraint for i (maximises its fraction)
  #   demand[j] = (1 - s_i) / (N-1)  for j ≠ i
  # The two (or N) resulting production points define the production frontier.
  frontier_raw <- list()
  if (do_frontier) {
    for (prim in out_comms) {
      si      <- if (is.finite(share_up[[prim]])) share_up[[prim]] else 1 / length(out_comms)
      n_other <- length(out_comms) - 1
      dv_oth  <- if (n_other > 0) (1 - si) / n_other else 0
      dem_fr  <- setNames(rep(max(dv_oth, 1e-6), length(out_comms)), out_comms)
      dem_fr[[prim]] <- si
      dobj_fr  <- make_demands_(dem_fr)
      scen_fr  <- build_and_solve_(dobj_fr, suffix = paste0("_fr_", prim))
      if (!isTRUE(scen_fr@status$optimal))
        warning("Frontier scenario max_", prim, " for '", tech_name,
                "' did not solve to optimality.")
      fr_res <- levcost_extract_(scen_fr, dem_fr, primary_comm = prim)
      frontier_raw[[paste0("max_", prim)]] <- c(fr_res,
        list(primary_comm = prim, primary_input = NA_character_,
             dem_vals = dem_fr))
    }
  }

  # ── 12.7. Input frontier solves: vary input mix for each output corner ──────
  # For each output corner × each input commodity in a multi-commodity input
  # group, fix the input share via share.fx to force a specific input mix.
  if (do_frontier && length(in_groups) > 0) {
    inp_frontier_groups <- Filter(
      function(g) length(in_group_comms[[g]]) >= 2, in_groups)
    if (length(inp_frontier_groups) > 0) {
      original_ceff <- object@ceff
      for (prim_out in out_comms) {
        # Same output-corner demands as the output frontier
        si_out  <- if (is.finite(share_up[[prim_out]])) share_up[[prim_out]] else
          1 / length(out_comms)
        n_oth   <- length(out_comms) - 1
        dv_oth  <- if (n_oth > 0) (1 - si_out) / n_oth else 0
        dem_ifr <- setNames(rep(max(dv_oth, 1e-6), length(out_comms)), out_comms)
        dem_ifr[[prim_out]] <- si_out
        dobj_ifr <- make_demands_(dem_ifr)

        for (grp in inp_frontier_groups) {
          grp_comms <- in_group_comms[[grp]]
          # Skip commodities that already have share.fx in the original ceff
          already_fixed <- character(0)
          if ("share.fx" %in% names(original_ceff)) {
            for (cm in grp_comms) {
              rf <- original_ceff[original_ceff$comm == cm &
                !is.na(original_ceff$share.fx), , drop = FALSE]
              if (nrow(rf) > 0) already_fixed <- c(already_fixed, cm)
            }
          }
          vary_comms <- setdiff(grp_comms, already_fixed)
          if (length(vary_comms) < 2) next

          for (prim_inp in vary_comms) {
            mod_ceff <- original_ceff
            inp_su <- in_share_up[[grp]][[prim_inp]]
            fx_val <- if (is.finite(inp_su)) inp_su else 1.0
            other_comms <- setdiff(grp_comms, prim_inp)
            other_fx <- if (length(other_comms) > 0)
              (1 - fx_val) / length(other_comms) else 0
            if (!"share.fx" %in% names(mod_ceff))
              mod_ceff$share.fx <- NA_real_
            for (cm in grp_comms) {
              rows <- which(mod_ceff$comm == cm)
              if (length(rows) > 0)
                mod_ceff$share.fx[rows] <-
                  if (cm == prim_inp) fx_val else other_fx
            }
            object <- update(object, ceff = mod_ceff)
            suffix  <- paste0("_fr_", prim_out, "_", prim_inp)
            scen_ifr <- build_and_solve_(dobj_ifr, suffix = suffix)
            if (!isTRUE(scen_ifr@status$optimal))
              warning("Input frontier scenario max_", prim_out, "_max_",
                      prim_inp, " for '", tech_name,
                      "' did not solve to optimality.")
            ifr_res <- levcost_extract_(scen_ifr, dem_ifr,
                                        primary_comm = prim_out)
            frontier_raw[[paste0("max_", prim_out, "_max_", prim_inp)]] <-
              c(ifr_res, list(primary_comm = prim_out,
                             primary_input = prim_inp,
                             dem_vals = dem_ifr))
            if (verbose)
              message("Input frontier: max_", prim_out, "_max_", prim_inp)
          }
        }
      }
      object <- update(object, ceff = original_ceff)
    }
  }

  # ── 13. Frontier result assembly ─────────────────────────────────────────────
  frontier           <- NULL
  levcost_by_comm    <- NULL
  levcost_by_act     <- NULL
  levcost_by_act_cbd <- NULL

  if (length(frontier_raw) > 0) {
    # levcost_by_comm: per-comm NPV cost breakdown (used in "npv" autoplot)
    bc_rows <- lapply(names(frontier_raw), function(sc_nm) {
      fr     <- frontier_raw[[sc_nm]]
      npv_df <- fr$cost_breakdown_npv
      if (is.null(npv_df)) return(NULL)
      npv_df$scenario      <- sc_nm
      npv_df$comm          <- fr$primary_comm
      npv_df$primary_input <- if (!is.null(fr$primary_input)) fr$primary_input else NA_character_
      npv_df
    })
    levcost_by_comm <- do.call(rbind, Filter(Negate(is.null), bc_rows))
    if (!is.null(levcost_by_comm)) rownames(levcost_by_comm) <- NULL
    # levcost_by_act: per-scenario NPV activity LCOE  (activity cost per unit of
    # total activity in each frontier scenario, using the actual discount rate)
    act_rows <- lapply(names(frontier_raw), function(sc_nm) {
      fr     <- frontier_raw[[sc_nm]]
      lca_df <- fr$levcost_per_act
      if (is.null(lca_df) || nrow(lca_df) == 0) return(NULL)
      lca_df$te  <- lca_df$year - as.integer(base_year)
      lca_df$dsc <- (1 + discount)^lca_df$te
      vld_cost <- is.finite(lca_df$cost)     & is.finite(lca_df$dsc) & lca_df$dsc > 0
      vld_act  <- is.finite(lca_df$activity) & is.finite(lca_df$dsc) & lca_df$dsc > 0
      npv_cost <- if (any(vld_cost)) sum(lca_df$cost[vld_cost]     / lca_df$dsc[vld_cost]) else NA_real_
      npv_act  <- if (any(vld_act))  sum(lca_df$activity[vld_act]  / lca_df$dsc[vld_act])  else NA_real_
      npv_lca  <- if (is.finite(npv_act) && npv_act > 0) npv_cost / npv_act else NA_real_
      data.frame(
        tech = lca_df$tech[1], group = lca_df$group[1],
        scenario = sc_nm, primary_comm = fr$primary_comm,
        primary_input = if (!is.null(fr$primary_input)) fr$primary_input else NA_character_,
        npv_act = npv_lca,
        stringsAsFactors = FALSE
      )
    })
    levcost_by_act <- do.call(rbind, Filter(Negate(is.null), act_rows))
    if (!is.null(levcost_by_act)) rownames(levcost_by_act) <- NULL

    # levcost_by_act_cbd: per-activity NPV cost-component breakdown, one row per
    # frontier scenario × component.  Parallels levcost_by_comm but normalised by
    # NPV(vTechAct) so Activity bars in the NPV chart show fuel-cost components.
    act_cbd_rows <- lapply(names(frontier_raw), function(sc_nm) {
      fr     <- frontier_raw[[sc_nm]]
      cbd_pa <- fr$cost_breakdown_npv_per_act
      if (is.null(cbd_pa) || nrow(cbd_pa) == 0) return(NULL)
      cbd_pa$scenario      <- sc_nm
      cbd_pa$primary_comm  <- fr$primary_comm
      cbd_pa$primary_input <- if (!is.null(fr$primary_input)) fr$primary_input else NA_character_
      cbd_pa
    })
    levcost_by_act_cbd <- do.call(rbind, Filter(Negate(is.null), act_cbd_rows))
    if (!is.null(levcost_by_act_cbd)) rownames(levcost_by_act_cbd) <- NULL
    if (length(out_comms) == 2) {
      c1 <- out_comms[1]; c2 <- out_comms[2]
      sfget_fr <- function(sc, v) tryCatch({
        d <- energyRt::getData(sc, name = v, merge = TRUE, drop.zeros = FALSE)
        if (is.null(d) || nrow(d) == 0) return(NULL); as.data.frame(d)
      }, error = function(e) NULL)

      # Only output-only frontier entries for the 2D production frontier plot
      out_only_names <- names(frontier_raw)[vapply(frontier_raw, function(fr)
        is.null(fr$primary_input) || is.na(fr$primary_input), logical(1))]
      fr_pts <- do.call(rbind, lapply(out_only_names, function(sc_nm) {
        fr   <- frontier_raw[[sc_nm]]
        sc   <- fr$scen
        tout <- sfget_fr(sc, "vTechOut")
        if (is.null(tout)) return(NULL)
        tout$year <- as.integer(tout$year)
        agg   <- aggregate(value ~ comm + year, tout, sum, na.rm = TRUE)
        tc_f  <- sfget_fr(sc, "vTotalCost")
        if (is.null(tc_f)) return(NULL)
        tc_f$year <- as.integer(tc_f$year)
        do.call(rbind, lapply(unique(agg$year), function(yr) {
          p1 <- agg$value[agg$comm == c1 & agg$year == yr]
          p2 <- agg$value[agg$comm == c2 & agg$year == yr]
          tc <- tc_f$value[tc_f$year == yr]
          data.frame(
            scenario   = sc_nm, year = yr,
            prod_comm1 = if (length(p1) > 0) p1[1] else NA_real_,
            prod_comm2 = if (length(p2) > 0) p2[1] else NA_real_,
            total_cost = if (length(tc) > 0) tc[1] else NA_real_,
            stringsAsFactors = FALSE
          )
        }))
      }))
      if (!is.null(fr_pts) && nrow(fr_pts) > 0) {
        fr_pts$tech           <- tech_name
        fr_pts$comm1          <- c1
        fr_pts$comm2          <- c2
        fr_pts$share_up_comm1 <- share_up[[c1]]
        fr_pts$share_up_comm2 <- share_up[[c2]]
        frontier <- fr_pts; rownames(frontier) <- NULL
      }
    }
  }

  # ── 13.5. Share constraint segments (geometric – no extra solves) ────────────
  # Delegate to standalone tech_share_frontier() which already handles both
  # input and output groups with effective-range computation.
  input_frontier <- tech_share_frontier(object)

  # ── 13.9. Technology units for axis labelling ────────────────────────────────
  costs_unit    <- if (nrow(object@units) > 0 && "costs"    %in% names(object@units)) {
    v <- object@units$costs[1]; if (!is.na(v) && nzchar(v)) v else ""
  } else ""
  activity_unit <- if (nrow(object@units) > 0 && "activity" %in% names(object@units)) {
    v <- object@units$activity[1]; if (!is.na(v) && nzchar(v)) v else ""
  } else ""
  output_units  <- setNames(
    vapply(out_comms, function(cm) {
      if (nrow(object@output) > 0 && "unit" %in% names(object@output)) {
        r <- object@output$unit[object@output$comm == cm]
        if (length(r) > 0 && !is.na(r[1]) && nzchar(r[1])) return(r[1])
      }
      ""
    }, character(1)),
    out_comms
  )
  tech_units <- list(costs = costs_unit, activity = activity_unit,
                     output = output_units)

  # ── 14. Return ───────────────────────────────────────────────────────────────
  if (full_output) {
    scen@misc$levcost            <- levcost_tbl
    scen@misc$levcost_npv        <- levcost_npv
    scen@misc$cost_breakdown     <- cost_breakdown
    scen@misc$cost_breakdown_npv <- cost_breakdown_npv
    scen@misc$cost_yearly        <- cost_yearly
    scen@misc$levcost_per_act    <- levcost_per_act
    scen@misc$frontier           <- frontier
    scen@misc$levcost_by_comm    <- levcost_by_comm
    scen@misc$levcost_by_act     <- levcost_by_act
    scen@misc$levcost_by_act_cbd <- levcost_by_act_cbd
    scen@misc$input_frontier     <- input_frontier
    return(scen)
  }

  result <- list(
    levcost            = levcost_tbl,
    levcost_npv        = levcost_npv,
    cost_breakdown     = cost_breakdown,
    cost_breakdown_npv = cost_breakdown_npv,
    levcost_per_act    = levcost_per_act,
    cost_yearly        = cost_yearly,
    frontier           = frontier,
    levcost_by_comm    = levcost_by_comm,
    levcost_by_act     = levcost_by_act,
    levcost_by_act_cbd = levcost_by_act_cbd,
    input_frontier     = input_frontier,
    discount           = discount,
    base_year          = as.integer(base_year),
    units              = tech_units,
    scenario           = scen
  )
  class(result) <- c("ideea_levcost", "list")
  result
}

# ── plot_share_frontier ─────────────────────────────────────────────────────────
# Build the diagonal share-mix chart from a tech_share_frontier() data frame.
# Returns a named list of ggplot2 objects (class "share_frontier_plots"), one
# per (direction x group).  For 2-comm groups the plot has proper x/y axis
# labels (the two commodity names).  For 3+ comm groups a facet_wrap is used
# with x = "Share of <comm>", y = "Share of rest".
#
# @param df      data.frame from tech_share_frontier()
# @param title   Optional overall title string prepended to each plot title
# @return list of class "share_frontier_plots" or NULL when df is empty/NULL
#' @export
plot_share_frontier <- function(df, title = NULL, base_size = 11L) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plot_share_frontier().")

  # Fall back to direct constraints if effective range not available
  if (!"share_lo_eff" %in% names(df)) df$share_lo_eff <- df$share_lo
  if (!"share_hi_eff" %in% names(df)) df$share_hi_eff <- df$share_hi
  if (!"n_in_group" %in% names(df))   df$n_in_group   <- NA_integer_

  dir_cols <- c(input = "#E84B35", output = "#4E79A7")
  dir_fill <- c(input = "#E84B3530", output = "#4E79A730")  # unused but kept for ref

  # ── shared geom layers as a helper ─────────────────────────────────────────
  .diagonal_layers <- function(col, lo_name = "share_lo_eff", hi_name = "share_hi_eff") {
    list(
      ggplot2::annotate("segment",
        x = 0, xend = 1, y = 1, yend = 0,
        colour = "grey80", linewidth = 0.5, linetype = "solid"),
      ggplot2::geom_rect(
        ggplot2::aes(xmin = share_lo_eff, xmax = share_hi_eff,
                     ymin = 0, ymax = 1),
        fill = col, alpha = 0.12),
      ggplot2::geom_segment(
        ggplot2::aes(x    = share_lo_eff, xend = share_hi_eff,
                     y    = 1 - share_lo_eff, yend = 1 - share_hi_eff),
        colour = col, linewidth = 2.2),
      ggplot2::geom_point(
        ggplot2::aes(x = share_lo_eff, y = 1 - share_lo_eff),
        colour = col, size = 2.5, shape = 16),
      ggplot2::geom_point(
        ggplot2::aes(x = share_hi_eff, y = 1 - share_hi_eff),
        colour = col, size = 2.5, shape = 16),
      ggplot2::geom_text(
        ggplot2::aes(x = share_lo_eff, y = 1 - share_lo_eff,
                     label = paste0(round(share_lo_eff * 100), "%")),
        hjust = 1.25, vjust = 0.5, size = 2.5, colour = col),
      ggplot2::geom_text(
        ggplot2::aes(x = share_hi_eff, y = 1 - share_hi_eff,
                     label = paste0(round(share_hi_eff * 100), "%")),
        hjust = -0.25, vjust = 0.5, size = 2.5, colour = col)
    )
  }

  .base_scales <- function(x_name, y_name) {
    list(
      ggplot2::coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1),
                           expand = TRUE, clip = "off"),
      ggplot2::scale_x_continuous(
        name   = x_name,
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = function(x) paste0(round(x * 100), "%")),
      ggplot2::scale_y_continuous(
        name   = y_name,
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = function(x) paste0(round(x * 100), "%")),
      ggplot2::theme_bw(base_size = base_size),
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        strip.text       = ggplot2::element_text(size = 7),
        axis.title       = ggplot2::element_text(size = 8, face = "bold"),
        plot.title       = ggplot2::element_text(size = 8, colour = "grey30",
                                                  hjust = 0.5, face = "bold"),
        plot.margin      = ggplot2::margin(4, 8, 4, 4))
    )
  }

  # ── one ggplot per (direction x group) ────────────────────────────────────
  all_plots <- list()
  n_panels  <- 0L

  for (dir in c("input", "output")) {
    sub_dir <- df[df$direction == dir, , drop = FALSE]
    if (nrow(sub_dir) == 0L) next
    col <- dir_cols[[dir]]

    for (grp in unique(sub_dir$group)) {
      sub <- sub_dir[sub_dir$group == grp, , drop = FALSE]
      n_g <- if (!is.na(sub$n_in_group[1])) sub$n_in_group[1] else nrow(sub)
      plot_title <- if (!is.null(title))
        paste0(title, "  |  ", dir, ": ", grp)
      else
        paste0(dir, ": ", grp)

      if (n_g == 2L) {
        # ── 2-comm: single panel, comms as axis labels ───────────────────
        # keep only the first row (the two rows are mirrors of each other)
        row1 <- sub[1L, , drop = FALSE]
        p <- ggplot2::ggplot(row1) +
          .diagonal_layers(col) +
          .base_scales(x_name = row1$comm[1], y_name = row1$others[1]) +
          ggplot2::labs(title = plot_title)

      } else {
        # ── 3+ comm: one facet per commodity ────────────────────────────
        sub$facet_lab <- paste0(sub$comm, "\nvs. ", sub$others)
        p <- ggplot2::ggplot(sub) +
          .diagonal_layers(col) +
          ggplot2::facet_wrap(~ facet_lab) +
          .base_scales(x_name = "Share of commodity",
                       y_name = "Share of rest") +
          ggplot2::labs(title = plot_title)
      }

      key <- paste(dir, grp, sep = ":::")
      all_plots[[key]] <- p
      n_panels <- n_panels + 1L
    }
  }

  if (length(all_plots) == 0L) return(NULL)

  n_per_row <- min(n_panels, 4L)
  attr(all_plots, "n_per_row") <- n_per_row
  attr(all_plots, "n_panels")  <- n_panels
  class(all_plots) <- c("share_frontier_plots", "list")
  all_plots
}

#' @export
print.share_frontier_plots <- function(x, ...) {
  if (requireNamespace("patchwork", quietly = TRUE)) {
    n_per_row <- attr(x, "n_per_row")
    if (is.null(n_per_row)) n_per_row <- min(length(x), 4L)
    p <- patchwork::wrap_plots(x, ncol = n_per_row)
    print(p)
  } else {
    for (p in x) print(p)
  }
  invisible(x)
}

# ── autoplot methods ────────────────────────────────────────────────────────────

# Define a local autoplot generic so the function is available even when ggplot2
# is not attached (i.e. library(ggplot2) not called).  When ggplot2 IS attached,
# its own generic takes precedence in the search path; UseMethod() dispatches to
# autoplot.ideea_levcost / autoplot.ideea_levcost_list defined below.
if (!exists("autoplot", mode = "function", inherits = TRUE)) {
  autoplot <- function(object, ...) {
    if (!requireNamespace("ggplot2", quietly = TRUE))
      stop("Package 'ggplot2' is required for autoplot.")
    UseMethod("autoplot")
  }
}

# Canonical component ordering and display labels (shared by all methods).
.levcost_comp_order  <- c("eac", "fixom", "varom",
                           "supply", "import", "export")
.levcost_comp_labels <- c(eac           = "EAC (Annualised Inv.)",
                           fixom         = "Fixed O&M",
                           varom         = "Variable O&M",
                           supply        = "Supply / Fuel",
                           import        = "Import",
                           export        = "Export (credit)")

#' Plot levelized cost results
#'
#' \code{autoplot} methods for objects returned by \code{\link{ideea_levcost}}.
#'
#' @param object  An \code{ideea_levcost} or \code{ideea_levcost_list} object.
#' @param type    One of:
#'   \code{"components"} (default) – per-year stacked bar chart with a dashed NPV
#'   line;  \code{"npv"} – NPV-weighted cost columns for each output metric
#'   (activity, comm1, …, commN when frontier data are present, or cost components
#'   when not);  \code{"totals"} – simple per-year total bar chart;
#'   \code{"frontier"} – 2D production-frontier plot (2-commodity grouped outputs
#'   only).
#' @param year    Integer or \code{NULL}.  Year to display in the
#'   \code{"frontier"} plot.  \code{NULL} (default) uses the first milestone year.
#' @param cost_unit Character or \code{NULL}.  Override the y-axis cost unit
#'   label (e.g. \code{"USD/(Vh*km)"}).  \code{NULL} (default) uses the unit
#'   stored in \code{object$units$costs}.
#' @param ...     Currently unused.
#'
#' @return A \code{ggplot2} plot object.
#' @export
autoplot.ideea_levcost <- function(object,
                                   type = c("components", "npv", "totals",
                                            "frontier", "input_frontier"),
                                   year = NULL,
                                   cost_unit = NULL,
                                   cost_unit_comm = NULL,
                                   ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for autoplot. ",
         "Install it with install.packages('ggplot2').")

  type  <- match.arg(type)
  npv   <- as.numeric(object$levcost_npv)
  title <- paste0("Levelized Cost: ", names(object$levcost_npv))

  # Resolve display cost unit label for LCOE axes
  # Helper: parse "USD/(Vh*km)" → list(currency = "USD", denom = "Vh*km")
  .parse_cu <- function(cu) {
    m <- regmatches(cu, regexec("^(.+)/\\((.+)\\)$", cu))[[1]]
    if (length(m) == 3) list(currency = m[2], denom = m[3])
    else list(currency = cu, denom = "")
  }
  # Build y-axis label: "USD per Vh*km" or "USD per Vh*km or Passenger*km"
  .cost_axis_label <- function(cu, cu_comm = NULL) {
    p <- .parse_cu(cu)
    lbl <- if (nzchar(p$denom)) paste(p$currency, "per", p$denom) else p$currency
    if (!is.null(cu_comm) && nzchar(cu_comm)) {
      pc <- .parse_cu(cu_comm)
      if (nzchar(pc$denom)) lbl <- paste0(lbl, " or ", pc$denom)
    }
    lbl
  }

  if (!is.null(cost_unit) && nzchar(cost_unit)) {
    # Explicit override — build human-readable label
    costs_lbl <- cost_unit
    y_lbl_act  <- .cost_axis_label(cost_unit)
    y_lbl_both <- .cost_axis_label(cost_unit, cost_unit_comm)
  } else {
    # Auto-compose from technology units: costs / activity
    cu <- if (!is.null(object$units$costs) && nzchar(object$units$costs))
      object$units$costs else ""
    au <- if (!is.null(object$units$activity) && nzchar(object$units$activity))
      object$units$activity else ""
    costs_lbl <- if (nzchar(cu) && nzchar(au)) {
      paste0(cu, " / ", au)
    } else if (nzchar(cu)) {
      cu
    } else ""
    y_lbl_act  <- if (nzchar(costs_lbl)) paste0("LCOE [", costs_lbl, "]") else "Levelized Cost"
    y_lbl_both <- y_lbl_act
  }

  # ── "frontier" type: 2D production frontier ─────────────────────────────────
  if (type == "frontier") {
    df <- object$frontier
    if (is.null(df) || nrow(df) == 0) {
      message("No frontier data available. Run ideea_levcost() with frontier = TRUE ",
              "and a grouped technology having >= 2 output commodities with share.up.")
      return(invisible(NULL))
    }
    # Select representative year
    yr_sel <- if (!is.null(year)) as.integer(year) else min(df$year, na.rm = TRUE)
    df_yr  <- df[df$year == yr_sel, , drop = FALSE]
    c1_lbl <- df_yr$comm1[1]; c2_lbl <- df_yr$comm2[1]
    su1    <- df_yr$share_up_comm1[1]; su2 <- df_yr$share_up_comm2[1]

    # Build scenario display labels
    df_yr$label <- ifelse(
      df_yr$scenario == paste0("max_", c1_lbl),
      paste0("max ", c1_lbl, " (share \u2264 ", round(su1 * 100), "%)"),
      paste0("max ", c2_lbl, " (share \u2264 ", round(su2 * 100), "%)"))

    p <- ggplot2::ggplot(df_yr,
           ggplot2::aes(x = prod_comm1, y = prod_comm2, colour = label)) +
      ggplot2::geom_line(colour = "grey50", linewidth = 0.8,
                         data = df_yr[order(df_yr$prod_comm1), ]) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_text(
        ggplot2::aes(label = {
          sc_nm <- df_yr$scenario
          lca_df <- object$levcost_by_act
          if (!is.null(lca_df)) {
            # Base LCOE for this output corner
            lca_v <- lca_df$npv_act[match(sc_nm, lca_df$scenario)]
            # Check input frontier scenarios for min/max range
            sapply(seq_along(sc_nm), function(i) {
              base_val <- lca_v[i]
              if (!is.finite(base_val)) return("")
              prim_comm <- sub("^max_", "", sc_nm[i])
              inp_rows  <- lca_df[!is.na(lca_df$primary_input) &
                                  lca_df$primary_comm == prim_comm, , drop = FALSE]
              if (nrow(inp_rows) > 0) {
                all_vals <- c(base_val, inp_rows$npv_act[is.finite(inp_rows$npv_act)])
                lo <- min(all_vals, na.rm = TRUE)
                hi <- max(all_vals, na.rm = TRUE)
                if (abs(hi - lo) > 1e-6)
                  paste0("LCOE = ", round(lo, 3), "\u2013", round(hi, 3))
                else
                  paste0("LCOE = ", round(base_val, 3))
              } else {
                paste0("LCOE = ", round(base_val, 3))
              }
            })
          } else {
            paste0("cost = ", round(df_yr$total_cost, 2))
          }
        }),
        vjust = -1, size = 3, show.legend = FALSE) +
      ggplot2::scale_colour_brewer(palette = "Set1") +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(
        title    = paste0("Production Frontier: ", names(object$levcost_npv)),
        subtitle = paste0("Year ", yr_sel),
        x        = paste0("Production share of ", c1_lbl),
        y        = paste0("Production share of ", c2_lbl),
        colour   = "Operating extreme"
      ) +
      ggplot2::theme_bw()
    return(p)
  }

  # ── "input_frontier" type: feasible share ranges (input + output groups) ─────
  if (type == "input_frontier") {
    df <- object$input_frontier
    p  <- plot_share_frontier(df, title = paste0("Share Mix: ", names(object$levcost_npv)))
    if (is.null(p)) {
      message("No share constraint data available. The technology may have no ",
              "grouped inputs or outputs with share.up / share.lo constraints.")
      return(invisible(NULL))
    }
    return(p)
  }

  # ── "npv" type ───────────────────────────────────────────────────────────────
  if (type == "npv") {
    has_frontier_data <- !is.null(object$levcost_by_comm) &&
                         nrow(object$levcost_by_comm) > 0

    if (has_frontier_data) {
      # ── NPV columns per output metric: activity + each max-comm scenario ────
      # Collect NPV LCOE scalar for each metric
      npv_rows <- list()

      # 1. Per-scenario activity LCOE (activity cost in each frontier scenario)
      if (!is.null(object$levcost_by_act) && nrow(object$levcost_by_act) > 0) {
        has_act_cbd <- !is.null(object$levcost_by_act_cbd) &&
                       nrow(object$levcost_by_act_cbd) > 0
        for (i in seq_len(nrow(object$levcost_by_act))) {
          row_i <- object$levcost_by_act[i, ]
          prim  <- row_i$primary_comm
          sc_nm <- row_i$scenario
          prim_inp <- if ("primary_input" %in% names(row_i) &&
            !is.na(row_i$primary_input)) row_i$primary_input else NA_character_
          lbl   <- if (!is.na(prim_inp))
            paste0("Activity\n(max ", prim, " + ", prim_inp, ")")
          else
            paste0("Activity\n(max ", prim, ")")
          if (has_act_cbd) {
            cbd_i <- object$levcost_by_act_cbd[
              object$levcost_by_act_cbd$scenario == sc_nm, , drop = FALSE]
            if (nrow(cbd_i) > 0) {
              cbd_i$comm  <- "Activity"
              cbd_i$label <- lbl
              npv_rows[[paste0("act_sc_", i)]] <- cbd_i[,
                c("tech", "group", "comm", "label", "component", "value"),
                drop = FALSE]
              next
            }
          }
          # Fallback: single "total" bar when component breakdown unavailable.
          npv_rows[[paste0("act_sc_", i)]] <- data.frame(
            tech = row_i$tech, group = row_i$group,
            comm = "Activity", label = lbl,
            component = "total", value = row_i$npv_act,
            stringsAsFactors = FALSE
          )
        }
      }

      # 3. Per-comm scenarios from frontier
      fr_df <- object$levcost_by_comm
      for (sc_nm in unique(fr_df$scenario)) {
        sub <- fr_df[fr_df$scenario == sc_nm, , drop = FALSE]
        prim_inp_c <- if ("primary_input" %in% names(sub) &&
          !is.na(sub$primary_input[1])) sub$primary_input[1] else NA_character_
        sub$scenario     <- NULL
        sub$primary_input <- NULL
        if (!is.na(prim_inp_c)) {
          sub$label <- paste0(sub$comm, "\n(max ", sub$comm,
                             " + ", prim_inp_c, ")")
        } else {
          sub$label <- paste0(sub$comm, "\n(max ", sub$comm, ")")
        }
        npv_rows[[sc_nm]] <- sub
      }

      all_df <- do.call(rbind, Filter(Negate(is.null), npv_rows))
      if (is.null(all_df) || nrow(all_df) == 0) {
        message("No NPV data assembled; falling back to 'totals'.")
        type <- "totals"
      } else {
        present <- intersect(.levcost_comp_order, unique(all_df$component))
        if (length(present) == 0) present <- unique(all_df$component)   # e.g., "total"
        all_df$component <- factor(all_df$component,
                                   levels = c(present, setdiff(unique(all_df$component), present)))
        # Label ordering: all activity bars first, then per-comm
        lbl_order    <- unique(all_df$label)
        act_lbl      <- lbl_order[grepl("^Activity", lbl_order)]
        other_lbl    <- setdiff(lbl_order, act_lbl)
        all_df$label <- factor(all_df$label, levels = c(act_lbl, other_lbl))

        y_lbl <- y_lbl_both
        p <- ggplot2::ggplot(all_df,
               ggplot2::aes(x = label, y = value, fill = component)) +
          ggplot2::geom_col(position = "stack") +
          ggplot2::geom_hline(yintercept = npv, linetype = "dashed",
                              colour = "black", linewidth = 0.7) +
          ggplot2::scale_fill_brewer(
            palette = "Set2",
            labels  = c(.levcost_comp_labels, total = "Total")) +
          ggplot2::labs(
            title   = paste0("Levelized Costs by Output Metric: ", names(object$levcost_npv)),
            x       = "Output metric",
            y       = y_lbl,
            fill    = "Component",
            caption = paste0("Dashed line: combined LCOE = ", round(npv, 3))
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 45, hjust = 1, size = 8))
        return(p)
      }
    } else {
      # ── Fall back: component breakdown of combined LCOE ──────────────────────
      df <- object$cost_breakdown_npv
      if (is.null(df) || nrow(df) == 0) {
        message("No cost_breakdown_npv available; falling back to 'totals'.")
        type <- "totals"
      } else {
        present      <- intersect(.levcost_comp_order, unique(df$component))
        df$component <- factor(df$component, levels = present)
        df$label     <- .levcost_comp_labels[as.character(df$component)]
        df$label[is.na(df$label)] <- as.character(df$component[is.na(df$label)])

        p <- ggplot2::ggplot(df, ggplot2::aes(x = label, y = value,
                                               fill = component)) +
          ggplot2::geom_col() +
          ggplot2::scale_fill_brewer(palette = "Set2", guide = "none") +
          ggplot2::labs(
            title   = paste0("Cost Breakdown: ", title),
            x       = "Component",
            y       = y_lbl_act,
            caption = paste0("LCOE = ", round(npv, 3))
          ) +
          ggplot2::theme_bw() +
          ggplot2::coord_flip()
        return(p)
      }
    }
  }

  if (type == "components") {
    # ── Per-year stacked bar + NPV dashed line ───────────────────────────────
    df <- object$cost_breakdown
    if (is.null(df) || nrow(df) == 0) {
      message("No cost_breakdown available; falling back to 'totals'.")
      type <- "totals"
    } else {
      present      <- intersect(.levcost_comp_order, unique(df$component))
      df$component <- factor(df$component, levels = present)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = value,
                                             fill = component)) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::geom_hline(yintercept = npv, linetype = "dashed",
                            colour = "black", linewidth = 0.8) +
        ggplot2::scale_fill_brewer(palette = "Set2",
                                   labels = .levcost_comp_labels) +
        ggplot2::labs(
          title   = title,
          x       = "Year",
          y       = y_lbl_act,
          fill    = "Component",
          caption = paste0("Dashed line: LCOE = ", round(npv, 3))
        ) +
        ggplot2::theme_bw()
      return(p)
    }
  }

  # ── type == "totals" (or fallback) ───────────────────────────────────────────
  df <- object$levcost
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = levcost)) +
    ggplot2::geom_col(fill = "#4E79A7") +
    ggplot2::geom_hline(yintercept = npv, linetype = "dashed",
                        colour = "black", linewidth = 0.8) +
    ggplot2::labs(
      title   = title,
      x       = "Year",
      y       = y_lbl_act,
      caption = paste0("Dashed line: LCOE = ", round(npv, 3))
    ) +
    ggplot2::theme_bw()
  p
}

#' @rdname autoplot.ideea_levcost
#' @export
autoplot.ideea_levcost_list <- function(object,
                                        type = c("components", "npv",
                                                 "totals", "frontier"),
                                        year = NULL,
                                        ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for autoplot.")

  type <- match.arg(type)

  # ── "frontier": one panel per technology ────────────────────────────────────
  if (type == "frontier") {
    # Collect frontier data from each tech result, add tech column
    fr_list <- lapply(names(object), function(nm) {
      df <- object[[nm]]$frontier
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$tech_label <- nm
      df
    })
    fr_all <- do.call(rbind, Filter(Negate(is.null), fr_list))
    if (is.null(fr_all) || nrow(fr_all) == 0) {
      message("No frontier data in any list element.")
      return(invisible(NULL))
    }
    yr_sel <- if (!is.null(year)) as.integer(year) else min(fr_all$year, na.rm = TRUE)
    df_yr  <- fr_all[fr_all$year == yr_sel, , drop = FALSE]

    p <- ggplot2::ggplot(df_yr,
           ggplot2::aes(x = prod_comm1, y = prod_comm2,
                        colour = scenario, group = tech_label)) +
      ggplot2::geom_line(colour = "grey60", linewidth = 0.7,
                         ggplot2::aes(group = tech_label)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~ tech_label, scales = "free") +
      ggplot2::scale_colour_brewer(palette = "Set1") +
      ggplot2::labs(
        title  = paste0("Production Frontiers (year ", yr_sel, ")"),
        colour = "Operating extreme"
      ) +
      ggplot2::theme_bw()
    return(p)
  }

  # Collect NPV values for summary plots
  npv_vals <- sapply(object, function(x) as.numeric(x$levcost_npv))
  npv_df   <- data.frame(
    tech = names(npv_vals),
    npv  = as.numeric(npv_vals),
    stringsAsFactors = FALSE
  )

  if (type == "npv") {
    # ── Technology NPV comparison bar chart ─────────────────────────────────
    p <- ggplot2::ggplot(npv_df,
                         ggplot2::aes(x = stats::reorder(tech, npv),
                                      y = npv, fill = tech)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::labs(
        title = "NPV-weighted Levelized Cost Comparison",
        x     = "Technology",
        y     = "NPV LCOE"
      ) +
      ggplot2::theme_bw() +
      ggplot2::coord_flip()
    return(p)
  }

  if (type == "components") {
    # ── Per-year stacked bars, faceted by technology ─────────────────────────
    dfs <- lapply(object, function(x) x$cost_breakdown)
    dfs <- Filter(Negate(is.null), dfs)
    if (length(dfs) == 0) stop("No cost_breakdown available in list results.")

    df_all           <- do.call(rbind, dfs)
    present          <- intersect(.levcost_comp_order, unique(df_all$component))
    df_all$component <- factor(df_all$component, levels = present)

    p <- ggplot2::ggplot(df_all, ggplot2::aes(x = year, y = value,
                                               fill = component)) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::geom_hline(data = npv_df,
                          ggplot2::aes(yintercept = npv),
                          linetype = "dashed", colour = "black",
                          linewidth = 0.8, inherit.aes = FALSE) +
      ggplot2::facet_wrap(~ tech, scales = "free_y") +
      ggplot2::scale_fill_brewer(palette = "Set2",
                                 labels = .levcost_comp_labels) +
      ggplot2::labs(
        title = "Levelized Cost Comparison",
        x     = "Year",
        y     = "Cost",
        fill  = "Component"
      ) +
      ggplot2::theme_bw()
    return(p)
  }

  # ── type == "totals": dodged bars of total cost by year ─────────────────────
  dfs <- lapply(object, function(x) x$levcost)
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) stop("No levcost data available in list results.")

  df_all <- do.call(rbind, dfs)

  p <- ggplot2::ggplot(df_all, ggplot2::aes(x = year, y = levcost,
                                             fill = tech)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::labs(
      title = "Levelized Cost Comparison",
      x     = "Year",
      y     = "LCOE",
      fill  = "Technology"
    ) +
    ggplot2::theme_bw()
  p
}

# ── S3 method registration ─────────────────────────────────────────────────────
# In a sourced-script (non-package) context, ggplot2::autoplot() dispatches via
# UseMethod() but only finds methods registered in ggplot2's own namespace.
# registerS3method() here makes the methods discoverable regardless of how the
# file is loaded.
if (requireNamespace("ggplot2", quietly = TRUE)) {
  registerS3method("autoplot", "ideea_levcost",
                   autoplot.ideea_levcost,
                   envir = asNamespace("ggplot2"))
  registerS3method("autoplot", "ideea_levcost_list",
                   autoplot.ideea_levcost_list,
                   envir = asNamespace("ggplot2"))
}
