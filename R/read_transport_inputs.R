# read_transport_inputs.R
# Functions to read transport_inputs.xlsx and build energyRt model objects.
#
# Usage:
#   source("R/read_transport_inputs.R")
#   tabs  <- read_transport_inputs("data/transport_inputs.xlsx")
#   comms <- build_ldv_commodities(tabs)
#   techs <- build_ldv_technologies(tabs, fuel_energy = ldv_fuel_energy)
#   fleet <- build_ldv_fleet(techs, tabs)
#   sup   <- build_ldv_supply(tabs)
#   dems  <- build_ldv_demands(tabs)
#
# Requires: readxl, energyRt (for newCommodity / newSupply / newDemand),
#           ideea_vehicle.R (sourced from same R/ directory)

# ── 1. Read all tabs ─────────────────────────────────────────────────────────

#' Read all tabs from transport_inputs.xlsx into a named list of data.frames.
#'
#' @param xls_path Path to transport_inputs.xlsx.
#' @return Named list; names match the sheet names in the workbook.
read_transport_inputs <- function(xls_path = "data/transport_inputs.xlsx") {
  if (!file.exists(xls_path))
    stop("Excel file not found: ", xls_path)
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' is required: install.packages('readxl')")

  sheets <- readxl::excel_sheets(xls_path)
  tabs   <- lapply(sheets, function(s) {
    as.data.frame(readxl::read_excel(xls_path, sheet = s, na = c("", "NA")))
  })
  names(tabs) <- sheets
  tabs
}

# ── 1b. Generic repository builder ──────────────────────────────────────────

#' Wrap a named list of energyRt objects into a newRepository.
#'
#' @param name  Name of the repository (character).
#' @param objs  Named list of energyRt objects to add.
#' @return A \code{repository} object.
build_repository <- function(name, objs) {
  repo <- newRepository(name)
  Reduce(add, objs, repo)
}

# ── 2. Commodities ───────────────────────────────────────────────────────────

#' Build a repository of newCommodity() objects from the commodities tab.
#'
#' Supports emis_CO2 and emis_PM columns; NA values are silently skipped.
#'
#' @param tabs Named list returned by read_transport_inputs().
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a
#'   named list.
#' @return A \code{repository} or named list of newCommodity objects.
build_ldv_commodities <- function(tabs, branch = NULL, as_repo = TRUE) {
  df <- tabs[["commodities"]]
  if (is.null(df)) stop("'commodities' sheet not found in transport inputs.")

  # Filter by branch: include the requested branch's service comms + all shared fuels.
  # branch=NULL returns all rows (backward-compatible).
  if (!is.null(branch) && "branch" %in% names(df)) {
    df <- df[df[["branch"]] %in% c(branch, "shared"), , drop = FALSE]
  }

  comm_list <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]

    # Build emissions data.frame from non-NA emis columns
    emis_vals <- c(CO2 = r[["emis_CO2"]], PM = r[["emis_PM"]])
    emis_vals <- emis_vals[!is.na(emis_vals)]

    emis_df <- if (length(emis_vals) > 0) {
      data.frame(comm = names(emis_vals), emis = unname(emis_vals),
                 stringsAsFactors = FALSE)
    } else NULL

    misc_data <- list()
    if ("energy_content" %in% names(r) && !is.na(r[["energy_content"]])) {
      misc_data$energy_content <- as.numeric(r[["energy_content"]])
      misc_data$energy_unit    <- r[["energy_unit"]]
    }

    newCommodity(
      name      = r[["name"]],
      desc      = if (!is.na(r[["desc"]])) r[["desc"]] else "",
      unit      = r[["unit"]],
      timeframe = r[["timeframe"]],
      emis      = emis_df,
      misc      = misc_data
    )
  })
  names(comm_list) <- df[["name"]]
  if (as_repo) build_repository("repo_comm", comm_list) else comm_list
}

# ── 3. Demands ───────────────────────────────────────────────────────────────

#' Build a repository of newDemand() objects from the demand tab.
#'
#' Year columns (y2025…y2050) are read as numeric. NA values (or formula
#' cells not yet filled) are silently skipped — newDemand() handles sparse data.
#'
#' @param tabs Named list returned by read_transport_inputs().
#' @param pattern Optional regex pattern applied to the `name` column to select
#'   a subset of demand rows. E.g. \code{"PLDV"} for passenger only,
#'   \code{"FLDV"} for freight only. Default \code{NULL} returns all rows.
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a
#'   named list.
#' @return A \code{repository} or named list of newDemand objects.
build_ldv_demands <- function(tabs, pattern = NULL, branch = NULL, as_repo = TRUE) {
  df   <- tabs[["demand"]]
  if (is.null(df)) stop("'demand' sheet not found in transport inputs.")

  if (!is.null(branch) && "branch" %in% names(df)) {
    # branch column takes priority over pattern
    df <- df[df[["branch"]] == branch, , drop = FALSE]
    if (nrow(df) == 0) stop("No demand rows with branch == '", branch, "'.")
  } else if (!is.null(pattern)) {
    df <- df[grepl(pattern, df[["name"]], ignore.case = TRUE), , drop = FALSE]
    if (nrow(df) == 0) stop("No demand rows match pattern '", pattern, "'.")
  }

  yr_cols <- grep("^y[0-9]{4}$", names(df), value = TRUE)

  dem_list <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]

    # Build year-value pairs, dropping NAs
    yr_vals <- as.numeric(r[yr_cols])
    yrs     <- as.integer(sub("^y", "", yr_cols))
    keep    <- !is.na(yr_vals)

    dem_df <- if (any(keep)) {
      data.frame(
        region = r[["region"]],
        year   = yrs[keep],
        dem    = yr_vals[keep],
        stringsAsFactors = FALSE
      )
    } else NULL

    newDemand(
      name      = r[["name"]],
      desc      = if (!is.na(r[["desc"]])) r[["desc"]] else "",
      commodity = r[["commodity"]],
      unit      = r[["unit"]],
      dem       = dem_df
    )
  })
  names(dem_list) <- df[["name"]]
  if (as_repo) build_repository("repo_dem", dem_list) else dem_list
}

# ── 4. Supply ────────────────────────────────────────────────────────────────

#' Build a repository of newSupply() objects from the supply and infrastructure
#' tabs.
#'
#' Cost columns (y2025…y2050) supply time-varying availability cost (MUSD/PJ).
#' Rows where all year columns are NA are created with no availability data
#' (placeholder objects).
#'
#' @param tabs      Named list returned by read_transport_inputs().
#' @param base_year Integer; year used when only one cost value is available and
#'   no year column is present.
#' @param as_repo   Logical; if TRUE (default) return a newRepository, else a
#'   named list.
#' @return A \code{repository} or named list of newSupply objects (supply tab
#'   first, then infrastructure tab if present).
build_ldv_supply <- function(tabs, base_year = 2025L, as_repo = TRUE) {
  .build_one_supply_tab <- function(df) {
    yr_cols <- grep("^y[0-9]{4}$", names(df), value = TRUE)
    lapply(seq_len(nrow(df)), function(i) {
      r      <- df[i, ]
      region <- as.character(r[["region"]])   # coerce NA_logical → NA_character
      yr_vals  <- as.numeric(r[yr_cols])
      yrs      <- as.integer(sub("^y", "", yr_cols))
      keep     <- !is.na(yr_vals)

      ava_df <- if (any(keep)) {
        data.frame(
          region = region,
          year   = yrs[keep],
          cost   = yr_vals[keep],
          stringsAsFactors = FALSE
        )
      } else NULL

      newSupply(
        name         = r[["name"]],
        commodity    = r[["commodity"]],
        region       = region,
        availability = ava_df
      )
    })
  }

  sup <- .build_one_supply_tab(tabs[["supply"]])
  inf <- if (!is.null(tabs[["infrastructure"]])) {
    .build_one_supply_tab(tabs[["infrastructure"]])
  } else list()

  all_sup <- c(sup, inf)
  names(all_sup) <- c(tabs[["supply"]][["name"]],
                      if (!is.null(tabs[["infrastructure"]]))
                        tabs[["infrastructure"]][["name"]])
  if (as_repo) build_repository("repo_sup", all_sup) else all_sup
}

# ── 5. Technologies ──────────────────────────────────────────────────────────

#' Build ideea_vehicle() objects from the technologies tab.
#'
#' Maps the flat Excel columns to ideea_vehicle() arguments:
#'   eff_hwy / eff_cty / eff_unit  → fuel efficiency
#'   invcost_2025                   → vehicle_cost_usd (base year)
#'   fixom_2025                     → annual_cost_usd  (base year)
#'   olife / avg_km_yr / load_*    → vehicle physics
#'   fuels / primary_fuel / blend_* → fuel structure
#'
#' Year-varying invcost_20** and fixom_20** columns are preserved in the
#' returned list as attribute "invcost_ts" / "fixom_ts" for future use when
#' ideea_vehicle() gains explicit time-varying cost support.
#'
#' @param tabs         Named list returned by read_transport_inputs().
#' @param fuel_energy  Named numeric vector of energy content (MJ/L or MJ/kg);
#'   passed straight to ideea_vehicle().
#' @param service_hwy  Highway service commodity name (default "PLDVHWY").
#' @param service_cty  City service commodity name (default "PLDVCTY").
#' @param start_year   Integer; technology availability start year.
#' @param as_repo      Logical; if TRUE (default) return a newRepository, else
#'   a named list.
#' @return A \code{repository} or named list of ideea_vehicle objects with
#'   attributes "invcost_ts" and "fixom_ts".
build_ldv_technologies <- function(tabs,
                                    fuel_energy  = c(),
                                    service_hwy  = "PLDVHWY",
                                    service_cty  = "PLDVCTY",
                                    branch       = NULL,
                                    start_year   = 2022L,
                                    as_repo      = TRUE) {
  df <- tabs[["technologies"]]
  if (is.null(df)) stop("'technologies' sheet not found in transport inputs.")

  # Derive MUSD → cr. INR conversion factor from the settings tab
  # Formula: 1 MUSD = usd_to_crinr / 10 cr. INR
  settings_df   <- tabs[["settings"]]
  musd_to_crinr <- if (!is.null(settings_df) && "key" %in% names(settings_df)) {
    rate <- settings_df$value[settings_df$key == "usd_to_crinr"]
    if (length(rate) == 1 && !is.na(rate)) as.numeric(rate) / 10 else 1
  } else 1

  # Auto-derive fuel_energy from commodities tab when not explicitly provided.
  # Builds a named numeric vector from energy_content column (NA rows skipped).
  if (length(fuel_energy) == 0) {
    comm_df <- tabs[["commodities"]]
    if (!is.null(comm_df) && "energy_content" %in% names(comm_df)) {
      has_val    <- !is.na(comm_df[["energy_content"]])
      fuel_energy <- stats::setNames(as.numeric(comm_df[["energy_content"]][has_val]),
                                     comm_df[["name"]][has_val])
    }
  }

  # Filter by branch when column exists
  if (!is.null(branch) && "branch" %in% names(df)) {
    df <- df[df[["branch"]] == branch, , drop = FALSE]
    if (nrow(df) == 0) stop("No technology rows with branch == '", branch, "'.")
  }

  # Check for per-row service commodity columns (added in multi-branch build)
  has_service_cols <- all(c("service_hwy", "service_cty") %in% names(df))

  parse_fuels <- function(x) trimws(strsplit(x, "\\|")[[1]])

  # Identify invcost / fixom year columns (X_20** pattern after read_excel)
  inv_cols  <- sort(grep("^invcost_[0-9]{4}$", names(df), value = TRUE))
  fixom_cols <- sort(grep("^fixom_[0-9]{4}$",  names(df), value = TRUE))

  tech_list <- lapply(seq_len(nrow(df)), function(i) {
    p <- df[i, ]

    fuels      <- parse_fuels(p[["fuels"]])
    blend_fuel <- if (!is.na(p[["blend_fuel"]])) p[["blend_fuel"]] else NULL

    # Base-year cost values (USD/vehicle)
    invcost_base <- if ("invcost_2025" %in% names(p)) as.numeric(p[["invcost_2025"]]) else NULL
    fixom_base   <- if ("fixom_2025"   %in% names(p)) as.numeric(p[["fixom_2025"]])   else NULL

    # Get service commodities: per-row column takes priority over function param
      svc_hwy <- if (has_service_cols && !is.na(p[["service_hwy"]])) p[["service_hwy"]] else service_hwy
      svc_cty <- if (has_service_cols && !is.na(p[["service_cty"]])) p[["service_cty"]] else service_cty

      tech_obj <- ideea_vehicle(
      name          = p[["name"]],
      desc          = if (!is.na(p[["desc"]])) p[["desc"]] else "",
      fuels         = fuels,
      primary_fuel  = p[["primary_fuel"]],
      blend_fuel    = blend_fuel,
      blend_share_up = if (!is.na(p[["blend_share_up"]])) as.numeric(p[["blend_share_up"]]) else 0.1,

      service_hwy   = svc_hwy,
      service_cty   = svc_cty,

      eff_hwy       = as.numeric(p[["eff_hwy"]]),
      eff_cty       = as.numeric(p[["eff_cty"]]),
      eff_unit      = p[["eff_unit"]],
      fuel_energy   = fuel_energy,

      avg_km_yr            = as.numeric(p[["avg_km_yr"]]),
      capacity_units       = as.numeric(p[["capacity_units"]]),
      load_per_vehicle_hwy = as.numeric(p[["load_per_vehicle_hwy"]]),
      load_per_vehicle_cty = as.numeric(p[["load_per_vehicle_cty"]]),
      share_hwy_up         = as.numeric(p[["share_hwy_up"]]),
      share_cty_up         = as.numeric(p[["share_cty_up"]]),

      vehicle_cost_usd = invcost_base,
      annual_cost_usd  = fixom_base,

      olife      = as.integer(p[["olife"]]),
      region     = if (!is.na(p[["region"]])) p[["region"]] else NA_character_,
      start_year = start_year
    )

    # Attach time-varying cost series as attributes for future use
    cap_units <- as.numeric(p[["capacity_units"]])
    if (length(inv_cols) > 0) {
      yrs <- as.integer(sub("^invcost_", "", inv_cols))
      vals <- as.numeric(p[inv_cols]) * cap_units / 1e6 * musd_to_crinr  # USD/veh → cr.INR/1000veh
      attr(tech_obj, "invcost_ts") <- data.frame(year = yrs, value_crinr = vals)
    }
    if (length(fixom_cols) > 0) {
      yrs <- as.integer(sub("^fixom_", "", fixom_cols))
      vals <- as.numeric(p[fixom_cols]) * cap_units / 1e6 * musd_to_crinr
      attr(tech_obj, "fixom_ts") <- data.frame(year = yrs, value_crinr = vals)
    }

    tech_obj
  })
  names(tech_list) <- df[["name"]]
  if (as_repo) build_repository("repo_tech", tech_list) else tech_list
}
# ── 5b. Infrastructure as Technology ─────────────────────────────────────────────────

#' Build newTechnology objects from the infra_tech tab.
#'
#' Each row becomes one technology with LAND as input and a refueling/charging
#' service commodity as output. The optional cng_leak column adds an aeff row
#' with act2aout so fugitive CH4 is tracked as CNG_LEAK activity.
#'
#' @param tabs    Named list from read_transport_inputs().
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a list.
build_ldv_infra_techs <- function(tabs, as_repo = TRUE) {
  df <- tabs[["infra_tech"]]
  if (is.null(df)) {
    warning("infra_tech sheet not found in tabs — returning empty.")
    return(if (as_repo) newRepository("repo_infra_tech") else list())
  }
  inv_cols   <- sort(grep("^invcost_[0-9]{4}$", names(df), value = TRUE))
  fixom_cols <- sort(grep("^fixom_[0-9]{4}$",  names(df), value = TRUE))

  tech_list <- lapply(seq_len(nrow(df)), function(i) {
    p <- df[i, ]
    inp <- data.frame(comm = p[["input_comm"]],  unit = p[["input_unit"]],
                      group = "i", stringsAsFactors = FALSE)
    out <- data.frame(comm = p[["output_comm"]], unit = p[["output_unit"]],
                      group = "o", stringsAsFactors = FALSE)

    # Time-varying invcost from invcost_YYYY columns
    inv_arg <- if (length(inv_cols) > 0) {
      yrs  <- as.integer(sub("^invcost_", "", inv_cols))
      vals <- as.numeric(p[inv_cols])
      keep <- !is.na(vals)
      if (any(keep)) data.frame(region = NA_character_, year = yrs[keep],
                                invcost = vals[keep]) else NULL
    } else NULL

    # Time-varying fixom from fixom_YYYY columns
    fix_arg <- if (length(fixom_cols) > 0) {
      yrs  <- as.integer(sub("^fixom_", "", fixom_cols))
      vals <- as.numeric(p[fixom_cols])
      keep <- !is.na(vals)
      if (any(keep)) data.frame(region = NA_character_, year = yrs[keep],
                                fixom = vals[keep]) else NULL
    } else NULL

    # Variable O&M scalar
    varom_arg <- if ("varom" %in% names(p) && !is.na(p[["varom"]]))
      list(varom = as.numeric(p[["varom"]])) else NULL

    # Fugitive CH4 leakage via aeff act2aout (only for CNG station)
    aeff_arg <- if ("cng_leak" %in% names(p) && !is.na(p[["cng_leak"]]))
      data.frame(acomm = "CNG_LEAK", act2aout = as.numeric(p[["cng_leak"]]),
                 stringsAsFactors = FALSE)
    else NULL

    args <- list(
      name    = p[["name"]],
      desc    = p[["desc"]],
      input   = inp,
      output  = out,
      cap2act = as.numeric(p[["cap2act"]]),
      olife   = list(olife = as.integer(p[["olife"]]))
    )
    if (!is.null(inv_arg))   args$invcost <- inv_arg
    if (!is.null(fix_arg))   args$fixom   <- fix_arg
    if (!is.null(varom_arg)) args$varom   <- varom_arg
    if (!is.null(aeff_arg))  args$aeff    <- aeff_arg
    do.call(newTechnology, args)
  })
  names(tech_list) <- df[["name"]]
  if (as_repo) Reduce(add, tech_list, newRepository("repo_infra_tech"))
  else tech_list
}

#' Add cinp2ainp aeff rows to vehicle technologies linking fuel use to infra service.
#'
#' Reads the cinp_comms and cinp2ainp_ratio columns from infra_df to build a
#' fuel->infra map, then appends cinp2ainp aeff rows to each vehicle technology
#' whose input commodities match an entry in the map.
#'
#' @param tech_list Named list of technology objects.
#' @param infra_df  Data frame from tabs[["infra_tech"]].
#' @return Updated tech_list with aeff rows added.
add_infra_aeff <- function(tech_list, infra_df) {
  if (is.null(infra_df) || !("cinp_comms" %in% names(infra_df))) return(tech_list)

  # Build fuel -> list(acomm, cinp2ainp) map from infra_df rows
  fuel_map <- list()
  for (i in seq_len(nrow(infra_df))) {
    acomm <- infra_df[["output_comm"]][i]
    ratio <- as.numeric(infra_df[["cinp2ainp_ratio"]][i])
    fuels <- trimws(strsplit(infra_df[["cinp_comms"]][i], ",")[[1]])
    for (f in fuels) fuel_map[[f]] <- list(acomm = acomm, cinp2ainp = ratio)
  }

  lapply(tech_list, function(tch) {
    # Access input commodity names safely across energyRt slot variations
    inp_data  <- tryCatch(slot(tch, "input"), error = function(e) NULL)
    inp_comms <- if (is.null(inp_data))        character(0)
                 else if (is.data.frame(inp_data)) inp_data$comm
                 else if (isS4(inp_data))      inp_data@data$comm
                 else                          character(0)

    matched <- inp_comms[inp_comms %in% names(fuel_map)]
    if (length(matched) == 0) return(tch)

    new_rows <- do.call(rbind, lapply(matched, function(f) {
      m <- fuel_map[[f]]
      data.frame(acomm = m$acomm, comm = f, cinp2ainp = m$cinp2ainp,
                 stringsAsFactors = FALSE)
    }))
    combined <- rbind(tch@aeff, new_rows)
    update(tch, aeff = combined)
  })
}
# ── 6. Fleet (stock) ─────────────────────────────────────────────────────────

#' Apply fleet stock values from the fleet tab to technology objects.
#'
#' Sets the @capacity$stock slot on each matching technology.  Technologies
#' present in tech_list but absent from the fleet tab are left unchanged.
#'
#' @param tech_list  Named list or repository of ideea_vehicle (newTechnology)
#'   objects. If a repository is passed, the updated repository is returned.
#' @param tabs       Named list returned by read_transport_inputs().
#' @param branch     Optional character; when the fleet tab has a 'branch' column
#'   only rows matching this branch are applied. NULL (default) applies all rows.
#' @return Same type as tech_list (named list or repository) with
#'   @capacity$stock populated for each technology that appears in the fleet tab.
build_ldv_fleet <- function(tech_list, tabs, branch = NULL) {
  # Handle both plain list and repository inputs transparently
  is_repo <- inherits(tech_list, "repository")
  obj_list <- if (is_repo) tech_list@data else tech_list

  df <- tabs[["fleet"]]
  if (is.null(df)) {
    warning("'fleet' sheet not found — returning tech_list unchanged.")
    return(tech_list)
  }

  # Filter by branch when available
  if (!is.null(branch) && "branch" %in% names(df)) {
    df <- df[df[["branch"]] == branch, , drop = FALSE]
  }

  # ── Wide format: stock_YYYY columns (one row per tech) ─────────────────────
  # Build names like stock_2024, stock_2025, ... from whatever columns exist.
  stock_yr_cols <- sort(grep("^stock_[0-9]{4}$", names(df), value = TRUE))
  bound_cols    <- c("cap.lo", "cap.up", "cap.fx", "ncap.lo", "ncap.up", "ncap.fx")

  if (length(stock_yr_cols) > 0) {
    years <- as.integer(sub("^stock_", "", stock_yr_cols))

    for (nm in unique(df[["tech"]])) {
      if (!nm %in% names(obj_list)) next

      row        <- df[df[["tech"]] == nm, , drop = FALSE][1L, ]
      stock_vals <- as.numeric(row[stock_yr_cols])
      valid      <- !is.na(stock_vals)
      if (!any(valid)) next

      stock_df <- data.frame(
        region = row[["region"]],
        year   = years[valid],
        stock  = stock_vals[valid],
        stringsAsFactors = FALSE
      )
      # Single-valued bounds (not year-varying): replicate to fill every year row
      for (bnd in bound_cols) {
        if (bnd %in% names(row) && !is.na(row[[bnd]]))
          stock_df[[bnd]] <- row[[bnd]]
      }

      obj_list[[nm]] <- update(obj_list[[nm]], capacity = stock_df)
    }

  } else {
    # ── Long format (backward compat): year + stock columns ──────────────────
    stk <- df[!is.na(df[["stock"]]) & df[["stock"]] >= 0, ]

    for (nm in unique(stk[["tech"]])) {
      if (!nm %in% names(obj_list)) next

      rows <- stk[stk[["tech"]] == nm, , drop = FALSE]
      stock_df <- data.frame(
        region = rows[["region"]],
        year   = as.integer(rows[["year"]]),
        stock  = rows[["stock"]],
        stringsAsFactors = FALSE
      )
      for (bnd in bound_cols) {
        if (bnd %in% names(rows) && any(!is.na(rows[[bnd]])))
          stock_df[[bnd]] <- rows[[bnd]]
      }

      obj_list[[nm]] <- update(obj_list[[nm]], capacity = stock_df)
    }
  }

  if (is_repo) {
    tech_list@data <- obj_list
    tech_list
  } else {
    obj_list
  }
}

# ── 7. Fuel commodities (shared across branches) ──────────────────────────────

#' Return only the shared fuel commodity objects (branch == "shared").
#'
#' Useful when assembling a combined transport model where fuel commodities
#' are included once, not once per branch.
#'
#' @param tabs    Named list returned by read_transport_inputs().
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a list.
build_fuel_commodities <- function(tabs, as_repo = TRUE) {
  build_ldv_commodities(tabs, branch = "shared", as_repo = as_repo)
}

# ── 8. Branch repository builder ──────────────────────────────────────────────

#' Build a complete mini-model repository for one transport branch.
#'
#' Combines service commodities (branch-specific + shared fuels), fuel supply,
#' demands, and technologies (with fleet stock applied) for the given branch.
#' Returns a newRepository ready to pass to newModel().
#'
#' Infra supply objects (PETRO_REFUEL, CNG_REFUEL, LPG_REFUEL, EV_CHARGING)
#' are excluded — they are not yet linked via aeff/aux in technologies.
#'
#' @param tabs         Named list from read_transport_inputs().
#' @param branch       Character; branch identifier, e.g. "LDV_psgr", "HDV_frgt".
#' @param fuel_energy  Named numeric vector of fuel energy content passed to
#'   build_ldv_technologies().
#' @param start_year   Integer; technology availability start year (default 2022).
#' @return A \code{repository} object containing all model objects for the branch.
build_branch_repo <- function(tabs, branch, fuel_energy = c(), start_year = 2022L,
                              infra_as_tech = getOption("infra_as_tech", FALSE)) {
  infra_names <- c("SUP_PETRO_REFUEL", "SUP_CNG_REFUEL", "SUP_LPG_REFUEL", "SUP_EV_CHARGING")

  comm_list <- build_ldv_commodities(tabs, branch = branch, as_repo = FALSE)
  dem_list  <- build_ldv_demands(tabs, branch = branch, as_repo = FALSE)
  tech_list <- build_ldv_technologies(tabs, branch = branch, fuel_energy = fuel_energy,
                                      start_year = start_year, as_repo = FALSE)
  tech_list <- build_ldv_fleet(tech_list, tabs, branch = branch)

  sup_list  <- build_ldv_supply(tabs, as_repo = FALSE)
  fuel_sup  <- sup_list[!names(sup_list) %in% infra_names]

  # When infra_as_tech = TRUE: build INFR_* technology objects, link vehicle fuel
  # inputs to infra service commodities via cinp2ainp aeff rows, and add an
  # unconstrained SUP_LAND so INFR_* techs can draw on the LAND resource.
  infra_tech_list <- list()
  land_sup_obj    <- NULL
  if (isTRUE(infra_as_tech)) {
    infra_tech_list <- build_ldv_infra_techs(tabs, as_repo = FALSE)
    tech_list       <- add_infra_aeff(tech_list, tabs[["infra_tech"]])
    land_sup_obj    <- tryCatch(
      newSupply(
        name         = "SUP_LAND",
        commodity    = "LAND",
        region       = NA_character_,
        availability = data.frame(region = NA_character_, year = 2025L,
                                  cost = 0, stringsAsFactors = FALSE)
      ),
      error = function(e) { warning("Could not create SUP_LAND: ", e$message); NULL }
    )
  }

  all_objs <- c(
    unname(comm_list),
    unname(fuel_sup),
    if (!is.null(land_sup_obj)) list(land_sup_obj),
    unname(dem_list),
    unname(tech_list),
    unname(infra_tech_list)
  )

  repo_name <- paste0("repo_", gsub("[^A-Za-z0-9]", "_", branch))
  Reduce(add, all_objs, newRepository(repo_name))
}
