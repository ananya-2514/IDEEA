# =============================================================================
# sankey_transport.R
# Shared helper: energy-flow Sankey for the IDEEA transport model.
#
# Two exported functions:
#   make_sankey_data() — builds nodes/links lists from vTechInp (and optionally
#                        vTechOut for hover annotation)
#   plot_sankey()      — wraps make_sankey_data() into a plotly Sankey, with
#                        optional animation over a vector of years
#
# Inputs
#   df_inp : data.frame from getData(scen, "vTechInp", ...)  — must have
#             columns: tech, comm, year, value  (and optionally region/scenario)
#   df_out : (optional) getData(scen, "vTechOut", ...) for hover annotation
#
# Used by:
#   R/transport_explorer/app.R  — renderPlotly() in the Sankey tab
#   transport_report.qmd        — emit_tabset() in the Energy Flow slide
# =============================================================================

# ── Tech prefix → branch label ────────────────────────────────────────────────
# Longer prefixes must come first (LDVF_ before LDV_, HDVF_ before HDV_, etc.)
.SANKEY_PREFIX_MAP <- c(
  "LDVF_"  = "LDV freight",
  "LDV_"   = "LDV psgr",
  "HDVF_"  = "HDV freight",
  "HDV_"   = "HDV psgr",
  "ARCF_"  = "Aircraft freight",
  "ARC_"   = "Aircraft psgr",
  "RAILF_" = "Rail freight",
  "RAIL_"  = "Rail psgr",
  "SHIPF_" = "Ships freight",
  "SHIP_"  = "Ships psgr",
  "MOTOF_" = "Motorbikes freight",
  "MOTO_"  = "Motorbikes psgr"
)

.detect_branch_sankey <- function(tech) {
  vapply(as.character(tech), function(t) {
    for (pfx in names(.SANKEY_PREFIX_MAP)) {
      if (startsWith(t, pfx)) return(.SANKEY_PREFIX_MAP[[pfx]])
    }
    NA_character_
  }, character(1L))
}

# ── Colour palettes ──────────────────────────────────────────────────────────
.FUEL_COLOURS <- c(
  GSL    = "#e6550d",   # orange-red — gasoline
  DSL    = "#756bb1",   # purple     — diesel
  CNG    = "#74c476",   # green      — natural gas
  LPG    = "#fdae6b",   # light orange — LPG
  ELCPJ  = "#3182bd",   # blue       — electricity
  H2     = "#6baed6",   # light blue — hydrogen
  BIO    = "#31a354",   # dark green — biofuels
  GSL10  = "#fd8d3c",   # orange     — E10 blend
  OTHER  = "#bdbdbd"    # grey       — catch-all
)

.BRANCH_COLOURS <- c(
  "LDV psgr"          = "#1f77b4",
  "LDV freight"       = "#aec7e8",
  "HDV psgr"          = "#ff7f0e",
  "HDV freight"       = "#ffbb78",
  "Rail psgr"         = "#2ca02c",
  "Rail freight"      = "#98df8a",
  "Aircraft psgr"     = "#d62728",
  "Aircraft freight"  = "#ff9896",
  "Ships psgr"        = "#9467bd",
  "Ships freight"     = "#c5b0d5",
  "Motorbikes psgr"   = "#8c564b",
  "Motorbikes freight"= "#c49c94"
)

.fuel_colour <- function(comm) {
  cols <- .FUEL_COLOURS[as.character(comm)]
  cols[is.na(cols)] <- .FUEL_COLOURS[["OTHER"]]
  unname(cols)
}

.branch_colour <- function(branch) {
  cols <- .BRANCH_COLOURS[as.character(branch)]
  cols[is.na(cols)] <- "#bdbdbd"
  unname(cols)
}

# ── Colour for a mixed vector of node labels ────────────────────────────────
.node_colour <- function(label, group) {
  ifelse(
    group == "fuel",
    .fuel_colour(label),
    ifelse(group == "branch", .branch_colour(label),
           # tech level: colour by detected branch
           .branch_colour(.detect_branch_sankey(label)))
  )
}

# =============================================================================
#' Build Sankey nodes and links for one year.
#'
#' @param df_inp  data.frame: vTechInp for *one scenario* (scenario column
#'                optional). Must have columns: tech, comm, year, value.
#' @param year_val  integer. Year to extract.
#' @param tech_level  logical. If TRUE: 3-layer (Fuel → Tech → Branch).
#'                   If FALSE (default): 2-layer (Fuel → Branch).
#' @param branch_filter  character vector of branch labels to keep, or NULL for all.
#' @param df_out  optional data.frame (vTechOut) used for hover text on Branch
#'               nodes.  Must have columns: tech, comm, year, value.
#' @return list(nodes, links) where:
#'   nodes: data.frame(id, label, group, colour)     — 0-indexed
#'   links: data.frame(source, target, value, label) — 0-indexed
# =============================================================================
make_sankey_data <- function(df_inp,
                              year_val,
                              tech_level    = FALSE,
                              branch_filter = NULL,
                              df_out        = NULL) {
  stopifnot(is.data.frame(df_inp))
  stopifnot(all(c("tech", "comm", "year", "value") %in% names(df_inp)))

  # ── 1. Filter to requested year and non-zero positive flows ─────────────────
  df <- df_inp[df_inp$year == year_val & !is.na(df_inp$value) & df_inp$value > 0, ]
  if (nrow(df) == 0L) {
    return(list(nodes = data.frame(), links = data.frame()))
  }

  # ── 2. Detect branch for every tech ─────────────────────────────────────────
  df$branch <- .detect_branch_sankey(df$tech)
  df <- df[!is.na(df$branch), ]           # drop unrecognised techs
  if (nrow(df) == 0L) {
    return(list(nodes = data.frame(), links = data.frame()))
  }

  # ── 3. Optional branch filter ────────────────────────────────────────────────
  if (!is.null(branch_filter) && length(branch_filter) > 0L) {
    df <- df[df$branch %in% branch_filter, ]
    if (nrow(df) == 0L) return(list(nodes = data.frame(), links = data.frame()))
  }

  # ── 4. Build output hover text (per branch, from df_out) ────────────────────
  branch_hover <- character(0L)
  if (!is.null(df_out) && is.data.frame(df_out) &&
      all(c("tech", "comm", "year", "value") %in% names(df_out))) {
    dfo <- df_out[df_out$year == year_val & !is.na(df_out$value) & df_out$value > 0, ]
    dfo$branch <- .detect_branch_sankey(dfo$tech)
    dfo <- dfo[!is.na(dfo$branch), ]
    if (nrow(dfo) > 0L) {
      agg <- tapply(dfo$value, dfo$branch, sum, na.rm = TRUE)
      branch_hover <- paste0(
        sprintf("%.0f", agg), " (output)"
      )
      names(branch_hover) <- names(agg)
    }
  }

  # ── 5. Aggregate flows ───────────────────────────────────────────────────────
  if (tech_level) {
    # Fuel → Tech (sum over region/slice)
    lnk1 <- aggregate(value ~ comm + tech, data = df, FUN = sum, na.rm = TRUE)
    names(lnk1) <- c("from", "to", "value")
    lnk1$from_group <- "fuel"
    lnk1$to_group   <- "tech"

    # Tech → Branch
    lnk2 <- aggregate(value ~ tech + branch, data = df, FUN = sum, na.rm = TRUE)
    names(lnk2) <- c("from", "to", "value")
    lnk2$from_group <- "tech"
    lnk2$to_group   <- "branch"

    all_links_raw <- rbind(
      lnk1[, c("from", "to", "value", "from_group", "to_group")],
      lnk2[, c("from", "to", "value", "from_group", "to_group")]
    )
  } else {
    # Fuel → Branch (2-layer)
    lnk <- aggregate(value ~ comm + branch, data = df, FUN = sum, na.rm = TRUE)
    names(lnk) <- c("from", "to", "value")
    lnk$from_group <- "fuel"
    lnk$to_group   <- "branch"
    all_links_raw <- lnk[, c("from", "to", "value", "from_group", "to_group")]
  }

  # ── 6. Build unique node table (0-indexed) ───────────────────────────────────
  node_labels <- unique(c(all_links_raw$from, all_links_raw$to))
  # Assign group: fuel, tech, or branch
  node_groups <- vapply(node_labels, function(n) {
    if (n %in% all_links_raw$from[all_links_raw$from_group == "fuel"]) "fuel"
    else if (tech_level && n %in% all_links_raw$to[all_links_raw$to_group == "tech"]) "tech"
    else "branch"
  }, character(1L))

  nodes <- data.frame(
    id     = seq_along(node_labels) - 1L,  # 0-indexed for plotly
    label  = node_labels,
    group  = node_groups,
    colour = .node_colour(node_labels, node_groups),
    stringsAsFactors = FALSE
  )

  # ── 7. Append hover text for branch nodes ───────────────────────────────────
  if (length(branch_hover) > 0L) {
    hover_col <- branch_hover[nodes$label]
    hover_col[is.na(hover_col)] <- ""
    nodes$hover <- ifelse(
      nodes$group == "branch" & nchar(hover_col) > 0,
      paste0(nodes$label, "<br>", hover_col),
      nodes$label
    )
  } else {
    nodes$hover <- nodes$label
  }

  # ── 8. Map labels to indices in links ────────────────────────────────────────
  node_idx <- stats::setNames(nodes$id, nodes$label)
  links <- data.frame(
    source      = unname(node_idx[all_links_raw$from]),
    target      = unname(node_idx[all_links_raw$to]),
    value       = all_links_raw$value,
    label       = sprintf("%.1f PJ", all_links_raw$value),
    link_colour = paste0("rgba(180,180,180,0.35)"),
    stringsAsFactors = FALSE
  )
  links <- links[!is.na(links$source) & !is.na(links$target), ]

  list(nodes = nodes, links = links)
}

# =============================================================================
#' Plot an interactive Sankey diagram with plotly.
#'
#' @param df_inp   data.frame: output of getData(scen, "vTechInp", merge=FALSE)
#'                 or getData(named_list, ..., merge=TRUE) filtered to one
#'                 scenario.  Columns: tech, comm, year, value.
#' @param years    integer vector of years.
#'                 Length-1 → static single-year Sankey.
#'                 Length>1 → animated Sankey with a play/pause button and
#'                            a year slider (plotly frames).
#' @param title    character. Chart title (shown as plotly layout title).
#' @param tech_level logical. Show intermediate technology nodes (Fuel→Tech→Branch)
#'                 instead of the default 2-layer (Fuel→Branch).
#' @param branch_filter character vector or NULL.  Restrict to selected branches
#'                 (use .SANKEY_PREFIX_MAP values, e.g. "LDV psgr").
#' @param df_out   optional vTechOut data.frame for branch hover annotation.
#' @param height   integer. Plot height in pixels.
#'
#' @return A plotly htmlwidget.
# =============================================================================
plot_sankey <- function(df_inp,
                         years         = NULL,
                         title         = "",
                         tech_level    = FALSE,
                         branch_filter = NULL,
                         df_out        = NULL,
                         height        = 520L) {
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Package 'plotly' is required. Install with: install.packages('plotly')")

  if (is.null(df_inp) || nrow(df_inp) == 0L)
    return(plotly::plot_ly() |>
             plotly::layout(title = "No data available"))

  # Resolve years: default to all years present
  avail_years <- sort(unique(df_inp$year))
  if (is.null(years) || length(years) == 0L) {
    years <- avail_years
  } else {
    years <- intersect(as.integer(years), avail_years)
  }
  if (length(years) == 0L)
    return(plotly::plot_ly() |>
             plotly::layout(title = "No matching years in data"))

  # ── Build base (first year) ─────────────────────────────────────────────────
  base <- make_sankey_data(df_inp, years[[1L]], tech_level, branch_filter, df_out)
  if (nrow(base$nodes) == 0L)
    return(plotly::plot_ly() |>
             plotly::layout(title = paste("No data for year", years[[1L]])))

  make_trace <- function(sd) {
    list(
      type  = "sankey",
      orientation = "h",
      node = list(
        pad       = 15,
        thickness = 18,
        line      = list(color = "white", width = 0.5),
        label     = sd$nodes$hover,
        color     = sd$nodes$colour
      ),
      link = list(
        source = sd$links$source,
        target = sd$links$target,
        value  = sd$links$value,
        label  = sd$links$label,
        color  = sd$links$link_colour
      )
    )
  }

  base_trace <- make_trace(base)

  # ── Static (single year) ────────────────────────────────────────────────────
  if (length(years) == 1L) {
    return(
      plotly::plot_ly(
        type        = "sankey",
        orientation = "h",
        node        = base_trace$node,
        link        = base_trace$link,
        height      = height
      ) |>
        plotly::layout(
          title = list(text = if (nzchar(title)) title
                               else paste0("Energy Flows — ", years, " (PJ)")),
          font  = list(size = 12)
        )
    )
  }

  # ── Animated (multiple years) ───────────────────────────────────────────────
  # Build every frame independently.  Node sets can differ between years so
  # each frame carries a fully self-contained trace.
  frames <- lapply(years, function(yr) {
    sd <- make_sankey_data(df_inp, yr, tech_level, branch_filter, df_out)
    if (nrow(sd$nodes) == 0L) return(NULL)
    list(name = as.character(yr), data = list(make_trace(sd)))
  })
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0L)
    return(plotly::plot_ly() |>
             plotly::layout(title = "No data for requested years"))

  # Build base plotly object (first frame)
  p <- plotly::plot_ly(
    type        = "sankey",
    orientation = "h",
    node        = base_trace$node,
    link        = base_trace$link,
    height      = height
  ) |>
    plotly::layout(
      title  = list(
        text = if (nzchar(title)) title else "Energy Flows by Year (PJ)"
      ),
      font   = list(size = 12),
      updatemenus = list(
        list(
          type       = "buttons",
          showactive = FALSE,
          y = 1.12, x = 0.02, xanchor = "left",
          buttons = list(
            list(
              label  = "\u25b6 Play",
              method = "animate",
              args   = list(
                NULL,
                list(frame      = list(duration = 900L, redraw = TRUE),
                     transition = list(duration = 0L),
                     fromcurrent = TRUE, mode = "immediate")
              )
            ),
            list(
              label  = "\u23f8 Pause",
              method = "animate",
              args   = list(
                list(NULL),
                list(frame = list(duration = 0L, redraw = FALSE),
                     mode  = "immediate")
              )
            )
          )
        )
      ),
      sliders = list(
        list(
          active       = 0L,
          currentvalue = list(prefix = "Year: ", visible = TRUE,
                              xanchor = "center"),
          pad          = list(t = 50L),
          steps = lapply(seq_along(frames), function(i) {
            list(
              label  = frames[[i]]$name,
              method = "animate",
              args   = list(
                list(frames[[i]]$name),
                list(frame      = list(duration = 0L, redraw = TRUE),
                     transition = list(duration = 0L),
                     mode       = "immediate")
              )
            )
          })
        )
      )
    )

  # Inject frames directly into the plotly widget's JSON spec.
  # IMPORTANT: each frame must declare `traces = [0]` so plotly.js knows the
  # frame.data[0] replaces trace index 0 (the sankey trace). Without this the
  # play button silently no-ops.
  built <- plotly::plotly_build(p)
  built$x$frames <- lapply(frames, function(fr) {
    list(name = fr$name, data = fr$data, traces = list(0L))
  })
  built
}

# ── Convenience: available branch labels ──────────────────────────────────────
#' Return a sorted character vector of all branch labels present in df_inp.
sankey_branches <- function(df_inp) {
  if (is.null(df_inp) || !("tech" %in% names(df_inp))) return(character(0L))
  br <- .detect_branch_sankey(unique(df_inp$tech))
  sort(unique(br[!is.na(br)]))
}
