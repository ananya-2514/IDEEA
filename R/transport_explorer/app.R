# =============================================================================
# IDEEA Transport Explorer — Shiny app
# =============================================================================
# Launch from the project root:
#   shiny::runApp("R/transport_explorer")
# Or open this file in RStudio and click "Run App".
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(rpivotTable)
library(shinyWidgets)
library(plotly)
library(DT)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ── Filtering helpers ─────────────────────────────────────────────────────────
# Columns never shown as per-dimension filter selectors
FILTER_EXCLUDE <- c("year", "value", "slice", "scenario")

# Return names of filterable columns (categorical, >1 unique value)
filter_cols <- function(df) {
  cols <- setdiff(names(df), FILTER_EXCLUDE)
  cols[vapply(cols, function(cc) length(unique(df[[cc]])) > 1L, logical(1))]
}

# Build a flex row of pickerInputs, one per filterable column.
# Each picker shows a compact count badge and has built-in search + Select All/None.
make_filter_ui <- function(prefix, df) {
  cols <- filter_cols(df)
  if (length(cols) == 0)
    return(tags$small(class = "text-muted", "No filterable dimensions available."))
  div(
    style = "display:flex; flex-wrap:wrap; gap:6px; align-items:flex-start; margin-bottom:4px;",
    lapply(cols, function(cc) {
      vals <- sort(unique(as.character(df[[cc]])))
      div(
        style = "min-width:180px; flex:1;",
        pickerInput(
          paste0(prefix, "_flt_", cc), cc,
          choices  = vals,
          selected = vals,
          multiple = TRUE,
          options  = pickerOptions(
            actionsBox         = TRUE,        # Select All / Deselect All buttons
            liveSearch         = TRUE,        # search box inside dropdown
            liveSearchPlaceholder = "Search...",
            selectedTextFormat = "count > 2", # shows "N of M selected" when >2
            size               = 12,          # dropdown rows before scrolling
            noneSelectedText   = "(none)"
          )
        )
      )
    })
  )
}

# Subset df by filter inputs (reads input[[prefix_flt_col]] for each filterable col)
apply_filters <- function(df, prefix, input) {
  for (cc in filter_cols(df)) {
    flt_id  <- paste0(prefix, "_flt_", cc)
    flt_val <- input[[flt_id]]
    if (!is.null(flt_val) && length(flt_val) > 0)
      df <- df[as.character(df[[cc]]) %in% flt_val, , drop = FALSE]
  }
  df
}

# ── Palette helpers ───────────────────────────────────────────────────────────
PALETTE_CHOICES <- list(
  "Transport"      = c("Fuels" = "Fuels", "Branches" = "Branches",
                       "Vehicle type" = "Vehicle type"),
  "Viridis family" = c("Default" = "Default", "Viridis" = "Viridis",
                       "Magma" = "Magma", "Plasma" = "Plasma",
                       "Inferno" = "Inferno", "Cividis" = "Cividis",
                       "Turbo" = "Turbo")
)

# apply_palette: add a colour/fill scale to a ggplot.
# fill_values: the actual fill-column data vector (needed for transport schemes).
apply_palette <- function(p, palette, aes_type = "fill", fill_values = NULL) {
  if (palette %in% c("Fuels", "Branches", "Vehicle type")) {
    cfg <- switch(palette,
      "Fuels"        = FUEL_COLOUR_CONFIG,
      "Branches"     = BRANCH_COLOUR_CONFIG,
      "Vehicle type" = VEHICLE_TYPE_COLOUR_CONFIG
    )
    if (!is.null(fill_values) && length(fill_values) > 0) {
      cols <- resolve_colours(fill_values, cfg)
      if (aes_type == "fill")
        return(p + ggplot2::scale_fill_manual(values = cols))
      else
        return(p + ggplot2::scale_color_manual(values = cols))
    }
    return(p)
  }
  if (palette == "Default") return(p)
  opt <- tolower(palette)
  if (aes_type == "fill")
    p + scale_fill_viridis_d(option = opt, direction = -1)
  else
    p + scale_color_viridis_d(option = opt, direction = -1)
}

# ── Variable choice lists ──────────────────────────────────────────────────────
FLOWS_VAR_CHOICES <- list(
  "Commodity balance" = list(
    "vBalance" = "vBalance",
    "vInpTot"  = "vInpTot",
    "vOutTot"  = "vOutTot"
  ),
  "Technology output / input" = list(
    "vTechOut"    = "vTechOut",
    "vTechInp"    = "vTechInp",
    "vTechAct"    = "vTechAct",
    "vTechOutTot" = "vTechOutTot",
    "vTechInpTot" = "vTechInpTot",
    "vTechEmsFuel" = "vTechEmsFuel"
  ),
  "Technology capacity" = list(
    "vTechCap"            = "vTechCap",
    "vTechNewCap"         = "vTechNewCap",
    "vTechRetiredNewCap"  = "vTechRetiredNewCap"
  ),
  "Supply / Demand" = list(
    "vSupOut"    = "vSupOut",
    "vSupOutTot" = "vSupOutTot",
    "pDemand"    = "pDemand",
    "vDemInp"    = "vDemInp"
  ),
  "Trade" = list(
    "vImportTot" = "vImportTot",
    "vExportTot" = "vExportTot",
    "vImportRow" = "vImportRow",
    "vExportRow" = "vExportRow"
  ),
  "Emissions" = list(
    "vEmsFuelTot" = "vEmsFuelTot",
    "vAggOutTot"  = "vAggOutTot"
  ),
  "Storage" = list(
    "vStorageOut" = "vStorageOut",
    "vStorageInp" = "vStorageInp",
    "vStorageCap" = "vStorageCap"
  )
)

# Variables known to carry a 'comm' dimension (used to show/hide commodity filter)
VARS_WITH_COMM <- c(
  "vBalance", "vTechOut", "vTechInp", "vTechOutTot", "vTechInpTot",
  "vTechEmsFuel", "vDemInp", "vImportTot", "vExportTot",
  "vImportRow", "vExportRow", "vAggOutTot", "vStorageOut",
  "vStorageInp", "vEmsFuelTot", "vInpTot", "vOutTot"
)

COSTS_VAR_CHOICES <- c(
  "vObjective"     = "vObjective",
  "vTotalCost"     = "vTotalCost",
  "vTechEac"       = "vTechEac",
  "vTechInv"       = "vTechInv",
  "vTechOMCost"    = "vTechOMCost",
  "vSupCost"       = "vSupCost",
  "vSubsCost"      = "vSubsCost",
  "vTaxCost"       = "vTaxCost",
  "vTradeCost"     = "vTradeCost",
  "vStorageOMCost" = "vStorageOMCost"
)

# ── Working directory ─────────────────────────────────────────────────────────
# Shiny sets wd to the app directory when launched via runApp().
# Adjust back to the project root so source("R/setup.R") works.
if (!file.exists("R/setup.R") && file.exists("../../R/setup.R")) {
  setwd("../..")
}

# ── Source project helpers ─────────────────────────────────────────────────────
# setup.R loads IDEEA, tidyverse, sets solver + paths.
# Wrapped in try() because ideea_extra() may error when path is not set up.
suppressWarnings(try(source("R/setup.R"), silent = TRUE))

# ideea_levcost may be in the IDEEA package or in the project scripts.
if (!exists("ideea_levcost")) {
  suppressWarnings(try(source("R/ideea_levcost.R"), silent = TRUE))
}

# Sankey / riverplot helper
suppressWarnings(try(source("R/sankey_transport.R"), silent = TRUE))
# Fixed transport colour palettes (FUEL/BRANCH/VEHICLE_TYPE configs + resolve_colours)
suppressWarnings(try(source("R/transport_explorer/colour_config.R"), silent = TRUE))

# ── Helper: scan a directory for solved scenarios ──────────────────────────────
# An energyRt scenario is solved when its directory contains "scen.RData".
scan_scenarios <- function(path) {
  if (!dir.exists(path)) return(character(0))
  dirs   <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  solved <- dirs[file.exists(file.path(dirs, "scen.RData"))]
  stats::setNames(solved, basename(solved))   # name → full path
}

# Load a scenario into .scen using the directory basename as the key name.
# Returns the name it was stored under, or NULL on failure.
safe_load_scenario <- function(path) {
  nm <- basename(path)
  ok <- tryCatch(
    load_scenario(path, name = nm, env = .scen, overwrite = TRUE, verbose = FALSE),
    error = function(e) {
      message("Could not load: ", path, "\n  ", conditionMessage(e))
      FALSE
    }
  )
  if (isTRUE(ok)) nm else NULL
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title  = "IDEEA Transport Explorer",
  theme  = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 300,

    h6("Scenarios directory"),
    textInput(
      "scen_dir", NULL,
      value = tryCatch(get_scenarios_path(), error = function(e) ""),
      width = "100%"
    ),
    actionButton("btn_scan", "Scan", icon = icon("search"),
                 class = "btn-sm btn-outline-primary w-100 mb-2"),

    hr(class = "my-2"),
    h6("Select scenarios"),
    textInput("scen_filter", NULL, placeholder = "filter by name...", width = "100%"),
    div(
      style = "max-height: 240px; overflow-y: auto;",
      checkboxGroupInput("sel_scens", NULL, choices = character(0))
    ),
    layout_columns(
      col_widths = c(6, 6),
      actionButton("btn_load",    "Load",    icon = icon("download"),
                   class = "btn-sm btn-primary w-100 mt-1"),
      actionButton("btn_refresh", "Refresh", icon = icon("rotate-right"),
                   class = "btn-sm btn-outline-secondary w-100 mt-1")
    ),
    div(class = "mt-2", uiOutput("load_status_ui")),

    hr(class = "my-2"),
    h6("Years (global)"),
    pickerInput(
      "global_years", NULL,
      choices = character(0), multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,
                              selectedTextFormat = "count > 3",
                              noneSelectedText   = "(all years)")
    ),

    hr(class = "my-2"),
    h6("Settings"),
    downloadButton("settings_save", "Save",
                   class = "btn-sm btn-outline-secondary w-100 mb-1"),
    fileInput("settings_load", NULL,
              accept = ".json", buttonLabel = "Load",
              placeholder = "settings.json", width = "100%"),
    checkboxInput("settings_autosave", "Auto-save settings",  value = FALSE),
    checkboxInput("settings_autoload", "Auto-load on start", value = FALSE),
    checkboxInput("scen_autoload",     "Auto-load scenarios on start", value = FALSE)
  ),

  navset_tab(
    id = "main_tabs",

    # ── Demand ────────────────────────────────────────────────────────────────
    nav_panel(
      "Demand",
      card(
        card_header("Demand by year  [pDemand]"),
        # Row 1: chart controls
        layout_columns(
          col_widths = c(3, 3, 2, 1, 3),
          selectInput("dem_fill",   "Colour by",
                      choices = c("dem", "scenario"), selected = "dem"),
          selectInput("dem_facet",  "Facet by",
                      choices = c("none", "scenario", "dem"), selected = "none"),
          selectInput("dem_type",   "Chart type",
                      choices = c("Stacked bar", "Line"), selected = "Stacked bar"),
          div(class = "pt-4",
              checkboxInput("dem_legend", "Legend", value = TRUE)),
          selectInput("dem_pal",    "Palette", PALETTE_CHOICES, "Default")
        ),
        # Row 2: dynamic per-dimension filters (populated after Refresh)
        uiOutput("dem_filter_ui"),
        plotly::plotlyOutput("plot_demand", height = "400px")
      )
    ),

    # ── Capacity ──────────────────────────────────────────────────────────────
    nav_panel(
      "Capacity",
      card(
        card_header("Capacity by year"),
        layout_columns(
          col_widths = c(4, 3, 3, 2),
          selectInput("cap_var", "Variable",
                      choices = c(
                        "Technology"   = "",
                        "vTechCap"              = "vTechCap",
                        "vTechNewCap"           = "vTechNewCap",
                        "vTechRetiredNewCap"    = "vTechRetiredNewCap",
                        "vTechRetiredStock"     = "vTechRetiredStock",
                        "vTechRetiredStockCum"  = "vTechRetiredStockCum",
                        "Storage"      = "",
                        "vStorageCap"           = "vStorageCap",
                        "vStorageNewCap"        = "vStorageNewCap",
                        "Trade"        = "",
                        "vTradeCap"             = "vTradeCap",
                        "vTradeNewCap"          = "vTradeNewCap"
                      ),
                      selected = "vTechCap"),
          selectInput("cap_fill",   "Colour by",
                      choices = c("process", "tech", "scenario"), selected = "process"),
          selectInput("cap_facet",  "Facet by",
                      choices = c("none", "scenario", "process", "tech", "region"), selected = "none"),
          selectInput("cap_type",   "Chart type",
                      choices = c("Stacked bar", "Line"), selected = "Stacked bar")
        ),
        layout_columns(
          col_widths = c(1, 11),
          div(class = "pt-4",
              checkboxInput("cap_legend", "Legend", value = TRUE)),
          selectInput("cap_pal",    "Palette", PALETTE_CHOICES, "Default")
        ),
        uiOutput("cap_filter_ui"),
        plotly::plotlyOutput("plot_cap", height = "400px")
      )
    ),

    # ── Flows ─────────────────────────────────────────────────────────────────
    nav_panel(
      "Flows",
      card(
        card_header("Commodity flows \u2014 variable explorer"),
        # Row 1: variable, commodity, colour
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("flows_var",  "Variable",
                      choices  = FLOWS_VAR_CHOICES, selected = "vBalance"),
          selectInput("flows_comm", "Commodity",
                      choices = c("(all)" = "__all__"), selected = "__all__"),
          selectInput("flows_fill", "Colour by",
                      choices = c("process", "tech", "scenario"), selected = "process")
        ),
        # Row 2: facet, chart type, legend, palette
        layout_columns(
          col_widths = c(3, 3, 2, 1, 3),
          selectInput("flows_facet", "Facet by",
                      choices = c("none", "scenario", "process", "tech"), selected = "none"),
          selectInput("flows_type",  "Chart type",
                      choices = c("Stacked bar", "Line"), selected = "Stacked bar"),
          NULL,
          div(class = "pt-4",
              checkboxInput("flows_legend", "Legend", value = TRUE)),
          selectInput("flows_pal",   "Palette", PALETTE_CHOICES, "Default")
        ),
        uiOutput("flows_filter_ui"),
        plotly::plotlyOutput("plot_flows", height = "400px")
      )
    ),

    # ── Pivot ─────────────────────────────────────────────────────────────────
    nav_panel(
      "Pivot",
      card(
        card_header("Pivot table / chart — drag dimensions to rows / columns"),
        layout_columns(
          col_widths = c(3, 3, 6),
          selectInput(
            "pvt_var", "Variable",
            choices = c(
              # Commodity flows
              "vBalance", "vInpTot", "vOutTot",
              # Technology
              "vTechOut", "vTechInp", "vTechAct",
              "vTechOutTot", "vTechInpTot", "vTechEmsFuel",
              "vTechCap", "vTechNewCap", "vTechRetiredNewCap",
              "vTechEac", "vTechInv", "vTechOMCost",
              # Demand / Supply
              "pDemand", "vDemInp",
              "vSupOut", "vSupOutTot", "vSupCost",
              # Emissions
              "vEmsFuelTot", "vAggOutTot",
              # Trade
              "vImportTot", "vExportTot", "vImportRow", "vExportRow",
              # Storage
              "vStorageOut", "vStorageInp", "vStorageCap", "vStorageOMCost",
              # Costs / Objective
              "vObjective", "vTotalCost", "vTaxCost", "vSubsCost", "vTradeCost"
            ),
            selected = "vTechCap"
          ),
          div(class = "pt-4",
              checkboxInput("pvt_drop_zeros",
                            "Drop zero / NA rows", value = TRUE)),
          uiOutput("pvt_status_ui")
        ),
        rpivotTableOutput("pivot_tbl", height = "580px")
      )
    ),

    # ── Costs ─────────────────────────────────────────────────────────────────
    nav_panel(
      "Costs",
      card(
        card_header("Cost variables by year"),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("costs_var",   "Variable",
                      choices = COSTS_VAR_CHOICES, selected = "vObjective"),
          selectInput("costs_fill",  "Colour by",
                      choices = c("tech", "region", "comm", "scenario"), selected = "tech"),
          selectInput("costs_facet", "Facet by",
                      choices = c("none", "scenario", "tech", "region"), selected = "none")
        ),
        layout_columns(
          col_widths = c(3, 3, 2, 1, 3),
          selectInput("costs_type", "Chart type",
                      choices = c("Stacked bar", "Line"), selected = "Stacked bar"),
          NULL, NULL,
          div(class = "pt-4",
              checkboxInput("costs_legend", "Legend", value = TRUE)),
          selectInput("costs_pal", "Palette", PALETTE_CHOICES, "Default")
        ),
        uiOutput("costs_filter_ui"),
        plotly::plotlyOutput("plot_costs", height = "400px")
      )
    ),

    # ── Process ───────────────────────────────────────────────────────────────
    nav_panel(
      "Process",
      card(
        card_header("Levelized Cost of Energy  [ideea_levcost]"),
        layout_columns(
          col_widths = c(4, 4, 4),
          selectInput("proc_scen", "Scenario", choices = character(0)),
          pickerInput(
            "proc_techs", "Technologies",
            choices  = character(0),
            selected = character(0),
            multiple = TRUE,
            options  = pickerOptions(
              actionsBox            = TRUE,
              liveSearch            = TRUE,
              liveSearchPlaceholder = "Search...",
              selectedTextFormat    = "count > 2",
              size                  = 15,
              noneSelectedText      = "(none selected)"
            )
          ),
          div(
            numericInput("proc_discount", "Discount rate (WACC)",
                         value = 0.05, min = 0, max = 0.5, step = 0.01),
            checkboxInput("proc_overnight", "Use overnight cost", value = FALSE)
          )
        ),
        layout_columns(
          col_widths = c(3, 9),
          actionButton("btn_levcost", "Run Levcost",
                       icon = icon("calculator"),
                       class = "btn-warning w-100"),
          uiOutput("proc_status_ui")
        ),
        br(),
        # Filters: years + view selector populated after Run Levcost
        layout_columns(
          col_widths = c(4, 3, 2, 3),
          pickerInput(
            "proc_years", "Years",
            choices = character(0), multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,
                                    selectedTextFormat = "count > 3",
                                    noneSelectedText = "(all)")
          ),
          selectInput("proc_view", "View",
                      choices = c("Annual LCOE"       = "lcoe",
                                  "Cost breakdown"    = "breakdown",
                                  "LCOE"              = "npv"),
                      selected = "lcoe"),
          div(class = "pt-4",
              checkboxInput("proc_fix_scale", "Fix scale", value = FALSE)),
          selectInput("proc_pal", "Palette", PALETTE_CHOICES, "Default")
        ),
        plotly::plotlyOutput("proc_levcost_plot", height = "420px"),
        br(),
        h6("Detail table"),
        DT::dataTableOutput("proc_levcost_table")
      )
    ),

    # ── Sankey ────────────────────────────────────────────────────────────────
    nav_panel(
      "Sankey",
      card(
        card_header("Energy flow diagram  [vTechInp]"),
        layout_columns(
          col_widths = c(4, 4, 4),
          uiOutput("sankey_year_ui"),
          selectInput(
            "sankey_branches", "Branches",
            choices  = character(0),
            multiple = TRUE,
            selectize = TRUE
          ),
          div(
            checkboxInput("sankey_tech_level", "Show individual technologies",
                          value = FALSE),
            checkboxInput("sankey_animate",    "Animate over all report years",
                          value = TRUE)
          )
        ),
        plotly::plotlyOutput("plot_sankey", height = "520px")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    scen_paths     = character(0),  # name → full path
    loaded         = character(0),  # names of scenarios currently in .scen
    levcost_result = NULL           # ideea_levcost() output
  )

  # Counter that gates all chart rendering — increment to trigger a refresh
  refresh_counter <- reactiveVal(0L)

  # ── Scan directory ──────────────────────────────────────────────────────────
  observeEvent(input$btn_scan, {
    paths <- scan_scenarios(trimws(input$scen_dir))
    rv$scen_paths <- paths
    if (length(paths) == 0) {
      showNotification("No solved scenarios found in that directory.", type = "warning")
    } else {
      showNotification(paste0(length(paths), " scenario(s) found."), type = "message")
    }
    updateCheckboxGroupInput(session, "sel_scens",
                             choices  = names(paths),
                             selected = names(paths))
  })

  # ── Filter scenario list ────────────────────────────────────────────────────
  observe({
    paths <- rv$scen_paths
    if (length(paths) == 0) return()
    ft <- tolower(trimws(input$scen_filter))
    if (nchar(ft) > 0) paths <- paths[grepl(ft, tolower(names(paths)), fixed = TRUE)]
    updateCheckboxGroupInput(session, "sel_scens",
                             choices  = names(paths),
                             selected = names(paths))
  })

  # ── Load selected scenarios ─────────────────────────────────────────────────
  observeEvent(input$btn_load, {
    req(length(input$sel_scens) > 0)
    sel  <- input$sel_scens
    todo <- sel[!sel %in% names(rv$loaded)]

    withProgress(message = "Loading scenarios...", value = 0, {
      for (nm in todo) {
        incProgress(1 / max(length(todo), 1), detail = nm)
        loaded_nm <- safe_load_scenario(rv$scen_paths[[nm]])
        if (!is.null(loaded_nm)) rv$loaded <- union(rv$loaded, loaded_nm)
      }
    })

    # Evict deselected from .scen and tracking vector
    to_drop <- setdiff(rv$loaded, sel)
    for (nm in to_drop) if (exists(nm, envir = .scen)) rm(list = nm, envir = .scen)
    rv$loaded <- setdiff(rv$loaded, to_drop)

    # Auto-refresh charts after loading
    refresh_counter(refresh_counter() + 1L)
  })

  # ── Manual refresh ──────────────────────────────────────────────────────────
  observeEvent(input$btn_refresh, {
    refresh_counter(refresh_counter() + 1L)
  })

  # Populate the sidebar global years selector from the union of years across
  # all loaded scenarios. Uses pDemand as a representative variable (always present).
  observeEvent(refresh_counter(), {
    scens <- isolate(active_scens())
    if (length(scens) == 0L) return()
    yrs <- tryCatch({
      df <- getData(scens, name = "pDemand", merge = TRUE, process = TRUE)
      if (!is.null(df) && "year" %in% names(df)) sort(unique(as.integer(df$year)))
      else integer(0)
    }, error = function(e) integer(0))
    if (length(yrs) == 0L) return()
    cur <- isolate(input$global_years)
    keep <- intersect(as.character(cur), as.character(yrs))
    updatePickerInput(session, "global_years",
                      choices  = as.character(yrs),
                      selected = if (length(keep) > 0) keep else as.character(yrs))
  })

  # ── Status badge ────────────────────────────────────────────────────────────
  output$load_status_ui <- renderUI({
    n <- length(rv$loaded)
    if (n == 0) {
      tags$small(class = "text-muted", "No scenarios loaded")
    } else {
      tags$small(class = "text-success fw-semibold",
                 paste0(n, " scenario(s) in memory"))
    }
  })

  # ── Active scenario list (loaded ∩ selected) ─────────────────────────────────
  # Build a named list from .scen for the selected+loaded scenarios.
  # Named list ensures getData() adds a 'scenario' column correctly.
  active_scens <- reactive({
    sel <- intersect(input$sel_scens, rv$loaded)
    if (length(sel) == 0) return(list())
    scen_list <- lapply(sel, function(nm) get0(nm, envir = .scen))
    names(scen_list) <- sel
    Filter(Negate(is.null), scen_list)
  })

  # ── Shared ggplot2 helpers ────────────────────────────────────────────────────
  make_theme <- function(show_legend) {
    theme_bw(base_size = 13) +
      theme(axis.text.x   = element_text(angle = 45, hjust = 1),
            legend.position = if (isTRUE(show_legend)) "right" else "none")
  }

  make_facet <- function(p, facet_col, df) {
    if (!is.null(facet_col) && facet_col != "none" && facet_col %in% names(df))
      p <- p + facet_wrap(vars(.data[[facet_col]]))
    p
  }

  # Subset a long data.frame by sidebar global-years selection.
  apply_global_years <- function(df) {
    if (is.null(df) || !"year" %in% names(df)) return(df)
    sel <- input$global_years
    if (is.null(sel) || length(sel) == 0L) return(df)
    df[as.integer(df$year) %in% as.integer(sel), , drop = FALSE]
  }

  # Render any ggplot2 object as plotly with consistent legend behaviour.
  to_plotly <- function(p, tooltip = c("x", "y", "fill", "colour")) {
    plotly::ggplotly(p, tooltip = tooltip) |>
      plotly::layout(legend = list(itemclick      = "toggle",
                                   itemdoubleclick = "toggleothers")) |>
      plotly::config(displaylogo = FALSE)
  }

  # ── Demand tab ───────────────────────────────────────────────────────────────
  dem_raw <- eventReactive(list(refresh_counter(), input$global_years), {
    scens <- active_scens()
    if (length(scens) == 0) return(NULL)
    df <- tryCatch(getData(scens, name = "pDemand", merge = TRUE, process = TRUE),
                   error = function(e) NULL)
    apply_global_years(df)
  })

  # Update fill/facet choices to match the actual columns of the returned table.
  observeEvent(dem_raw(), {
    df <- dem_raw(); req(!is.null(df))
    cat_cols <- setdiff(names(df), c("year", "value", "slice"))
    if (length(cat_cols) == 0L) return()
    fill_choices  <- cat_cols
    facet_choices <- c("none", cat_cols)
    updateSelectInput(session, "dem_fill",  choices = fill_choices,
                      selected = if (input$dem_fill  %in% fill_choices)  input$dem_fill  else fill_choices[1])
    updateSelectInput(session, "dem_facet", choices = facet_choices,
                      selected = if (input$dem_facet %in% facet_choices) input$dem_facet else "none")
  })

  output$dem_filter_ui <- renderUI({
    df <- dem_raw(); req(!is.null(df))
    make_filter_ui("dem", df)
  })

  output$plot_demand <- plotly::renderPlotly({
    df  <- dem_raw(); req(!is.null(df), nrow(df) > 0)
    df  <- apply_filters(df, "dem", input)
    grp <- if (input$dem_fill %in% names(df)) input$dem_fill else names(df)[1]
    aes_t <- if (input$dem_type == "Stacked bar") "fill" else "colour"

    p <- if (input$dem_type == "Stacked bar") {
      ggplot(df, aes(x = factor(year), y = value, fill = .data[[grp]])) +
        geom_col(position = "stack") +
        labs(x = "Year", y = "Demand", fill = grp)
    } else {
      ggplot(df, aes(x = year, y = value, colour = .data[[grp]],
                     group = .data[[grp]])) +
        geom_line(linewidth = 1) + geom_point() +
        labs(x = "Year", y = "Demand", colour = grp)
    }
    p <- apply_palette(p, input$dem_pal, aes_t, fill_values = df[[grp]]) + make_theme(input$dem_legend)
    p <- make_facet(p, input$dem_facet, df)
    to_plotly(p)
  })

  # ── Capacity tab ─────────────────────────────────────────────────────────────
  cap_raw <- eventReactive(
    list(refresh_counter(), input$cap_var, input$global_years), {
    scens <- active_scens()
    if (length(scens) == 0) return(NULL)
    var <- input$cap_var %||% "vTechCap"
    if (!nzchar(var)) var <- "vTechCap"   # guard against optgroup separators
    df <- tryCatch(getData(scens, name = var, merge = TRUE, process = TRUE),
                   error = function(e) NULL)
    apply_global_years(df)
  })

  observeEvent(cap_raw(), {
    df <- cap_raw(); req(!is.null(df))
    cat_cols <- setdiff(names(df), c("year", "value", "slice"))
    if (length(cat_cols) == 0L) return()
    # Always offer 'process' so the user can pick it even on tables that have
    # no 'process' column (the plot grp guard falls back gracefully).
    fill_choices  <- union("process", cat_cols)
    facet_choices <- c("none", union("process", cat_cols))
    updateSelectInput(session, "cap_fill",  choices = fill_choices,
                      selected = if (input$cap_fill  %in% fill_choices)  input$cap_fill  else fill_choices[1])
    updateSelectInput(session, "cap_facet", choices = facet_choices,
                      selected = if (input$cap_facet %in% facet_choices) input$cap_facet else "none")
  })

  output$cap_filter_ui <- renderUI({
    df <- cap_raw(); req(!is.null(df))
    make_filter_ui("cap", df)
  })

  output$plot_cap <- plotly::renderPlotly({
    df  <- cap_raw(); req(!is.null(df), nrow(df) > 0)
    df  <- apply_filters(df, "cap", input)
    grp <- if (input$cap_fill %in% names(df)) input$cap_fill else names(df)[1]
    aes_t <- if (input$cap_type == "Stacked bar") "fill" else "colour"
    ylab  <- input$cap_var %||% "Capacity"

    p <- if (input$cap_type == "Stacked bar") {
      ggplot(df, aes(x = factor(year), y = value, fill = .data[[grp]])) +
        geom_col(position = "stack") +
        labs(x = "Year", y = ylab, fill = grp)
    } else {
      ggplot(df, aes(x = year, y = value, colour = .data[[grp]],
                     group = .data[[grp]])) +
        geom_line(linewidth = 1) + geom_point() +
        labs(x = "Year", y = ylab, colour = grp)
    }
    p <- apply_palette(p, input$cap_pal, aes_t, fill_values = df[[grp]]) + make_theme(input$cap_legend)
    p <- make_facet(p, input$cap_facet, df)
    to_plotly(p)
  })

  # ── Flows tab ────────────────────────────────────────────────────────────────
  # Update commodity selector when variable or refresh changes.
  observeEvent(list(refresh_counter(), input$flows_var), {
    scens <- active_scens()
    var <- input$flows_var
    if (length(scens) == 0 || is.null(var) || !nzchar(var)) return()
    if (var %in% VARS_WITH_COMM) {
      df <- tryCatch(getData(scens, name = var, merge = TRUE, process = TRUE),
                     error = function(e) NULL)
      if (!is.null(df) && "comm" %in% names(df)) {
        comms <- sort(unique(as.character(df$comm)))
        # Prepend an "(all)" sentinel that disables commodity filtering.
        choices <- c("(all)" = "__all__", stats::setNames(comms, comms))
        sel <- if ("CO2" %in% comms) "CO2" else "__all__"
        updateSelectInput(session, "flows_comm", choices = choices, selected = sel)
      } else {
        updateSelectInput(session, "flows_comm",
                          choices = c("(all)" = "__all__"), selected = "__all__")
      }
    } else {
      updateSelectInput(session, "flows_comm",
                        choices = c("(all)" = "__all__"), selected = "__all__")
    }
  }, ignoreInit = TRUE)

  flows_raw <- eventReactive(list(refresh_counter(), input$flows_var,
                                  input$flows_comm, input$global_years), {
    scens <- active_scens()
    if (length(scens) == 0) return(NULL)
    var      <- input$flows_var
    comm_val <- input$flows_comm
    has_comm <- !is.null(comm_val) && nzchar(comm_val) &&
                !identical(comm_val, "__all__") && var %in% VARS_WITH_COMM
    df <- tryCatch(
      if (has_comm)
        getData(scens, name = var, comm = comm_val, process = TRUE, merge = TRUE)
      else
        getData(scens, name = var, merge = TRUE, process = TRUE),
      error = function(e) NULL
    )
    apply_global_years(df)
  })

  # Update fill/facet dropdowns to match actual columns of the returned table.
  observeEvent(flows_raw(), {
    df <- flows_raw(); req(!is.null(df))
    cat_cols <- setdiff(names(df), c("year", "value", "slice"))
    if (length(cat_cols) == 0L) return()
    fill_choices  <- union("process", cat_cols)
    facet_choices <- c("none", union("process", cat_cols))
    updateSelectInput(session, "flows_fill",  choices = fill_choices,
                      selected = if (input$flows_fill  %in% fill_choices)  input$flows_fill  else fill_choices[1])
    updateSelectInput(session, "flows_facet", choices = facet_choices,
                      selected = if (input$flows_facet %in% facet_choices) input$flows_facet else "none")
  })

  output$flows_filter_ui <- renderUI({
    df <- flows_raw(); req(!is.null(df))
    make_filter_ui("flows", df)
  })

  output$plot_flows <- plotly::renderPlotly({
    df   <- flows_raw(); req(!is.null(df), nrow(df) > 0)
    df   <- apply_filters(df, "flows", input)
    grp  <- if (input$flows_fill %in% names(df)) input$flows_fill else
              setdiff(names(df), c("year", "value", "slice"))[1]
    aes_t <- if (input$flows_type == "Stacked bar") "fill" else "colour"
    ylab  <- if (nzchar(input$flows_comm) && !identical(input$flows_comm, "__all__"))
               paste0(input$flows_var, "  [", input$flows_comm, "]")
             else input$flows_var

    p <- if (input$flows_type == "Stacked bar") {
      ggplot(df, aes(x = factor(year), y = value, fill = .data[[grp]])) +
        geom_col(position = "stack") +
        labs(x = "Year", y = ylab, fill = grp)
    } else {
      ggplot(df, aes(x = year, y = value, colour = .data[[grp]],
                     group = .data[[grp]])) +
        geom_line(linewidth = 1) + geom_point() +
        labs(x = "Year", y = ylab, colour = grp)
    }
    p <- apply_palette(p, input$flows_pal, aes_t, fill_values = df[[grp]]) + make_theme(input$flows_legend)
    p <- make_facet(p, input$flows_facet, df)
    to_plotly(p)
  })

  # ── Costs tab ────────────────────────────────────────────────────────────────
  costs_raw <- eventReactive(list(refresh_counter(), input$costs_var, input$global_years), {
    scens <- active_scens()
    if (length(scens) == 0) return(NULL)
    df <- tryCatch(getData(scens, name = input$costs_var, merge = TRUE, process = TRUE),
                   error = function(e) NULL)
    apply_global_years(df)
  })

  # Update fill/facet choices after data arrives so they match actual columns
  observeEvent(costs_raw(), {
    df <- costs_raw(); req(!is.null(df))
    cat_cols <- setdiff(names(df), c("year", "value", "slice", "scenario"))
    all_choices <- c(cat_cols, "scenario")
    updateSelectInput(session, "costs_fill",  choices = all_choices,
                      selected = if (input$costs_fill  %in% all_choices) input$costs_fill  else all_choices[1])
    updateSelectInput(session, "costs_facet", choices = c("none", all_choices),
                      selected = if (input$costs_facet %in% c("none", all_choices)) input$costs_facet else "none")
  })

  output$costs_filter_ui <- renderUI({
    df <- costs_raw(); req(!is.null(df))
    make_filter_ui("costs", df)
  })

  output$plot_costs <- plotly::renderPlotly({
    df   <- costs_raw(); req(!is.null(df), nrow(df) > 0)
    df   <- apply_filters(df, "costs", input)

    # Some cost variables (e.g. vObjective, vTotalCost) are scalars and have
    # neither 'year' nor categorical dims. Render those as a horizontal bar
    # across scenarios (long scenario names read better when rotated).
    if (!"year" %in% names(df)) {
      x_col <- if ("scenario" %in% names(df)) "scenario" else names(df)[1]
      p <- ggplot(df, aes(x = .data[[x_col]], y = value, fill = .data[[x_col]])) +
        geom_col() +
        coord_flip() +
        labs(x = x_col, y = input$costs_var, fill = x_col)
      p <- apply_palette(p, input$costs_pal, "fill", fill_values = df[[x_col]]) +
        theme_bw(base_size = 13) +
        theme(legend.position = if (isTRUE(input$costs_legend)) "right" else "none")
      return(to_plotly(p))
    }

    grp  <- if (input$costs_fill %in% names(df)) input$costs_fill else
              setdiff(names(df), c("year", "value", "slice"))[1]
    if (is.na(grp) || is.null(grp)) grp <- if ("scenario" %in% names(df)) "scenario" else names(df)[1]
    aes_t <- if (input$costs_type == "Stacked bar") "fill" else "colour"

    p <- if (input$costs_type == "Stacked bar") {
      ggplot(df, aes(x = factor(year), y = value, fill = .data[[grp]])) +
        geom_col(position = "stack") +
        labs(x = "Year", y = input$costs_var, fill = grp)
    } else {
      ggplot(df, aes(x = year, y = value, colour = .data[[grp]],
                     group = .data[[grp]])) +
        geom_line(linewidth = 1) + geom_point() +
        labs(x = "Year", y = input$costs_var, colour = grp)
    }
    p <- apply_palette(p, input$costs_pal, aes_t, fill_values = df[[grp]]) + make_theme(input$costs_legend)
    p <- make_facet(p, input$costs_facet, df)
    to_plotly(p)
  })

  # ── Process / Levcost tab ───────────────────────────────────────────────────
  # Model data is structured as: scen@model@data = list(repo = <repository>)
  # Technologies live inside repository@data, not at the top level of model@data.
  # This helper searches both levels and returns a named list of technology objects.
  get_model_techs <- function(scen) {
    all_techs <- list()
    # 1. Top-level model@data (direct technology objects)
    if (.hasSlot(scen@model, "data")) {
      top <- Filter(function(x) inherits(x, "technology"), scen@model@data)
      all_techs <- c(all_techs, top)
    }
    # 2. Inside repository objects nested in model@data
    repos <- Filter(function(x) inherits(x, "repository"), scen@model@data)
    for (rp in repos) {
      if (.hasSlot(rp, "data")) {
        inner <- Filter(function(x) inherits(x, "technology"), rp@data)
        all_techs <- c(all_techs, inner)
      }
    }
    all_techs
  }

  # Get technology names for the picker; fall back to vTechCap result column
  get_tech_names <- function(scen) {
    techs <- tryCatch(get_model_techs(scen), error = function(e) list())
    if (length(techs) > 0) return(sort(names(techs)))
    # Fallback: derive tech names from solved results
    df <- tryCatch(
      getData(list(scen), "vTechCap", merge = FALSE),
      error = function(e) NULL
    )
    if (!is.null(df) && "tech" %in% names(df))
      return(sort(unique(as.character(df$tech))))
    character(0)
  }

  # Populate scenario + technology pickers after every refresh
  observeEvent(refresh_counter(), {
    scens <- active_scens()
    if (length(scens) == 0) return()
    updateSelectInput(session, "proc_scen",
                      choices  = names(scens),
                      selected = names(scens)[1])
    nm   <- names(scens)[1]
    scen <- scens[[nm]]
    techs <- get_tech_names(scen)
    updatePickerInput(session, "proc_techs",
                      choices  = techs,
                      selected = character(0))
  })

  # When scenario picker changes, reload the technology list
  observeEvent(input$proc_scen, {
    nm <- input$proc_scen
    if (is.null(nm) || !nzchar(nm)) return()
    scen <- get0(nm, envir = .scen)
    if (is.null(scen)) return()
    techs <- get_tech_names(scen)
    updatePickerInput(session, "proc_techs",
                      choices  = techs,
                      selected = character(0))
  }, ignoreInit = TRUE)

  observeEvent(input$btn_levcost, {
    req(nzchar(input$proc_scen), length(input$proc_techs) > 0)
    nm   <- input$proc_scen
    scen <- get0(nm, envir = .scen)
    req(!is.null(scen))

    all_techs <- get_model_techs(scen)
    sel_techs <- all_techs[names(all_techs) %in% input$proc_techs]

    if (length(sel_techs) == 0) {
      showNotification("No matching technology objects found in scenario.", type = "error")
      return()
    }

    # Pull the scenario's repository so ideea_levcost() can use the actual
    # supply prices instead of auto-creating zero-cost supplies.
    scen_repo <- NULL
    if (.hasSlot(scen@model, "data")) {
      repos <- Filter(function(x) inherits(x, "repository"), scen@model@data)
      if (length(repos) >= 1L) scen_repo <- repos[[1L]]
    }

    withProgress(message = "Computing levelized costs...", value = 0.3, {
      lc <- tryCatch(
        ideea_levcost(
          sel_techs,
          repo           = scen_repo,
          discount       = input$proc_discount,
          overnight_cost = input$proc_overnight,
          solver         = energyRt::solver_options$glpk,
          verbose        = FALSE
        ),
        error = function(e) {
          showNotification(paste("Levcost error:", conditionMessage(e)),
                           type = "error", duration = 15)
          NULL
        }
      )
    })
    if (!is.null(lc)) rv$levcost_result <- lc
  })

  output$proc_status_ui <- renderUI({
    lc <- rv$levcost_result
    if (is.null(lc))
      return(tags$small(class = "text-muted pt-2 d-inline-block",
                        "Select technologies and click 'Run Levcost'."))
    n <- if (inherits(lc, "ideea_levcost_list")) length(lc) else 1L
    tags$small(class = "text-success fw-semibold pt-2 d-inline-block",
               paste0("\u2713 Results for ", n, " technology/ies"))
  })

  # Combine $levcost across techs into one tidy data.frame
  proc_lcoe_df <- reactive({
    lc <- rv$levcost_result; if (is.null(lc)) return(NULL)
    if (inherits(lc, "ideea_levcost_list")) {
      do.call(rbind, lapply(lc, function(x) if (!is.null(x$levcost)) x$levcost))
    } else {
      lc$levcost
    }
  })

  # Combine $cost_breakdown across techs
  proc_breakdown_df <- reactive({
    lc <- rv$levcost_result; if (is.null(lc)) return(NULL)
    if (inherits(lc, "ideea_levcost_list")) {
      do.call(rbind, lapply(lc, function(x) if (!is.null(x$cost_breakdown)) x$cost_breakdown))
    } else {
      lc$cost_breakdown
    }
  })

  proc_breakdown_npv_df <- reactive({
    lc <- rv$levcost_result; if (is.null(lc)) return(NULL)
    if (inherits(lc, "ideea_levcost_list")) {
      do.call(rbind, lapply(lc, function(x) if (!is.null(x$cost_breakdown_npv)) x$cost_breakdown_npv))
    } else {
      lc$cost_breakdown_npv
    }
  })

  # Populate years picker each time results are refreshed
  observeEvent(rv$levcost_result, {
    df <- proc_lcoe_df()
    yrs <- if (!is.null(df) && "year" %in% names(df))
      sort(unique(stats::na.omit(as.integer(df$year)))) else integer(0)
    updatePickerInput(session, "proc_years",
                      choices  = as.character(yrs),
                      selected = as.character(yrs))
  })

  output$proc_levcost_plot <- plotly::renderPlotly({
    view <- input$proc_view %||% "lcoe"
    pal_aes <- "fill"
    yrs_sel <- input$proc_years
    yrs_sel <- if (length(yrs_sel) > 0) as.integer(yrs_sel) else NULL
    fix_scale <- isTRUE(input$proc_fix_scale)

    # When fix_scale is on, derive a single y-axis maximum from the union of
    # LCOE / breakdown stack tops / NPV stack tops so all three views share
    # the same scale (and breakdown facets are forced to identical y-axes).
    y_max <- NULL
    if (fix_scale) {
      candidates <- c()
      df_l <- tryCatch(proc_lcoe_df(),          error = function(e) NULL)
      df_b <- tryCatch(proc_breakdown_df(),     error = function(e) NULL)
      df_n <- tryCatch(proc_breakdown_npv_df(), error = function(e) NULL)
      if (!is.null(df_l) && nrow(df_l) > 0)
        candidates <- c(candidates, max(df_l$levcost, na.rm = TRUE))
      if (!is.null(df_b) && nrow(df_b) > 0) {
        bp <- stats::aggregate(value ~ year + tech, data = df_b,
                               FUN = function(x) sum(pmax(x, 0), na.rm = TRUE))
        candidates <- c(candidates, max(bp$value, na.rm = TRUE))
      }
      if (!is.null(df_n) && nrow(df_n) > 0) {
        np <- stats::aggregate(value ~ tech, data = df_n,
                               FUN = function(x) sum(pmax(x, 0), na.rm = TRUE))
        candidates <- c(candidates, max(np$value, na.rm = TRUE))
      }
      candidates <- candidates[is.finite(candidates) & candidates > 0]
      if (length(candidates) > 0) y_max <- max(candidates)
    }

    p <- if (view == "breakdown") {
      df <- proc_breakdown_df(); req(!is.null(df), nrow(df) > 0)
      if (!is.null(yrs_sel)) df <- df[df$year %in% yrs_sel, , drop = FALSE]
      req(nrow(df) > 0)
      facet_scales <- if (fix_scale) "fixed" else "free_y"
      ggplot(df, aes(x = factor(year), y = value, fill = component)) +
        geom_col(position = "stack") +
        facet_wrap(vars(tech), scales = facet_scales) +
        labs(x = "Year", y = "Cost component (per unit output)", fill = "Component")
    } else if (view == "npv") {
      df <- proc_breakdown_npv_df(); req(!is.null(df), nrow(df) > 0)
      ggplot(df, aes(x = tech, y = value, fill = component)) +
        geom_col(position = "stack") +
        labs(x = "Technology", y = "LCOE", fill = "Component") +
        coord_flip()
    } else {
      df <- proc_lcoe_df(); req(!is.null(df), nrow(df) > 0)
      if (!is.null(yrs_sel)) df <- df[df$year %in% yrs_sel, , drop = FALSE]
      req(nrow(df) > 0)
      ggplot(df, aes(x = factor(year), y = levcost, fill = tech)) +
        geom_col(position = "dodge") +
        labs(x = "Year", y = "Levelized Cost", fill = "Technology")
    }
    if (!is.null(y_max)) {
      # NPV view uses coord_flip(), so the limit applies to the x-aesthetic
      # in the original plot — ggplot's coord_cartesian handles both cases.
      p <- p + ggplot2::coord_cartesian(ylim = c(0, y_max * 1.02))
      if (view == "npv") {
        # Restore coord_flip with explicit xlim (since coord_cartesian above
        # would override the earlier coord_flip()).
        p <- p + ggplot2::coord_flip(ylim = c(0, y_max * 1.02))
      }
    }
    proc_fill_col  <- if (view == "lcoe") "tech" else "component"
    proc_fill_vals <- if (proc_fill_col %in% names(df)) df[[proc_fill_col]] else NULL
    p <- apply_palette(p, input$proc_pal %||% "Default", pal_aes,
                       fill_values = proc_fill_vals) + make_theme(TRUE)
    plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
      plotly::layout(legend = list(itemclick = "toggle",
                                   itemdoubleclick = "toggleothers"))
  })

  output$proc_levcost_table <- DT::renderDataTable({
    view <- input$proc_view %||% "lcoe"
    yrs_sel <- input$proc_years
    yrs_sel <- if (length(yrs_sel) > 0) as.integer(yrs_sel) else NULL
    df <- switch(view,
      breakdown = proc_breakdown_df(),
      npv       = proc_breakdown_npv_df(),
      proc_lcoe_df()
    )
    req(!is.null(df), nrow(df) > 0)
    if (!is.null(yrs_sel) && "year" %in% names(df))
      df <- df[df$year %in% yrs_sel, , drop = FALSE]
    DT::datatable(
      df, rownames = FALSE, filter = "top",
      options = list(pageLength = 10, scrollX = TRUE, dom = "tip")
    ) |> DT::formatRound(columns = intersect(c("levcost", "value"), names(df)),
                         digits = 4)
  })

  # ── Pivot tab ────────────────────────────────────────────────────────────────
  pvt_data <- eventReactive(list(refresh_counter(), input$pvt_var, input$global_years), {
    scens <- active_scens()
    if (length(scens) == 0) return(NULL)
    df <- tryCatch(
      getData(scens, name = input$pvt_var, merge = TRUE, process = TRUE),
      error = function(e) NULL
    )
    apply_global_years(df)
  })

  output$pvt_status_ui <- renderUI({
    df <- pvt_data()
    if (is.null(df)) return(tags$small(class = "text-muted",
                                       "No data \u2014 load scenarios then Refresh."))
    n_total <- nrow(df)
    n_nz    <- if ("value" %in% names(df)) sum(!is.na(df$value) & df$value != 0) else n_total
    tags$small(class = "text-muted pt-2 d-inline-block",
               sprintf("%s rows  \u00b7  %s non-zero", format(n_total, big.mark = ","),
                       format(n_nz, big.mark = ",")))
  })

  output$pivot_tbl <- renderRpivotTable({
    df <- pvt_data(); req(!is.null(df), nrow(df) > 0)
    df <- as.data.frame(df)

    if (isTRUE(input$pvt_drop_zeros) && "value" %in% names(df))
      df <- df[!is.na(df$value) & df$value != 0, , drop = FALSE]

    req(nrow(df) > 0)

    # Pick sensible defaults so values appear immediately
    # (default rpivotTable aggregator is "Count", which hides numeric values).
    rows <- intersect(c("year"), names(df))
    cols <- intersect(c("scenario"), names(df))
    if (length(cols) == 0L)
      cols <- intersect(c("tech", "comm", "region", "sup", "dem"), names(df))[1]
    cols <- cols[!is.na(cols)]

    rpivotTable(
      df,
      rows           = rows,
      cols           = cols,
      aggregatorName = "Sum",
      vals           = "value",
      rendererName   = "Table"
    )
  })

  # ── Sankey tab ───────────────────────────────────────────────────────────────
  sankey_inp <- eventReactive(list(refresh_counter(), input$global_years), {
    scens <- active_scens()
    if (length(scens) == 0) return(NULL)
    df <- tryCatch(
      getData(scens, name = "vTechInp", merge = TRUE, drop.zeros = TRUE),
      error = function(e) NULL
    )
    apply_global_years(df)
  })

  sankey_out <- eventReactive(list(refresh_counter(), input$global_years), {
    scens <- active_scens()
    if (length(scens) == 0) return(NULL)
    df <- tryCatch(
      getData(scens, name = "vTechOut", merge = TRUE, drop.zeros = TRUE),
      error = function(e) NULL
    )
    apply_global_years(df)
  })

  # Populate branch filter and year UI after data loads
  observeEvent(sankey_inp(), {
    df <- sankey_inp()
    if (is.null(df)) return()
    br <- if (exists("sankey_branches", mode = "function")) {
      sankey_branches(df)
    } else {
      character(0)
    }
    updateSelectInput(session, "sankey_branches",
                      choices = br, selected = br)
  })

  output$sankey_year_ui <- renderUI({
    # Hide single-year slider when animation is on (the embedded plotly
    # slider below the chart already covers all frames).
    if (isTRUE(input$sankey_animate)) {
      return(tags$small(class = "text-muted",
                        "Animation enabled \u2014 use the slider below the chart."))
    }
    df <- sankey_inp()
    yrs <- if (!is.null(df) && "year" %in% names(df)) sort(unique(df$year)) else 2025L
    sliderInput("sankey_year", "Year (single)",
                min = min(yrs), max = max(yrs),
                value = max(yrs), step = 5L, sep = "")
  })

  output$plot_sankey <- plotly::renderPlotly({
    df_inp <- sankey_inp()
    df_out <- sankey_out()
    req(!is.null(df_inp), nrow(df_inp) > 0)

    if (!exists("plot_sankey", mode = "function")) {
      return(plotly::plot_ly() |>
               plotly::layout(title = "sankey_transport.R not loaded"))
    }

    # Filter to selected scenario(s) if scenario column present
    active <- names(active_scens())
    if ("scenario" %in% names(df_inp) && length(active) > 0) {
      df_inp <- df_inp[df_inp$scenario %in% active, ]
      if (!is.null(df_out) && "scenario" %in% names(df_out))
        df_out <- df_out[df_out$scenario %in% active, ]
    }

    br_filter <- if (length(input$sankey_branches) > 0) input$sankey_branches else NULL
    animate   <- isTRUE(input$sankey_animate)

    yrs <- if (animate) {
      sort(unique(df_inp$year))
    } else {
      req(!is.null(input$sankey_year))
      as.integer(input$sankey_year)
    }

    plot_sankey(
      df_inp        = df_inp,
      years         = yrs,
      tech_level    = isTRUE(input$sankey_tech_level),
      branch_filter = br_filter,
      df_out        = df_out,
      height        = 500L
    )
  })

  # ── Save / Load settings ─────────────────────────────────────────────────────
  # Whitelist of input ids that describe view state worth persisting.
  # (Excludes file uploads, action button counters, scenario selection.)
  settings_input_ids <- function() {
    ids <- names(reactiveValuesToList(input))
    keep_prefixes <- c(
      "dem_", "cap_", "flows_", "costs_", "proc_", "pvt_",
      "sankey_", "global_", "scen_dir", "scen_filter"
    )
    drop_exact <- c("settings_load", "settings_save",
                    "settings_autosave", "settings_autoload",
                    "scen_autoload",
                    "btn_load", "btn_refresh", "btn_scan",
                    "btn_levcost", "main_tabs")
    ids <- ids[!ids %in% drop_exact]
    ids <- ids[grepl(paste0("^(", paste(keep_prefixes, collapse = "|"), ")"), ids)]
    sort(ids)
  }

  # Default location for auto-save / auto-load
  AUTOSAVE_PATH <- file.path(
    tools::R_user_dir("transport_explorer", which = "config"),
    "settings.json"
  )

  # Build the settings list (whitelisted inputs + scenario state)
  build_settings <- function() {
    ids <- settings_input_ids()
    vals <- lapply(ids, function(id) input[[id]])
    names(vals) <- ids
    # Persist autosave/autoload preferences so startup can honour them
    vals$settings_autosave <- isolate(input$settings_autosave)
    vals$settings_autoload <- isolate(input$settings_autoload)
    vals$scen_autoload     <- isolate(input$scen_autoload)
    # Persist scenario state separately so scen_autoload can act independently
    vals[["__scenarios__"]] <- list(
      scen_dir   = isolate(input$scen_dir),
      sel_scens  = isolate(input$sel_scens),
      loaded     = isolate(rv$loaded)
    )
    vals
  }

  write_settings_file <- function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(build_settings(), path, auto_unbox = TRUE,
                         pretty = TRUE, null = "null")
  }

  output$settings_save <- downloadHandler(
    filename = function() {
      paste0("transport_explorer_settings_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      jsonlite::write_json(build_settings(), file, auto_unbox = TRUE,
                           pretty = TRUE, null = "null")
    }
  )

  # Apply a parsed settings list to the running session
  apply_settings <- function(settings, load_scenarios = FALSE) {
    if (is.null(settings)) return(invisible(0L))
    scen_state <- settings[["__scenarios__"]]
    settings[["__scenarios__"]] <- NULL

    n_applied <- 0L
    for (id in names(settings)) {
      val <- settings[[id]]
      if (is.list(val) && length(val) == 1L) val <- val[[1L]]
      if (is.list(val)) val <- unlist(val, use.names = FALSE)
      tryCatch({
        session$sendInputMessage(id, list(value = val))
        n_applied <- n_applied + 1L
      }, error = function(e) NULL)
    }

    if (isTRUE(load_scenarios) && !is.null(scen_state)) {
      sd <- scen_state$scen_dir
      to_load <- scen_state$loaded %||% scen_state$sel_scens
      if (!is.null(sd) && nzchar(sd) && dir.exists(sd)) {
        paths <- scan_scenarios(sd)
        rv$scen_paths <- paths
        sel <- intersect(as.character(to_load), names(paths))
        updateCheckboxGroupInput(session, "sel_scens",
                                 choices = names(paths), selected = sel)
        if (length(sel) > 0L) {
          withProgress(message = "Auto-loading scenarios...", value = 0, {
            for (nm in sel) {
              incProgress(1 / length(sel), detail = nm)
              loaded_nm <- safe_load_scenario(paths[[nm]])
              if (!is.null(loaded_nm)) rv$loaded <- union(rv$loaded, loaded_nm)
            }
          })
        }
      }
    }

    refresh_counter(refresh_counter() + 1L)
    invisible(n_applied)
  }

  observeEvent(input$settings_load, {
    f <- input$settings_load
    req(f, file.exists(f$datapath))
    settings <- tryCatch(
      jsonlite::fromJSON(f$datapath, simplifyVector = FALSE),
      error = function(e) {
        showNotification(paste("Bad settings file:", conditionMessage(e)),
                         type = "error")
        NULL
      }
    )
    req(!is.null(settings))
    n_applied <- apply_settings(settings, load_scenarios = isTRUE(input$scen_autoload))
    showNotification(paste0("Restored ", n_applied, " setting(s)."),
                     type = "message")
  })

  # ── Auto-save (debounced) ───────────────────────────────────────────────────
  # Watch the same inputs we persist. Debounce so we write at most once per
  # 1.5 s of inactivity. Skip on the very first tick (initial UI build).
  autosave_trigger <- debounce(
    reactive({
      if (!isTRUE(input$settings_autosave)) return(NULL)
      ids <- settings_input_ids()
      lapply(ids, function(id) input[[id]])
    }),
    millis = 1500
  )

  autosave_started <- reactiveVal(FALSE)
  observe({
    val <- autosave_trigger()
    if (is.null(val)) return()
    if (!isTRUE(autosave_started())) {
      autosave_started(TRUE)
      return()  # skip first fire after enabling
    }
    tryCatch(write_settings_file(AUTOSAVE_PATH),
             error = function(e) message("Auto-save failed: ", conditionMessage(e)))
  })

  # Re-arm autosave when the checkbox is toggled
  observeEvent(input$settings_autosave, {
    autosave_started(FALSE)
  }, ignoreInit = TRUE)

  # ── Auto-load on startup ────────────────────────────────────────────────────
  # Run once after session starts, if the persisted settings file exists AND
  # the *previously-saved* autoload preference was TRUE. We bootstrap by
  # reading the file directly (the checkbox value isn't restored yet).
  observe({
    isolate({
      if (!file.exists(AUTOSAVE_PATH)) return()
      settings <- tryCatch(
        jsonlite::fromJSON(AUTOSAVE_PATH, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (is.null(settings)) return()
      want_autoload <- isTRUE(settings$settings_autoload)
      want_scens    <- isTRUE(settings$scen_autoload)
      if (!want_autoload && !want_scens) return()
      if (want_autoload) {
        n <- apply_settings(settings, load_scenarios = want_scens)
        showNotification(paste0("Auto-loaded ", n, " setting(s)."),
                         type = "message", duration = 4)
      } else if (want_scens) {
        # Apply only scenario state without restoring view inputs
        apply_settings(list("__scenarios__" = settings[["__scenarios__"]]),
                       load_scenarios = TRUE)
      }
    })
  })
}

shinyApp(ui, server)
