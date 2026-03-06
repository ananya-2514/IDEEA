# ideea_report.r - To add in IDEEA package
# One-page PDF summary reports for energyRt S4 objects.
# Requires: rmarkdown, tinytex (or a local LaTeX installation), ggplot2

# S4 generic ---------------------------------------------------------------

#' Generate a one-page PDF report for an energyRt object
#'
#' @description
#' Creates a single-page PDF document summarising key parameters of the object.
#' The layout and content are controlled by the `type` preset.
#'
#' @param object An energyRt S4 object. Currently supports `technology`.
#' @param type Character. Preset name that defines which parameters to display.
#'   Supported: `"vehicle"`. When `NULL` a preset is chosen automatically from
#'   the object's unit labels.
#' @param image_file Character. Optional path to a PNG/JPG image displayed in
#'   the upper-right corner of the page. `NULL` skips the image.
#' @param file Character. Destination PDF path.  Defaults to a temporary file.
#' @param ... Additional arguments forwarded to `rmarkdown::render()`.
#'
#' @return The path to the generated PDF (invisibly).
#' @export
setGeneric(
  "ideea_report",
  function(object, type = NULL, image_file = NULL, file = NULL,
           fuel_energy = c(), ...) {
    standardGeneric("ideea_report")
  }
)

# technology method --------------------------------------------------------

#' @rdname ideea_report
#' @export
setMethod(
  "ideea_report",
  "technology",
  function(object, type = NULL, image_file = NULL, file = NULL,
           fuel_energy = c(), ...) {

    # -- resolve preset type ------------------------------------------------
    if (is.null(type)) {
      cap_unit <- .slot_val(object@units, "capacity", "")
      type <- if (grepl("vehicle|car|veh|truck|bus",
                        tolower(cap_unit))) "vehicle" else "generic"
    }
    type <- match.arg(type, c("vehicle", "generic"))

    # -- locate Rmd template ------------------------------------------------
    tmpl <- .find_template(type)

    # -- output file --------------------------------------------------------
    if (is.null(file)) {
      file <- tempfile(
        pattern = paste0("ideea_report_", gsub("[^A-Za-z0-9_]", "_",
                                               object@name), "_"),
        fileext = ".pdf"
      )
    }
    file <- normalizePath(file, mustWork = FALSE)

    # -- image path must be absolute and space-free for LaTeX ---------------
    if (!is.null(image_file)) {
      if (!file.exists(image_file)) {
        warning("image_file not found and will be ignored: ", image_file)
        image_file <- NULL
      } else {
        image_file <- normalizePath(image_file, mustWork = TRUE)
        # pdflatex cannot handle spaces in paths – copy to a temp file
        if (grepl(" ", image_file)) {
          ext <- tolower(tools::file_ext(image_file))
          tmp_img <- tempfile(pattern = "ideea_img_", fileext = paste0(".", ext))
          file.copy(image_file, tmp_img, overwrite = TRUE)
          image_file <- tmp_img
        }
        # On Windows replace backslashes for LaTeX
        image_file <- gsub("\\\\", "/", image_file)
      }
    }

    # -- save draw() schematic to a temp PNG ----------------------------------
    # draw() prints to the active device rather than returning a ggplot,
    # so we capture it by opening a PNG device beforehand.
    draw_file <- tryCatch({
      tmp_draw <- tempfile(pattern = "ideea_draw_", fileext = ".png")
      grDevices::png(tmp_draw, width = 900, height = 600, res = 150, bg = "white")
      # prefer energyRt.dev if loaded, fall back to energyRt
      draw_fn <- if (isNamespaceLoaded("energyRt.dev")) {
        get("draw", envir = asNamespace("energyRt.dev"))
      } else {
        get("draw", envir = asNamespace("energyRt"))
      }
      draw_fn(object)
      grDevices::dev.off()
      # verify something was actually drawn (file > 5 kB)
      if (file.exists(tmp_draw) && file.info(tmp_draw)$size > 5000) tmp_draw else NULL
    }, error = function(e) {
      tryCatch(grDevices::dev.off(), error = function(e2) NULL)
      warning("draw() failed and will be omitted: ", conditionMessage(e))
      NULL
    })

    # -- build params -------------------------------------------------------
    params <- .tech_report_params(object, type, image_file, draw_file,
                                  fuel_energy = fuel_energy)

    # -- render -------------------------------------------------------------
    rmarkdown::render(
      input         = tmpl,
      output_format = rmarkdown::pdf_document(
        latex_engine = "pdflatex",
        keep_tex     = FALSE
      ),
      output_file   = file,
      params        = params,
      envir         = new.env(parent = globalenv()),
      quiet         = TRUE,
      ...
    )

    message("Report written to: ", file)
    invisible(file)
  }
)

# template finder ----------------------------------------------------------

#' Locate an Rmd report template by preset name
#' @param type character preset name (e.g. `"vehicle"`)
#' @return absolute path to the .Rmd file
#' @noRd
.find_template <- function(type) {
  fname <- paste0("report_", type, ".Rmd")

  # 1. Installed package path
  pkg_path <- suppressWarnings(
    tryCatch(system.file("templates", fname, package = "ideea"),
             error = function(e) "")
  )
  if (nzchar(pkg_path) && file.exists(pkg_path)) return(pkg_path)

  # 2. Development: search up from this file's directory
  candidates <- c(
    file.path(dirname(sys.frame(0)$ofile %||% "."), "..",
              "inst", "templates", fname),
    file.path("inst", "templates", fname)
  )
  for (p in candidates) {
    p2 <- tryCatch(normalizePath(p, mustWork = TRUE), error = function(e) NULL)
    if (!is.null(p2) && file.exists(p2)) return(p2)
  }

  stop(
    "Template '", fname, "' not found.\n",
    "Expected location: inst/templates/", fname
  )
}

# null-coalescing operator (base R equivalent) ----------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# params extractor ---------------------------------------------------------

#' Build the params list passed to rmarkdown::render
#' @param object technology S4 object
#' @param type character preset name
#' @param image_file absolute path or NULL
#' @param draw_file absolute path to schematic PNG or NULL
#' @noRd
.tech_report_params <- function(object, type, image_file, draw_file = NULL,
                                fuel_energy = c()) {

  # -- scalars --------------------------------------------------------------
  name    <- if (nzchar(object@name)) object@name else "(unnamed)"
  desc    <- if (nzchar(object@desc)) object@desc else ""
  cap2act <- object@cap2act

  # -- unit labels ----------------------------------------------------------
  units_cap   <- .slot_val(object@units, "capacity", "")
  units_act   <- .slot_val(object@units, "activity", "")
  units_costs <- .slot_val(object@units, "costs",    "")

  # -- operational parameters -----------------------------------------------
  olife_val <- if (nrow(object@olife) > 0) object@olife$olife[1] else NA_integer_
  start_val <- if (nrow(object@start) > 0) object@start$start[1] else NA_integer_
  end_val   <- if (nrow(object@end)   > 0) object@end$end[1]     else NA_integer_

  # -- capacity / stock -----------------------------------------------------
  cap_df <- object@capacity
  if (nrow(cap_df) > 0) {
    value_cols <- setdiff(names(cap_df), c("region", "year"))
    keep <- rowSums(!is.na(cap_df[, value_cols, drop = FALSE])) > 0
    cap_df <- cap_df[keep, , drop = FALSE]
  }
  # first non-NA stock value for the key-params panel
  stock_val <- if (nrow(cap_df) > 0 && "stock" %in% names(cap_df)) {
    v <- cap_df$stock[!is.na(cap_df$stock)]
    if (length(v) > 0) v[1] else NA_real_
  } else NA_real_

  # -- input / output tables ------------------------------------------------
  input_df  <- if (nrow(object@input)  > 0) object@input  else NULL
  output_df <- if (nrow(object@output) > 0) object@output else NULL

  # -- efficiency (ceff) ----------------------------------------------------
  ceff_df <- if (nrow(object@ceff) > 0) {
    preferred_cols <- c("comm", "cinp2use", "use2cact", "cact2cout",
                        "cinp2ginp", "share.lo", "share.up", "share.fx")
    cc  <- intersect(preferred_cols, names(object@ceff))
    df  <- object@ceff[, cc, drop = FALSE]
    # drop all-NA columns
    df  <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
    # drop rows with no numeric info beyond 'comm'
    val_cols <- setdiff(names(df), "comm")
    if (length(val_cols) > 0) {
      df <- df[rowSums(!is.na(df[, val_cols, drop = FALSE])) > 0, , drop = FALSE]
    }
    if (nrow(df) > 0) df else NULL
  } else NULL

  # -- costs ----------------------------------------------------------------
  invcost_df <- .clean_cost_df(object@invcost, "invcost")
  fixom_df   <- .clean_cost_df(object@fixom,   "fixom")
  varom_df   <- .clean_cost_df(object@varom,   "varom")

  # -- fuel efficiency table (human-readable units) -------------------------
  fuel_eff_df <- tryCatch(
    vehicle_eff_table(object, fuel_energy = fuel_energy),
    error = function(e) NULL
  )

  # -- vehicle params (captured by ideea_vehicle()) -------------------------
  vehicle_params_df <- tryCatch({
    vp <- object@misc[["vehicle_params"]]
    if (!is.null(vp) && length(vp) > 0) {
      nice_labels <- c(
        fuels          = "Fuels",
        primary_fuel   = "Primary fuel",
        eff_hwy        = "Efficiency (highway)",
        eff_cty        = "Efficiency (city)",
        avg_km_yr      = "Annual distance",
        capacity_units = "Capacity unit",
        load_hwy       = "Load/vehicle (hwy)",
        load_cty       = "Load/vehicle (city)",
        share_hwy_up   = "Max share (highway)",
        share_cty_up   = "Max share (city)",
        blend_fuel     = "Blend fuel",
        vehicle_cost   = "Vehicle cost",
        annual_cost    = "Annual fixed cost",
        olife          = "Lifetime"
      )
      nms    <- names(vp)
      labels <- ifelse(nms %in% names(nice_labels), nice_labels[nms], nms)
      data.frame(
        Parameter = labels,
        Value     = as.character(unlist(vp)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    } else NULL
  }, error = function(e) NULL)

  list(
    name        = name,
    desc        = desc,
    cap2act     = cap2act,
    units_cap   = units_cap,
    units_act   = units_act,
    units_costs = units_costs,
    olife       = olife_val,
    stock       = stock_val,
    start       = start_val,
    end         = end_val,
    cap_df      = cap_df,
    input_df    = input_df,
    output_df   = output_df,
    ceff_df     = ceff_df,
    invcost_df  = invcost_df,
    fixom_df    = fixom_df,
    varom_df    = varom_df,
    fuel_eff_df      = fuel_eff_df,
    vehicle_params_df = vehicle_params_df,
    image_file  = image_file,
    draw_file   = draw_file
  )
}

# small helpers ------------------------------------------------------------

#' Extract a single value from a slot that may be a data.frame or list
#' @noRd
.slot_val <- function(x, col, default = NA) {
  if (is.data.frame(x) && col %in% names(x) && nrow(x) > 0) {
    return(x[[col]][1])
  }
  if (is.list(x) && col %in% names(x) && length(x[[col]]) > 0) {
    return(x[[col]][1])
  }
  default
}

#' Keep only relevant columns from a cost data.frame, dropping all-NA rows
#' @noRd
.clean_cost_df <- function(df, cost_col) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  keep_cols <- intersect(c("region", "year", cost_col), names(df))
  df <- df[, keep_cols, drop = FALSE]
  val_cols <- setdiff(keep_cols, "region")
  if (length(val_cols) == 0) return(NULL)
  df <- df[rowSums(!is.na(df[, val_cols, drop = FALSE])) > 0, , drop = FALSE]
  if (nrow(df) == 0) NULL else df
}
