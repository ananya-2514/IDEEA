# ideea_report.r
# Summary reports for energyRt S4 objects in PDF, HTML, or LaTeX format.
# Requires: rmarkdown, ggplot2; PDF/TeX also require tinytex or a local LaTeX installation

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
#' @param file Character. Destination file path.  Defaults to a temporary file
#'   with the extension appropriate for `format`.
#' @param format Character. Output format: `"pdf"` (default), `"html"`, or
#'   `"tex"` (standalone LaTeX source, no PDF compilation).
#' @param levcost An \code{ideea_levcost} (or \code{ideea_levcost_list}) object
#'   returned by \code{\link{ideea_levcost}}, or \code{NULL} (default).  When
#'   \code{NULL} and any \code{ideea_levcost} arguments are passed via \code{...}
#'   (e.g. \code{group}, \code{repo}, \code{discount}), \code{ideea_levcost} is
#'   called automatically on \code{object} with those arguments.
#' @param ... Arguments forwarded to \code{\link{ideea_levcost}} (when
#'   \code{levcost = NULL} and levcost parameters are provided) and/or to
#'   \code{rmarkdown::render()}.  Known \code{ideea_levcost} parameter names are
#'   intercepted automatically; everything else is passed to the renderer.
#'
#' @return The path to the generated output file (invisibly).
#' @export
setGeneric(
  "ideea_report",
  function(object, type = NULL, image_file = NULL, file = NULL,
           format = c("pdf", "html", "tex"),
           fuel_energy = c(), levcost = NULL,
           cost_unit = NULL, cost_unit_comm = NULL, ...) {
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
           format = c("pdf", "html", "tex"),
           fuel_energy = c(), levcost = NULL,
           cost_unit = NULL, cost_unit_comm = NULL, ...) {

    format <- match.arg(format, c("pdf", "html", "tex"), several.ok = TRUE)

    # -- resolve preset type ------------------------------------------------
    if (is.null(type)) {
      cap_unit <- .slot_val(object@units, "capacity", "")
      type <- if (grepl("vehicle|car|veh|truck|bus",
                        tolower(cap_unit))) "vehicle" else "generic"
    }
    type <- match.arg(type, c("vehicle", "generic"))

    # -- default cost units for vehicle reports --------------------------------
    if (type == "vehicle") {
      if (is.null(cost_unit)) cost_unit <- "USD/(Vh*km)"
      if (is.null(cost_unit_comm)) cost_unit_comm <- "USD/(Passenger*km)"
    }

    # -- split ... into ideea_levcost args vs rmarkdown::render args ----------
    .levcost_params <- c("comm", "group", "repo", "fuel_costs",
                         "discount", "base_year",
                         "horizon", "calendar", "region", "weather",
                         "frontier", "solver",
                         "full_output", "verbose")
    dots       <- list(...)
    lc_dots    <- dots[intersect(names(dots), .levcost_params)]
    render_dots <- dots[setdiff(names(dots), .levcost_params)]

    # Auto-run ideea_levcost when caller supplied its args but not the result
    if (is.null(levcost) && length(lc_dots) > 0) {
      levcost <- tryCatch(
        do.call(ideea_levcost, c(list(object = object), lc_dots)),
        error = function(e) {
          warning("ideea_levcost() failed inside ideea_report: ",
                  conditionMessage(e), "\nLevelized cost section will be omitted.")
          NULL
        }
      )
    }

    # -- locate Rmd template ------------------------------------------------
    tmpl <- .find_template(type)

    # -- output file(s) -------------------------------------------------------
    # When multiple formats are requested, `file` is treated as a base path
    # (extension stripped/ignored) and each format gets its own extension.
    # When a single format is requested, `file` behaves as before.
    if (is.null(file)) {
      file_base <- tempfile(
        pattern = paste0("ideea_report_", gsub("[^A-Za-z0-9_]", "_",
                                               object@name), "_"))
    } else {
      file_base <- tools::file_path_sans_ext(file)
    }
    file_base <- normalizePath(file_base, mustWork = FALSE)

    # -- image path must be absolute ----------------------------------------
    image_file_abs <- NULL
    if (!is.null(image_file)) {
      if (!file.exists(image_file)) {
        warning("image_file not found and will be ignored: ", image_file)
      } else {
        image_file_abs <- normalizePath(image_file, mustWork = TRUE)
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

    # -- share frontier chart (from tech directly, always when groups present) -
    share_frontier_plot <- NULL
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      share_frontier_plot <- tryCatch({
        df <- tech_share_frontier(object)
        if (!is.null(df) && nrow(df) > 0) {
          plot_share_frontier(df, base_size = if (any(format %in% c("pdf", "tex"))) 8L else 11L)
        } else NULL
      }, error = function(e) {
        warning("share frontier plot failed: ", conditionMessage(e))
        NULL
      })
    }

    # -- generate levcost plots (compact, for inline report figure) ----------
    levcost_plot  <- NULL
    frontier_plot <- NULL
    if (!is.null(levcost)) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        warning("Package 'ggplot2' is required for levcost plots; they will be omitted.")
      } else {
        lc_obj <- if (inherits(levcost, "ideea_levcost_list")) levcost[[1]] else levcost
        compact_theme <- ggplot2::theme_bw(base_size = 10L) +
          ggplot2::theme(
            legend.position  = "bottom",
            legend.key.size  = ggplot2::unit(0.3, "cm"),
            legend.text      = ggplot2::element_text(size = 6),
            plot.title       = ggplot2::element_text(size = 8, face = "bold"),
            plot.subtitle    = ggplot2::element_text(size = 7),
            plot.caption     = ggplot2::element_text(size = 6),
            axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1,
                                                      size = 7),
            plot.margin      = ggplot2::margin(2, 4, 2, 2)
          )
        levcost_plot <- tryCatch({
          p <- autoplot(lc_obj, type = "npv", cost_unit = cost_unit,
                       cost_unit_comm = cost_unit_comm)
          if (!is.null(p)) p + compact_theme else NULL
        }, error = function(e) {
          warning("levcost plot (type='npv') failed: ", conditionMessage(e))
          NULL
        })
        if (!is.null(lc_obj$frontier) && nrow(lc_obj$frontier) > 0) {
          frontier_plot <- tryCatch({
            p <- autoplot(lc_obj, type = "frontier", cost_unit = cost_unit,
                         cost_unit_comm = cost_unit_comm)
            if (!is.null(p)) p + compact_theme else NULL
          }, error = function(e) {
            warning("frontier plot failed: ", conditionMessage(e))
            NULL
          })
        }
      }
    }

    # -- build params -------------------------------------------------------
    params <- .tech_report_params(object, type, image_file_abs, draw_file,
                                  fuel_energy = fuel_energy)
    params$levcost_plot          <- levcost_plot
    params$frontier_plot         <- frontier_plot
    params$share_frontier_plot   <- share_frontier_plot
    # Cost unit labels for template header
    params$units_costs      <- if (!is.null(cost_unit) && nzchar(cost_unit)) cost_unit else ""
    params$units_costs_comm <- if (!is.null(cost_unit_comm) && nzchar(cost_unit_comm)) cost_unit_comm else ""
    # Activity LCOE NPV
    params$levcost_npv <- NULL
    # Per-commodity LCOE NPVs (named numeric)
    params$levcost_npv_comm <- NULL
    if (!is.null(levcost)) {
      lc_src <- if (inherits(levcost, "ideea_levcost_list")) levcost[[1]] else levcost
      npv    <- lc_src$levcost_npv
      if (!is.null(npv)) params$levcost_npv <- as.numeric(npv)[1]
      # Per-commodity LCOE NPVs from frontier data
      lbc <- lc_src$levcost_by_comm
      if (!is.null(lbc) && nrow(lbc) > 0) {
        # Use output-only frontier scenarios (primary_input is NA)
        if ("primary_input" %in% names(lbc))
          lbc <- lbc[is.na(lbc$primary_input), , drop = FALSE]
        if (nrow(lbc) > 0)
          params$levcost_npv_comm <- tapply(lbc$value, lbc$comm, sum, na.rm = TRUE)
      }
    }

    # -- ensure TinyTeX is loaded when needed --------------------------------
    if (any(format %in% c("pdf", "tex"))) {
      if (!isNamespaceLoaded("tinytex") &&
          requireNamespace("tinytex", quietly = TRUE)) {
        loadNamespace("tinytex")
      } else if (!requireNamespace("tinytex", quietly = TRUE)) {
        message("Tip: install the 'tinytex' package to use TinyTeX for PDF rendering ",
                "(install.packages('tinytex'); tinytex::install_tinytex()).")
      }
    }

    # -- render each format (levcost computed once, reused) -----------------
    out_files <- character(0)
    for (fmt in format) {
      ext  <- switch(fmt, pdf = ".pdf", html = ".html", tex = ".tex")
      fout <- paste0(file_base, ext)

      out_fmt <- switch(fmt,
        pdf  = rmarkdown::pdf_document(latex_engine = "pdflatex", keep_tex = FALSE),
        html = rmarkdown::html_document(self_contained = TRUE),
        tex  = rmarkdown::latex_document()
      )

      # Adjust image path for LaTeX (spaces, backslashes)
      fmt_params <- params
      if (!is.null(image_file_abs) && fmt %in% c("pdf", "tex")) {
        img <- image_file_abs
        if (grepl(" ", img)) {
          ext_img <- tolower(tools::file_ext(img))
          tmp_img <- tempfile(pattern = "ideea_img_", fileext = paste0(".", ext_img))
          file.copy(img, tmp_img, overwrite = TRUE)
          img <- tmp_img
        }
        fmt_params$image_file <- gsub("\\\\", "/", img)
      }

      do.call(rmarkdown::render, c(
        list(
          input         = tmpl,
          output_format = out_fmt,
          output_file   = fout,
          params        = fmt_params,
          envir         = new.env(parent = globalenv()),
          quiet         = TRUE
        ),
        render_dots
      ))

      message("Report written to: ", fout)
      out_files <- c(out_files, fout)
    }

    invisible(if (length(out_files) == 1) out_files[1] else out_files)
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

# Format wrappers ----------------------------------------------------------

#' Generate a one-page PDF report for an energyRt object
#'
#' @description
#' Thin wrapper around \code{\link{ideea_report}} that fixes \code{format = "pdf"}.
#' See \code{\link{ideea_report}} for full parameter documentation.
#'
#' @inheritParams ideea_report
#' @return The path to the generated \code{.pdf} file (invisibly).
#' @export
setGeneric(
  "ideea_report_pdf",
  function(object, ...) standardGeneric("ideea_report_pdf")
)

#' @rdname ideea_report_pdf
#' @export
setMethod(
  "ideea_report_pdf",
  "technology",
  function(object, ...) ideea_report(object, ..., format = "pdf")
)

#' Generate a self-contained HTML report for an energyRt object
#'
#' @description
#' Thin wrapper around \code{\link{ideea_report}} that fixes \code{format = "html"}.
#' Produces a portable single-file \code{.html} document with embedded assets.
#' See \code{\link{ideea_report}} for full parameter documentation.
#'
#' @inheritParams ideea_report
#' @return The path to the generated \code{.html} file (invisibly).
#' @export
setGeneric(
  "ideea_report_html",
  function(object, ...) standardGeneric("ideea_report_html")
)

#' @rdname ideea_report_html
#' @export
setMethod(
  "ideea_report_html",
  "technology",
  function(object, ...) ideea_report(object, ..., format = "html")
)

#' Generate a standalone LaTeX report for an energyRt object
#'
#' @description
#' Thin wrapper around \code{\link{ideea_report}} that fixes \code{format = "tex"}.
#' Produces a standalone \code{.tex} file without compiling to PDF.
#' See \code{\link{ideea_report}} for full parameter documentation.
#'
#' @inheritParams ideea_report
#' @return The path to the generated \code{.tex} file (invisibly).
#' @export
setGeneric(
  "ideea_report_tex",
  function(object, ...) standardGeneric("ideea_report_tex")
)

#' @rdname ideea_report_tex
#' @export
setMethod(
  "ideea_report_tex",
  "technology",
  function(object, ...) ideea_report(object, ..., format = "tex")
)
