# install_latex_deps.R
# Ensures all LaTeX packages required by report_vehicle.Rmd (and rmarkdown's
# pandoc default template) are present in TinyTeX.
#
# Fast-path design (avoids slow tlmgr calls on every source()):
#   1. Session option  — skip entirely if already verified this R session.
#   2. Stamp file      — skip if a persistent stamp exists and is newer than
#                        this script (i.e. deps were installed in a prior session
#                        and the script has not been updated since).
#   3. Install         — only reached when something actually changed.

# --- 1. Session fast-path -------------------------------------------------
if (isTRUE(getOption("ideea.latex_deps_ok"))) {
  # Already verified in this R session — nothing to do.
} else {

  # --- 2. Stamp-file fast-path --------------------------------------------
  .stamp_file <- file.path(
    tools::R_user_dir("ideea_report", "cache"), "latex_deps_ok"
  )
  .script_file <- normalizePath(
    file.path(dirname(sys.frame(0)$filename %||% "R/install_latex_deps.R"),
              "install_latex_deps.R"),
    mustWork = FALSE
  )
  .stamp_ok <- file.exists(.stamp_file) &&
    file.info(.stamp_file)$mtime >= file.info(.script_file)$mtime

  if (.stamp_ok) {
    options(ideea.latex_deps_ok = TRUE)
    # message("LaTeX deps: OK (cached).")   # uncomment to debug
  } else {

    # --- 3. Actual install ------------------------------------------------
    if (!requireNamespace("tinytex", quietly = TRUE)) {
      message("tinytex R package is not installed — skipping LaTeX package check.")
    } else if (!tinytex::is_tinytex()) {
      message("TinyTeX distribution not found — skipping LaTeX package check.",
              "\nTo install TinyTeX run: tinytex::install_tinytex()")
    } else {
      # Packages used in report_vehicle.Rmd header-includes:
      .explicit_pkgs <- c(
        "booktabs",   # \usepackage{booktabs}  — kable tables
        "xcolor",     # \usepackage{xcolor}    — colour definitions
        "colortbl",   # \usepackage{colortbl}  — coloured table cells
        "float",      # \usepackage{float}     — figure/table float control
        "tabularx"    # \usepackage{tabularx}  — flexible-width tables
        # graphicx and array are bundled in base TinyTeX (graphics/tools bundles)
      )
      # Packages pulled in by rmarkdown's pandoc LaTeX template:
      .pandoc_pkgs <- c(
        "geometry",     # page-size / margin setup
        "lmodern",      # Latin Modern fonts
        "amsmath",      # maths environments
        "microtype",    # micro-typography
        "parskip",      # paragraph spacing
        "upquote",      # correct quotes in verbatim
        "iftex",        # \ifPDFTeX etc. conditionals
        "unicode-math", # Unicode maths (xelatex/lualatex path)
        "fontspec",     # font selection (xelatex/lualatex path)
        "babel"         # language support
      )

      .all_pkgs <- unique(c(.explicit_pkgs, .pandoc_pkgs))

      # Only install packages not already present (fast check via tl_pkgs())
      .installed <- tryCatch(tinytex::tl_pkgs(), error = function(e) character(0))
      .missing   <- setdiff(.all_pkgs, .installed)

      if (length(.missing) == 0L) {
        message("LaTeX deps: all ", length(.all_pkgs), " packages already installed.")
      } else {
        message("Installing ", length(.missing), " missing LaTeX package(s): ",
                paste(.missing, collapse = ", "))
        tinytex::tlmgr_install(.missing)
        message("LaTeX deps: OK.")
      }

      # Write stamp so future sessions skip directly to fast-path 2
      dir.create(dirname(.stamp_file), recursive = TRUE, showWarnings = FALSE)
      writeLines(as.character(Sys.time()), .stamp_file)
      options(ideea.latex_deps_ok = TRUE)
    }
  }

  rm(.stamp_file, .script_file, .stamp_ok)
}
