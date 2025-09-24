add_process_label <- function(x) {
  # browser()
  if (!is.data.frame(x)) stop("x is not a data.frame")
  if (is.null(x$process)) {
    warning("There is no `process` column in `x`. Skipping creation of process labels")
    return(x)
  }
  require(dplyr, quietly = T); require(data.table, quietly = T)
  # add a column with nice names for figures and tables
  x <- as.data.table(x)
  x[, process_label := NA_character_]
  x[grepl("EBIO", process), process_label := "Biomass"]
  x[grepl("ECOA", process), process_label := "Coal"]
  x[grepl("EHYD", process), process_label := "Hydro"]
  x[grepl("ENGCC", process), process_label := "Natural Gas"]
  x[grepl("EWIN", process), process_label := "Onshore Wind"]
  x[grepl("EWIF", process), process_label := "Offshore Wind"]
  x[grepl("ESOL", process), process_label := "Solar PV"]
  x[grepl("ENUC", process), process_label := "Nuclear"]
  x[grepl("STG_BTR", process), process_label := "Storage Battery"]
  x[grepl("ELC2EDC|EDC2ELC", process), process_label := "Converter station"]

  x
}

add_costs_label <- function(x) {
  # browser()
  if (!is.data.frame(x)) stop("x is not a data.frame")
  if (is.null(x$name)) {
    warning("There is no `name` column in `x`. Skipping creation of the labels")
    return(x)
  }
  require(dplyr, quietly = T); require(data.table, quietly = T)
  # add a column with nice names for figures and tables
  x <- as.data.table(x)
  x[, costs_label := NA_character_]
  x[grepl("^v.+Eac$", name, ignore.case = TRUE), costs_label := "Capital"]
  x[grepl("^v.+OMCost$", name, ignore.case = TRUE), costs_label := "O & M"]
  # x[grepl("^vSupCost$", name, ignore.case = TRUE), costs_label := "Fuel"]
  x[grepl("^vSupCost$", name, ignore.case = TRUE), costs_label := "Fuel"]
  x[grepl("^vTradeRowCost$", name, ignore.case = TRUE), costs_label := "Import"]
  # x <- as_tibble(x)

  x$costs_label <- factor(x$costs_label, ordered = TRUE,
                          levels = rev(c("Capital", "O & M", "Fuel", "Import")))
  x
}
