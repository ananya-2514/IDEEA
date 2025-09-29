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

ideea_gif_cf2 <- function(
    x,
    ideea_cl_sf,
    ideea_sf = NULL,
    cf_name = names(x)[grepl("cf_"), names(x)][1],
    slice = unique(x$slice)[1:24],
    timestamp.stamp = TRUE,
    # return_data = FALSE,
    fill_scale_lims = range(x[[cf_name]]),
    fps = 12,
    gif.width = 576, gif.height = 576,
    filename = "ideea_cl.gif"
) {

  verbose <- TRUE
  nframes <- length(slice)

  animation::saveGIF({
    if (verbose) cat("frame:")
    for (i in 1:nframes) {
      if (verbose) cat(format(i, width = nchar(nframes) + 1))
      # ii <- x[[timestamp.variable]] == frames[i]
      # arg$x <- x[ii,]
      arg <- list(
        x = x,
        ideea_cl_sf = ideea_cl_sf,
        ideea_sf = ideea_sf,
        cf_name = cf_name,
        slice = slice[i],
        timestamp.stamp = timestamp.stamp,
        return_data = FALSE,
        fill_scale_lims = fill_scale_lims
      )
      a <- do.call(ideea_snapshot_cf, arg, quote = FALSE)
      # a <- rlang::exec(.fn = FUN, !!!arg)
      if (i == nframes) {
        if (verbose) cat(" -> creating GIF\n")
      } else {
        if (verbose) cat(rep("\b", nchar(nframes) + 1), sep = "")
      }
      if (!is.null(a)) print(a)
    }
  },
  interval = 1/fps, ani.width = gif.width, ani.height = gif.height,
  ani.res = 600,
  movie.name = filename
  )
}

ideea_snapshot_cf <- function(
    x,
    ideea_cl_sf,
    ideea_sf = NULL,
    cf_name = names(x)[grepl("cf_"), names(x)][1],
    # cf_name = "wcf_100m",
    slice = sample(x$slice, 1),
    timestamp.stamp = TRUE,
    return_data = FALSE,
    fill_scale_lims = range(x[[cf_name]]),
    ...
) {
  # browser()
  SLICE <- slice

  if (inherits(ideea_cl_sf$cluster, "factor")) {
    ideea_cl_sf <- mutate(
      ideea_cl_sf,
      cluster = as.integer(as.character(cluster))
    )
  }

  if (!inherits(x$cluster, "integer")) {
    x <- mutate(x, cluster = as.integer(cluster))
  }

  ideea_cl_sf <- ideea_cl_sf |>
    select(starts_with("reg"), cluster, any_of(c("offshore", "mainland")))

  vrs <- intersect(names(x), names(ideea_cl_sf))

  d <- x |>
    filter(slice %in% SLICE) |>
    # filter(slice %in% x$slice[1000]) |>
    full_join(ideea_cl_sf, by = vrs, relationship = "many-to-many") |>
    st_as_sf()

  if (return_data) return(d)

  a <- ggplot()

  if (!is.null(ideea_sf)) {
    a <- a + geom_sf(data = ideea_sf, fill = "grey")
  }

  a <- a + geom_sf(aes(fill = .data[[cf_name]]), inherit.aes = F, data = d,
                   color = alpha("grey25", .25))

  if (is.numeric(d[[cf_name]])) {
    a <- a + scale_fill_viridis_c(option = "H", limits = range(x[[cf_name]]))
  } else {
    a <- a + scale_fill_viridis_d(option = "H")
  }

  if (isTRUE(timestamp.stamp)) {
    timestamp.stamp <- tsl2dtm(SLICE, year = x$year[1], tmz = "Asia/Kolkata") |>
      format("%Y-%b-%d, %Hh %Z")
  }
  timestamp.position <- c(97, 39.5)
  if (!isFALSE(timestamp.stamp) & is.character(timestamp.stamp)) {
    a <- a + geom_label(
      data = data.frame(x = timestamp.position[1],
                        y = timestamp.position[2],
                        label = timestamp.stamp),
      aes(x = x, y = y, label = label), label.size = 0., color = "black",
      hjust = 1, vjust = 1, fill = "white", alpha = 0.7) +
      labs(x = "", y = "")
  }

  a + scale_x_continuous(expand = c(0., 0.), limits = c(65, 97)) +
    scale_y_continuous(expand = c(0., 0.), limits = c(4, 40)) +
    theme_void()
}
