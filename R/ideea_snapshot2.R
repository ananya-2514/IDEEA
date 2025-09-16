ideea_snapshot2 <- function(scen, YEAR, SLICE, return_data = FALSE) {
  # browser()
  h <- getHorizon(scen)
  if (!any(YEAR %in% h@intervals$mid)) {
    stop("YEAR is not in the scenario range")
  }

  if (!any(SLICE %in% scen@settings@calendar@timetable$slice)) {
    stop("SLICE is not in the scenario range")
  }

  # browser()
  vTechOut_ELC <- getData(
    list(scen), "vTechOut", process = T, merge = T,
    year = YEAR, slice = SLICE,
    comm = "ELC", tech_ = "^E", drop.zeros = T, digits = 1) |>
    drop_process_cluster() |>
    drop_process_vintage() |>
    as.data.table()

  vStorageOut_ELC <- getData(
    list(scen), "vStorageOut", process = T, merge = T,
    year = YEAR, slice = SLICE,
    comm = "ELC", drop.zeros = T, digits = 1) |>
    # mutate(process = "Storage-Out") |>
    drop_process_vintage() |>
    # drop_cluster() |>
    as.data.table()

  vStorageInp_ELC <- getData(
    list(scen), "vStorageInp", process = T, merge = T,
    year = YEAR, slice = SLICE,
    comm = "ELC", drop.zeros = T, digits = 1) |>
    # mutate(process = "Storage-Inp") |>
    drop_process_vintage() |>
    # drop_cluster() |>
    as.data.table()

  vStorageIO_ELC <- rbind(
    vStorageOut_ELC,
    mutate(vStorageInp_ELC, value = -value)
  )
  # browser()
  vDailyOp <- rbindlist(
    list(
      vTechOut_ELC,
      vStorageIO_ELC
    ), use.names = T, fill = T) |>
    mutate(
      datetime = tsl2dtm(slice, year = year, tmz = "Asia/Kolkata")
    ) |>
    add_tech_type() |>
    # group_by(scenario, te)
    group_by(scenario, name, tech_type, comm, region, year, slice, datetime) |>
    summarise(value = sum(value), .groups = "keep")
  # complete()

  if (return_data) return(vDailyOp)

  vDailyOp |>
    group_by(scenario, region, tech_type, year, datetime) |>
    summarise(value = sum(value), .groups = "keep") |>
    ggplot() +
    # geom_area(aes(datetime, value, fill = tech_type)) +
    geom_col(
      aes(datetime, value, fill = tech_type),
      position = position_stack(reverse = F)) +
    scale_fill_viridis_d(option = "H", direction = 1, name = "Process") +
    theme_bw() +
    labs(x = "", y = "GWh")
}
