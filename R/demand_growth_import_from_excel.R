# This script creates data/load_growth_rates_r32.yaml file in semi-auto mode
# (adjustment is required to match the data and r32 mode regions)

library(tidyverse)
library(data.table)
library(sf)
library(yaml)

data_path <- "data/demand_forecast_r32/2024"

# files
ff <- list.files(data_path, pattern = "^Yearly.+xlsx$")
length(ff)

# function to read growth assumptions by scenario
fun_read_growth <- function(f) {
  a <- try(readxl::read_excel(file.path(data_path, f), sheet = "CAGR", range = "A1:D2"))
  if (inherits(a, "try-error")) {
    message("Cannot process file ", f, " check `CAGR` worksheet.")
    return(NULL)
  }
  as.data.table(a)
}

# test
fun_read_growth(ff[1])

# real all files
gg <- lapply(ff, fun_read_growth) |> rbindlist(use.names = T, fill = T) |>
  mutate(
    region_name = if_else(is.na(CAGR), `State/Region`, CAGR)
  ) |>
  select(region_name, 2:4)

yy <- pivot_longer(gg, cols = 2:4) |>
  unique() |>
  pivot_wider(names_from = "region_name")

snames <- yy$name
yy <- as.list(yy[,-1])
rnames <- names(yy)

yml <- lapply(rnames, function(r) {
  a <- yy[[r]]; names(a) <- snames
  as.list(a)
})
names(yml) <- rnames

# write_yaml(yml, "tmp/yml.yaml")
# manually edited the YAML and saved as data/load_growth_rates_r32.yaml




#
# y <-
  get_ideea_map(32) |> st_drop_geometry() |> select(-offshore) |>
  as.data.table()
  # pivot_wider(names_from = reg32, values_from = 2)

# write_yaml(y, "data/reg32.yaml")
