# test_LDV_reports.R — generate LDV vehicle reports (HTML + PDF)
library(energyRt)
library(dplyr)
library(rmarkdown)
library(ggplot2)
source("R/install_latex_deps.R")
source("R/ideea_report.r")
source("R/ideea_levcost.R")

# Example 1: Gasoline LDV
LDVG <- newTechnology(
  name = "LDVG", desc = "Gasoline Light Duty Vehicles",
  input  = data.frame(comm = c("GSL", "BIO"), unit = "PJ", group = "i"),
  output = data.frame(comm = c("PLDVHWY", "PLDVCTY"), unit = "MPKm", group = "o"),
  units  = list(capacity = "1000 Vehicles", activity = "million km, city", costs = "MUSD"),
  cap2act = 10,
  ceff = data.frame(
    comm = c("GSL", "BIO", "PLDVHWY", "PLDVCTY"),
    use2cact = c(NA, NA, 3, 2),
    share.up = c(NA, .1, .4, .8),
    cact2cout = c(NA, NA, 2, 3)
  ),
  olife    = list(olife = 10),
  capacity = list(region = NA, year = 2022, stock = 500),
  invcost  = list(invcost = 15),
  fixom    = list(fixom = .5)
)

lc_g <- ideea_levcost(LDVG, group = "o", discount = 0.07, base_year = 2024, verbose = FALSE)

ideea_report(LDVG, type = "vehicle", format = c("html", "pdf"),
  levcost = lc_g,
  image_file = "images/techs/LDV-gasoline.png",
  file = "tmp/LDVG_report")

# Example 2: Electric LDV
LDVE <- newTechnology(
  name = "LDVE", desc = "Electric Light Duty Vehicles",
  input  = data.frame(comm = "ELC", unit = "PJ", group = "i"),
  output = data.frame(comm = c("PLDVHWY", "PLDVCTY"), unit = "MPKm", group = "o"),
  units  = list(capacity = "1000 Vehicles", activity = "million km, city", costs = "MUSD"),
  cap2act = 10,
  ceff = data.frame(
    comm = c("ELC", "PLDVHWY", "PLDVCTY"),
    use2cact = c(NA, 3, 2),
    share.up = c(NA, .4, .8),
    cact2cout = c(NA, 2, 3)
  ),
  olife    = list(olife = 12),
  capacity = list(region = NA, year = 2022, stock = 100),
  invcost  = list(invcost = 25),
  fixom    = list(fixom = .3)
)

lc_e <- ideea_levcost(LDVE, group = "o", discount = 0.07, base_year = 2024, verbose = FALSE)

ideea_report(LDVE, type = "vehicle", format = c("html", "pdf"),
  levcost = lc_e,
  image_file = "images/techs/LDV-electric.png",
  file = "tmp/LDVE_report")
