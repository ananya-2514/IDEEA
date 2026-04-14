# test_ideea_levcost.R
# Test ideea_levcost() on the LDVG (gasoline LDV) technology from transport.Rmd
# Run interactively or via source().

library(tidyverse)
library(IDEEA)
# library(energyRt)

source("R/ideea_vehicle.R")
source("R/ideea_levcost.R")
source("R/ideea_report.R")

# ── 1. Rebuild the LDVG technology (from transport.Rmd) ──────────────────────
LDVG <- ideea_vehicle(
  name  = "LDVG",
  desc  = "Gasoline Light Duty Vehicles",
  fuels = c("GSL", "BIO"),
  blend_fuel      = "BIO",
  blend_share_up  = 0.1,
  eff_hwy = 6, eff_cty = 8,
  eff_unit = "L/100km",
  avg_km_yr        = 10000,
  load_per_vehicle_hwy = 2,
  load_per_vehicle_cty = 3,
  share_hwy_up = 0.4,
  share_cty_up = 0.8,
  vehicle_cost_usd = 15000,
  annual_cost_usd  = 500,
  olife      = 10,
  region     = "IND",
  start_year = 2024
)

cat("Technology '", LDVG@name, "' created.\n")
cat("  Inputs : ", paste(LDVG@input$comm, collapse = ", "), "\n")
cat("  Outputs: ", paste(LDVG@output$comm, collapse = ", "), "\n")
cat("  Groups : ", paste(unique(LDVG@output$group), collapse = ", "), "\n")
cat("  olife  : ", LDVG@olife$olife, "years\n")

# ── 2. Quick test: auto-created supplies (zero cost) + verbose messages ───────
cat("\n── Test 1: all auto-created (zero-cost) supplies ──\n")
lc_auto <- ideea_levcost(
  LDVG,
  group     = "o",      # both output commodities (PLDVHWY + PLDVCTY)
  discount  = 0.07,
  base_year = 2024,
  verbose   = TRUE
)

cat("\nLCOE table (per year):\n")
print(lc_auto$levcost)
names(lc_auto)

cat("\nNPV-weighted LCOE:\n")
print(lc_auto$levcost_npv)

# ── 3. Test with real supply costs from transport.Rmd ────────────────────────
cat("\n── Test 2: with real supply costs (GSL = 50, BIO = 70 MUSD/PJ) ──\n")

GSL <- newCommodity(name = "GSL", unit = "PJ", timeframe = "ANNUAL")
BIO <- newCommodity(name = "BIO", unit = "PJ", timeframe = "ANNUAL")

SUP_GSL <- newSupply(
  name = "SUP_GSL",
  commodity = "GSL",
  region = "IND",
  availability = data.frame(
    region = "IND", year = 2024, cost = 50, stringsAsFactors = FALSE
  )
)
SUP_BIO <- newSupply(
  name = "SUP_BIO",
  commodity = "BIO",
  region = "IND",
  availability = data.frame(
    region = "IND", year = 2024, cost = 70, stringsAsFactors = FALSE
  )
)

lc_real <- ideea_levcost(
  LDVG,
  group     = "o",
  repo      = list(GSL, BIO, SUP_GSL, SUP_BIO),
  discount  = 0.07,
  base_year = 2024,
  verbose   = TRUE
)

cat("\nLCOE table (per year):\n")
print(lc_real$levcost)

cat("\nNPV-weighted LCOE:\n")
print(lc_real$levcost_npv)

# ── 4. Test: explicit horizon (5-year milestones) ────────────────────────────
cat("\n── Test 3: explicit 5-year milestone horizon 2024-2049 ──\n")
lc_horizon <- ideea_levcost(
  LDVG,
  group     = "o",
  repo      = list(GSL, BIO, SUP_GSL, SUP_BIO),
  discount  = 0.07,
  horizon   = seq(2024, 2049, by = 5),
  base_year = 2024,
  verbose   = FALSE
)

cat("LCOE table:\n")
print(lc_horizon$levcost)
cat("NPV-weighted LCOE:", lc_horizon$levcost_npv, "\n")

# ── 5. Test: full_output = TRUE ───────────────────────────────────────────────
cat("\n── Test 4: full_output = TRUE ──\n")
scen_lc <- ideea_levcost(
  LDVG,
  group       = "o",
  repo        = list(GSL, BIO, SUP_GSL, SUP_BIO),
  discount    = 0.07,
  base_year   = 2024,
  full_output = TRUE,
  verbose     = FALSE
)

cat("Class of result:", class(scen_lc), "\n")
cat("Status optimal :", scen_lc@status$optimal, "\n")
cat("Stored LCOE table:\n")
print(scen_lc@misc$levcost)
cat("NPV LCOE stored:", scen_lc@misc$levcost_npv, "\n")
cat("Cost breakdown (NPV):\n")
print(scen_lc@misc$cost_breakdown_npv)

# ── 6. Test: multi-group error ───────────────────────────────────────────────
cat("\n── Test 5: multi-group with group = NULL (should error with guidance) ──\n")
# Create a tech with two distinct groups to trigger the error
LDVG_mg <- LDVG
LDVG_mg@output$group <- c("highway", "city")  # force two groups

tryCatch(
  ideea_levcost(LDVG_mg, base_year = 2024, verbose = FALSE),
  error = function(e) cat("Expected error:\n ", conditionMessage(e), "\n")
)

# ── 7. Test: cost breakdown fields ───────────────────────────────────────────
cat("\n── Test 6: cost breakdown structure ──\n")
# lc_real has the most interesting breakdown (non-zero supply costs)
cat("cost_breakdown (first few rows):\n")
print(head(lc_real$cost_breakdown))

cat("\ncost_breakdown_npv (NPV-weighted component totals):\n")
print(lc_real$cost_breakdown_npv)

cat("\nComponents present:", unique(lc_real$cost_breakdown$component), "\n")

# ── 8. Test: per-activity LCOE ────────────────────────────────────────────────
# 'group = "o"' triggers has_grouped_output → levcost_per_act is populated.
cat("\n── Test 7: per-activity LCOE (cost per unit of vTechAct) ──\n")
if (!is.null(lc_real$levcost_per_act)) {
  cat("levcost_per_act:\n")
  print(lc_real$levcost_per_act)
} else {
  cat("levcost_per_act is NULL (vTechAct not available in solution)\n")
}

# ── 9. Test: overnight_cost = TRUE ───────────────────────────────────────────
cat("\n── Test 8: overnight_cost = TRUE adds 'overnight_inv' component ──\n")
lc_overnight <- ideea_levcost(
  LDVG,
  group          = "o",
  repo           = list(GSL, BIO, SUP_GSL, SUP_BIO),
  discount       = 0.07,
  base_year      = 2024,
  overnight_cost = TRUE,
  verbose        = FALSE
)
cat("Components with overnight_cost:\n")
print(unique(lc_overnight$cost_breakdown$component))
cat("cost_breakdown_npv (overnight):\n")
print(lc_overnight$cost_breakdown_npv)

# ── 10. Test: list of technologies ───────────────────────────────────────────
cat("\n── Test 9: list of technologies ──\n")
LDVE <- ideea_vehicle(
  name  = "LDVE",
  desc  = "Battery Electric Light Duty Vehicles",
  fuels = "ELC",
  eff_hwy = 20, eff_cty = 25,
  eff_unit = "kWh/100km",
  avg_km_yr        = 10000,
  load_per_vehicle_hwy = 2,
  load_per_vehicle_cty = 3,
  share_hwy_up = 0.4,
  share_cty_up = 0.8,
  vehicle_cost_usd = 25000,
  annual_cost_usd  = 300,
  olife      = 10,
  region     = "IND",
  start_year = 2024
)

ELC <- newCommodity(name = "ELC", unit = "PJ", timeframe = "ANNUAL")
SUP_ELC <- newSupply(
  name = "SUP_ELC",
  commodity = "ELC",
  region = "IND",
  availability = data.frame(
    region = "IND", year = 2024, cost = 20, stringsAsFactors = FALSE
  )
)

lc_list <- ideea_levcost(
  list(LDVG, LDVE),
  group     = "o",
  repo      = list(GSL, BIO, ELC, SUP_GSL, SUP_BIO, SUP_ELC),
  discount  = 0.07,
  base_year = 2024,
  verbose   = FALSE
)

cat("Class:", class(lc_list), "\n")
cat("Technologies evaluated:", names(lc_list), "\n")
cat("NPV LCOE per technology:\n")
print(sapply(lc_list, function(x) x$levcost_npv))

# ── 11. autoplot examples ─────────────────────────────────────────────────────
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cat("\n── Test 10: autoplot examples ──\n")

  # 10a. Single technology – stacked components by year (default)
  p1 <- autoplot(lc_real)
  print(p1)
  cat("p1: per-year stacked component bars with NPV line\n")

  # 10b. Single technology – NPV-weighted component bar chart
  p2 <- autoplot(lc_real, type = "npv")
  print(p2)
  cat("p2: NPV-discounted component breakdown (horizontal bars)\n")

  # 10c. Single technology – simple total bar chart
  p3 <- autoplot(lc_real, type = "totals")
  print(p3)
  cat("p3: per-year total LCOE bars\n")

  # 10d. Single technology – overnight_cost variant showing both EAC + overnight
  p4 <- autoplot(lc_overnight, type = "npv")
  print(p4)
  cat("p4: NPV components including 'overnight_inv'\n")

  # 10e. List of technologies – NPV comparison bar chart
  p5 <- autoplot(lc_list, type = "npv")
  print(p5)
  cat("p5: technology comparison (NPV LCOE)\n")

  # 10f. List of technologies – faceted stacked component bars
  p6 <- autoplot(lc_list, type = "components")
  print(p6)
  cat("p6: faceted per-year component breakdown per technology\n")

  # 10g. List of technologies – side-by-side total bars
  p7 <- autoplot(lc_list, type = "totals")
  print(p7)
  cat("p7: side-by-side LCOE per year\n")
} else {
  cat("ggplot2 not available – skipping autoplot tests.\n")
}

# ── 12. Production frontier (grouped-output technology) ───────────────────────
# The dual-mode vehicle (PLDVHWY / PLDVCTY) has share.up bounds, so the
# production frontier analysis runs automatically (frontier = TRUE by default).
# lc_real already has this enabled; just demonstrate the new outputs here.

cat("\n── Test 11: production frontier ──\n")

# 11a. Inspect frontier table (one row per scenario × year)
cat("\nfrontier data frame:\n")
print(lc_real$frontier)

# 11b. NPV columns differ by output metric when frontier data are available
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p_fe <- autoplot(lc_real, type = "npv")
  print(p_fe)
  cat("p_fe: NPV columns – combined, per-PLDVHWY (max share), per-PLDVCTY (max share)\n")

  library(ggplot2)
  # 11c. 2D production frontier plot
  p_fr <- autoplot(lc_real, type = "frontier") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))
  print(p_fr)
  cat("p_fr: production frontier – PLDVHWY vs PLDVCTY\n")

  # 11d. Frontier for a technology WITHOUT share bounds should return NULL gracefully
  lc_no_frontier <- ideea_levcost(LDVG, comm = "PLDVHWY", group = "o",
                                  repo = list(GSL, BIO, ELC, SUP_GSL, SUP_BIO, SUP_ELC),
                                  discount = 0.1,
                                  base_year = 2020, horizon = 5,
                                  frontier = FALSE)   # explicitly disabled
  p_fr_null <- autoplot(lc_no_frontier, type = "frontier")
  cat("p_fr_null: NULL (no frontier data) – message expected\n")
}

