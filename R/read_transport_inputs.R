# read_transport_inputs.R
# Functions to read transport_inputs.xlsx and build energyRt model objects.
#
# Usage:
#   source("R/read_transport_inputs.R")
#   tabs  <- read_transport_inputs("data/transport_inputs.xlsx")
#   comms <- build_ldv_commodities(tabs)
#   techs <- build_ldv_technologies(tabs, fuel_energy = ldv_fuel_energy)
#   fleet <- build_ldv_fleet(techs, tabs)
#   sup   <- build_ldv_supply(tabs)
#   dems  <- build_ldv_demands(tabs)
#
# Requires: readxl, energyRt (for newCommodity / newSupply / newDemand),
#           ideea_vehicle.R (sourced from same R/ directory)

# ── 1. Read all tabs ─────────────────────────────────────────────────────────

#' Read all tabs from transport_inputs.xlsx into a named list of data.frames.
#'
#' @param xls_path Path to transport_inputs.xlsx.
#' @return Named list; names match the sheet names in the workbook.
read_transport_inputs <- function(xls_path = "data/transport_inputs.xlsx") {
  if (!file.exists(xls_path))
    stop("Excel file not found: ", xls_path)
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' is required: install.packages('readxl')")

  sheets <- readxl::excel_sheets(xls_path)
  tabs   <- lapply(sheets, function(s) {
    as.data.frame(readxl::read_excel(xls_path, sheet = s, na = c("", "NA")))
  })
  names(tabs) <- sheets
  tabs
}

# ── 1b. Generic repository builder ──────────────────────────────────────────

#' Wrap a named list of energyRt objects into a newRepository.
#'
#' @param name  Name of the repository (character).
#' @param objs  Named list of energyRt objects to add.
#' @return A \code{repository} object.
build_repository <- function(name, objs) {
  repo <- newRepository(name)
  Reduce(add, objs, repo)
}

# ── 2. Commodities ───────────────────────────────────────────────────────────

#' Build a repository of newCommodity() objects from the commodities tab.
#'
#' Supports emis_CO2 and emis_PM columns; NA values are silently skipped.
#'
#' @param tabs Named list returned by read_transport_inputs().
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a
#'   named list.
#' @return A \code{repository} or named list of newCommodity objects.
build_ldv_commodities <- function(tabs, branch = NULL, as_repo = TRUE) {
  df <- tabs[["commodities"]]
  if (is.null(df)) stop("'commodities' sheet not found in transport inputs.")

  # Filter by branch: include the requested branch's service comms + all shared fuels.
  # branch=NULL returns all rows (backward-compatible).
  if (!is.null(branch) && "branch" %in% names(df)) {
    df <- df[df[["branch"]] %in% c(branch, "shared"), , drop = FALSE]
  }

  comm_list <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]

    # Build emissions data.frame from non-NA emis columns
    emis_vals <- c(CO2 = r[["emis_CO2"]], PM = r[["emis_PM"]])
    emis_vals <- emis_vals[!is.na(emis_vals)]

    emis_df <- if (length(emis_vals) > 0) {
      data.frame(comm = names(emis_vals), emis = unname(emis_vals),
                 stringsAsFactors = FALSE)
    } else NULL

    misc_data <- list()
    if ("energy_content" %in% names(r) && !is.na(r[["energy_content"]])) {
      misc_data$energy_content <- as.numeric(r[["energy_content"]])
      misc_data$energy_unit    <- r[["energy_unit"]]
    }

    newCommodity(
      name      = r[["name"]],
      desc      = if (!is.na(r[["desc"]])) r[["desc"]] else "",
      unit      = r[["unit"]],
      timeframe = if (!is.na(r[["timeframe"]])) r[["timeframe"]] else "ANNUAL",
      emis      = emis_df,
      misc      = misc_data
    )
  })
  names(comm_list) <- df[["name"]]
  if (as_repo) build_repository("repo_comm", comm_list) else comm_list
}

# ── 3. Demands ───────────────────────────────────────────────────────────────

#' Build a repository of newDemand() objects from the demand tab.
#'
#' Year columns (y2025…y2050) are read as numeric. NA values (or formula
#' cells not yet filled) are silently skipped — newDemand() handles sparse data.
#'
#' @param tabs Named list returned by read_transport_inputs().
#' @param pattern Optional regex pattern applied to the `name` column to select
#'   a subset of demand rows. E.g. \code{"PLDV"} for passenger only,
#'   \code{"FLDV"} for freight only. Default \code{NULL} returns all rows.
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a
#'   named list.
#' @return A \code{repository} or named list of newDemand objects.
build_ldv_demands <- function(tabs, pattern = NULL, branch = NULL, as_repo = TRUE) {
  df   <- tabs[["demand"]]
  if (is.null(df)) stop("'demand' sheet not found in transport inputs.")

  if (!is.null(branch) && "branch" %in% names(df)) {
    # branch column takes priority over pattern
    df <- df[df[["branch"]] == branch, , drop = FALSE]
    if (nrow(df) == 0) stop("No demand rows with branch == '", branch, "'.")
  } else if (!is.null(pattern)) {
    df <- df[grepl(pattern, df[["name"]], ignore.case = TRUE), , drop = FALSE]
    if (nrow(df) == 0) stop("No demand rows match pattern '", pattern, "'.")
  }

  yr_cols <- grep("^y[0-9]{4}$", names(df), value = TRUE)

  dem_list <- lapply(seq_len(nrow(df)), function(i) {
    r <- df[i, ]

    # Build year-value pairs, dropping NAs
    yr_vals <- as.numeric(r[yr_cols])
    yrs     <- as.integer(sub("^y", "", yr_cols))
    keep    <- !is.na(yr_vals)

    dem_df <- if (any(keep)) {
      data.frame(
        region = r[["region"]],
        year   = yrs[keep],
        dem    = yr_vals[keep],
        stringsAsFactors = FALSE
      )
    } else NULL

    newDemand(
      name      = r[["name"]],
      desc      = if (!is.na(r[["desc"]])) r[["desc"]] else "",
      commodity = r[["commodity"]],
      unit      = r[["unit"]],
      dem       = dem_df
    )
  })
  names(dem_list) <- df[["name"]]
  if (as_repo) build_repository("repo_dem", dem_list) else dem_list
}

# ── 4. Supply ────────────────────────────────────────────────────────────────

#' Build a repository of newSupply() objects from the supply and infrastructure
#' tabs.
#'
#' Cost columns (y2025…y2050) supply time-varying availability cost (MUSD/PJ).
#' Rows where all year columns are NA are created with no availability data
#' (placeholder objects).
#'
#' @param tabs      Named list returned by read_transport_inputs().
#' @param base_year Integer; year used when only one cost value is available and
#'   no year column is present.
#' @param as_repo   Logical; if TRUE (default) return a newRepository, else a
#'   named list.
#' @return A \code{repository} or named list of newSupply objects (supply tab
#'   first, then infrastructure tab if present).
build_ldv_supply <- function(tabs, base_year = 2025L, as_repo = TRUE) {
  .build_one_supply_tab <- function(df) {
    yr_cols     <- grep("^y[0-9]{4}$",         names(df), value = TRUE)
    ava_up_cols <- grep("^ava_up_PJ_[0-9]{4}$", names(df), value = TRUE)
    lapply(seq_len(nrow(df)), function(i) {
      r      <- df[i, ]
      region <- as.character(r[["region"]])   # coerce NA_logical → NA_character

      cost_vals  <- as.numeric(r[yr_cols])
      cost_yrs   <- as.integer(sub("^y", "", yr_cols))

      ava_up_vals <- as.numeric(r[ava_up_cols])
      ava_up_yrs  <- as.integer(sub("^ava_up_PJ_", "", ava_up_cols))

      # Union of years that have either a cost or an ava.up value
      cost_have  <- cost_yrs[!is.na(cost_vals)]
      avup_have  <- ava_up_yrs[!is.na(ava_up_vals)]
      all_yrs    <- sort(union(cost_have, avup_have))

      ava_df <- if (length(all_yrs) > 0) {
        cv <- setNames(cost_vals,   as.character(cost_yrs))
        av <- setNames(ava_up_vals, as.character(ava_up_yrs))
        data.frame(
          region = region,
          year   = all_yrs,
          cost   = as.numeric(cv[as.character(all_yrs)]),
          ava.up = as.numeric(av[as.character(all_yrs)]),
          stringsAsFactors = FALSE
        )
      } else NULL

      newSupply(
        name         = r[["name"]],
        commodity    = r[["commodity"]],
        region       = region,
        availability = ava_df
      )
    })
  }

  sup <- .build_one_supply_tab(tabs[["supply"]])
  inf <- if (!is.null(tabs[["infrastructure"]])) {
    .build_one_supply_tab(tabs[["infrastructure"]])
  } else list()

  all_sup <- c(sup, inf)
  names(all_sup) <- c(tabs[["supply"]][["name"]],
                      if (!is.null(tabs[["infrastructure"]]))
                        tabs[["infrastructure"]][["name"]])
  if (as_repo) build_repository("repo_sup", all_sup) else all_sup
}

# ── 5. Technologies ──────────────────────────────────────────────────────────

#' Build ideea_vehicle() objects from the technologies tab.
#'
#' Maps the flat Excel columns to ideea_vehicle() arguments:
#'   eff_hwy / eff_cty / eff_unit  → fuel efficiency
#'   invcost_2025                   → vehicle_cost_usd (base year)
#'   fixom_2025                     → annual_cost_usd  (base year)
#'   olife / avg_km_yr / load_*    → vehicle physics
#'   fuels / primary_fuel / blend_* → fuel structure
#'
#' Year-varying invcost_20** and fixom_20** columns are preserved in the
#' returned list as attribute "invcost_ts" / "fixom_ts" for future use when
#' ideea_vehicle() gains explicit time-varying cost support.
#'
#' @param tabs         Named list returned by read_transport_inputs().
#' @param fuel_energy  Named numeric vector of energy content (MJ/L or MJ/kg);
#'   passed straight to ideea_vehicle().
#' @param service_hwy  Highway service commodity name (default "PLDVHWY").
#' @param service_cty  City service commodity name (default "PLDVCTY").
#' @param start_year   Integer; technology availability start year applied
#'   to all technologies (default 2025).
#' @param as_repo      Logical; if TRUE (default) return a newRepository, else
#'   a named list.
#' @return A \code{repository} or named list of ideea_vehicle objects with
#'   attributes "invcost_ts" and "fixom_ts".
build_ldv_technologies <- function(tabs,
                                    fuel_energy  = c(),
                                    service_hwy  = "PLDVHWY",
                                    service_cty  = "PLDVCTY",
                                    branch       = NULL,
                                    start_year   = 2025L,
                                    as_repo      = TRUE) {
  # browser()
  df <- tabs[["technologies"]]
  if (is.null(df)) stop("'technologies' sheet not found in transport inputs.")

  # Derive MUSD → cr. INR conversion factor from the settings tab
  # Key: musd_to_crinr (direct factor, e.g. 8.35 means 1 MUSD = 8.35 cr. INR).
  # Falls back to old key usd_to_crinr/10 for backward compatibility.
  settings_df   <- tabs[["settings"]]
  musd_to_crinr <- if (!is.null(settings_df) && "key" %in% names(settings_df)) {
    rate <- settings_df$value[settings_df$key == "musd_to_crinr"]
    if (length(rate) == 1 && !is.na(rate)) {
      as.numeric(rate)
    } else {
      rate2 <- settings_df$value[settings_df$key == "usd_to_crinr"]
      if (length(rate2) == 1 && !is.na(rate2)) as.numeric(rate2) / 10 else 1
    }
  } else 1

  # Auto-derive fuel_energy from commodities tab when not explicitly provided.
  # Builds a named numeric vector from energy_content column (NA rows skipped).
  if (length(fuel_energy) == 0) {
    comm_df <- tabs[["commodities"]]
    if (!is.null(comm_df) && "energy_content" %in% names(comm_df)) {
      has_val    <- !is.na(comm_df[["energy_content"]])
      fuel_energy <- stats::setNames(as.numeric(comm_df[["energy_content"]][has_val]),
                                     comm_df[["name"]][has_val])
    }
  }

  # Filter by branch when column exists
  if (!is.null(branch) && "branch" %in% names(df)) {
    df <- df[df[["branch"]] == branch, , drop = FALSE]
    if (nrow(df) == 0) stop("No technology rows with branch == '", branch, "'.")
  }

  # Check for per-row service commodity columns (added in multi-branch build)
  has_service_cols <- all(c("service_hwy", "service_cty") %in% names(df))

  parse_fuels <- function(x) trimws(strsplit(x, "\\|")[[1]])

  # Identify invcost / fixom year columns (X_20** pattern after read_excel)
  inv_cols  <- sort(grep("^invcost_[0-9]{4}$", names(df), value = TRUE))
  fixom_cols <- sort(grep("^fixom_[0-9]{4}$",  names(df), value = TRUE))

  tech_list <- lapply(seq_len(nrow(df)), function(i) {
    p <- df[i, ]

    fuels      <- parse_fuels(p[["fuels"]])
    blend_fuel <- if (!is.na(p[["blend_fuel"]])) p[["blend_fuel"]] else NULL

    # Base-year cost values (USD/vehicle)
    invcost_base <- if ("invcost_2025" %in% names(p)) as.numeric(p[["invcost_2025"]]) else NULL
    fixom_base   <- if ("fixom_2025"   %in% names(p)) as.numeric(p[["fixom_2025"]])   else NULL

    eff_start_yr <- start_year

    # Get service commodities: per-row column takes priority over function param
      svc_hwy <- if (has_service_cols && !is.na(p[["service_hwy"]])) p[["service_hwy"]] else service_hwy
      svc_cty <- if (has_service_cols && !is.na(p[["service_cty"]])) p[["service_cty"]] else service_cty

      tech_obj <- ideea_vehicle(
      name          = p[["name"]],
      desc          = if (!is.na(p[["desc"]])) p[["desc"]] else "",
      fuels         = fuels,
      primary_fuel  = p[["primary_fuel"]],
      blend_fuel    = blend_fuel,
      blend_share_up = if (!is.na(p[["blend_share_up"]])) as.numeric(p[["blend_share_up"]]) else 0.1,

      service_hwy   = svc_hwy,
      service_cty   = svc_cty,

      eff_hwy       = as.numeric(p[["eff_hwy"]]),
      eff_cty       = as.numeric(p[["eff_cty"]]),
      eff_unit      = p[["eff_unit"]],
      fuel_energy   = fuel_energy,

      avg_km_yr            = as.numeric(p[["avg_km_yr"]]),
      capacity_units       = as.numeric(p[["capacity_units"]]),
      load_per_vehicle_hwy = as.numeric(p[["load_per_vehicle_hwy"]]),
      load_per_vehicle_cty = as.numeric(p[["load_per_vehicle_cty"]]),
      share_hwy_up         = as.numeric(p[["share_hwy_up"]]),
      share_cty_up         = as.numeric(p[["share_cty_up"]]),

      vehicle_cost_usd = invcost_base,
      annual_cost_usd  = fixom_base,

      olife      = as.integer(p[["olife"]]),
      region     = if (!is.na(p[["region"]])) p[["region"]] else NA_character_,
      start_year = eff_start_yr
    )

    # Attach time-varying cost series as attributes for future use
    cap_units <- as.numeric(p[["capacity_units"]])
    if (length(inv_cols) > 0) {
      yrs <- as.integer(sub("^invcost_", "", inv_cols))
      vals <- as.numeric(p[inv_cols]) * cap_units / 1e6 * musd_to_crinr  # USD/veh → cr.INR/1000veh
      attr(tech_obj, "invcost_ts") <- data.frame(year = yrs, value_crinr = vals)
    }
    if (length(fixom_cols) > 0) {
      yrs <- as.integer(sub("^fixom_", "", fixom_cols))
      vals <- as.numeric(p[fixom_cols]) * cap_units / 1e6 * musd_to_crinr
      attr(tech_obj, "fixom_ts") <- data.frame(year = yrs, value_crinr = vals)
    }

    tech_obj
  })
  names(tech_list) <- df[["name"]]
  if (as_repo) build_repository("repo_tech", tech_list) else tech_list
}

.split_infra_values <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))

  x <- as.character(x[[1]])
  if (is.na(x) || !nzchar(trimws(x))) return(character(0))

  trimws(strsplit(x, "\\s*[,|]\\s*")[[1]])
}

.get_infra_input_spec <- function(row) {
  input_comms <- if ("input_comms" %in% names(row)) {
    .split_infra_values(row[["input_comms"]])
  } else {
    character(0)
  }
  if (length(input_comms) == 0 && "input_comm" %in% names(row) &&
      !is.na(row[["input_comm"]])) {
    input_comms <- as.character(row[["input_comm"]])
  }

  input_units <- if ("input_units" %in% names(row)) {
    .split_infra_values(row[["input_units"]])
  } else {
    character(0)
  }
  if (length(input_units) == 0 && "input_unit" %in% names(row) &&
      !is.na(row[["input_unit"]])) {
    input_units <- rep(as.character(row[["input_unit"]]), length(input_comms))
  } else if (length(input_units) == 1 && length(input_comms) > 1) {
    input_units <- rep(input_units, length(input_comms))
  }

  if (length(input_comms) != length(input_units)) {
    stop("infra_tech row '", row[["name"]], "' has mismatched input commodity metadata.")
  }

  input_shares <- if ("input_shares" %in% names(row)) {
    as.numeric(.split_infra_values(row[["input_shares"]]))
  } else {
    numeric(0)
  }
  if (length(input_shares) == 0) {
    input_shares <- rep(NA_real_, length(input_comms))
  }
  if (length(input_shares) != length(input_comms)) {
    stop("infra_tech row '", row[["name"]], "' has mismatched input shares.")
  }

  aux_outputs <- if ("input_to_aux" %in% names(row)) {
    .split_infra_values(row[["input_to_aux"]])
  } else {
    character(0)
  }
  if (length(aux_outputs) > 0 && length(aux_outputs) != length(input_comms)) {
    stop("infra_tech row '", row[["name"]], "' has mismatched auxiliary output mapping.")
  }

  list(
    comms = input_comms,
    units = input_units,
    shares = input_shares,
    aux_outputs = aux_outputs
  )
}

.petro_pump_defaults <- function() {
  liters_per_gallon <- 3.785411784
  acre_to_kha <- 0.00040468564224
  gasoline_share <- 0.9
  diesel_share <- 0.1
  weighted_mj_per_liter <- gasoline_share * 34.2 + diesel_share * 36.9
  weighted_mj_per_gallon <- weighted_mj_per_liter * liters_per_gallon
  service_cost_usd_per_gallon <- mean(c(0.10, 0.20))

  list(
    throughput_pj_per_station = 1.5e6 * weighted_mj_per_gallon / 1e9,
    land_kha_per_station = acre_to_kha,
    invcost_musd = 4.0,
    fixom_musd = mean(c(0.1, 0.3)),
    varom_musd_per_pj = service_cost_usd_per_gallon *
      (1e9 / weighted_mj_per_gallon) / 1e6
  )
}

.normalize_infra_tech_df <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  if (!"desc" %in% names(df))
    df[["desc"]] <- rep(NA_character_, nrow(df))
  if (!"input_comms" %in% names(df))
    df[["input_comms"]] <- if ("input_comm" %in% names(df)) as.character(df[["input_comm"]]) else rep(NA_character_, nrow(df))
  if (!"input_units" %in% names(df))
    df[["input_units"]] <- if ("input_unit" %in% names(df)) as.character(df[["input_unit"]]) else rep(NA_character_, nrow(df))
  if (!"input_shares" %in% names(df))
    df[["input_shares"]] <- rep(NA_character_, nrow(df))
  if (!"input_to_aux" %in% names(df))
    df[["input_to_aux"]] <- rep(NA_character_, nrow(df))
  if (!"cact2cout" %in% names(df))
    df[["cact2cout"]] <- rep(NA_real_, nrow(df))
  if (!"varom" %in% names(df))
    df[["varom"]] <- rep(NA_real_, nrow(df))
  if (!"cinp_comms" %in% names(df))
    df[["cinp_comms"]] <- rep(NA_character_, nrow(df))
  if (!"cinp2ainp_ratio" %in% names(df))
    df[["cinp2ainp_ratio"]] <- rep(NA_real_, nrow(df))
  if (!"land_per_kunit" %in% names(df))
    df[["land_per_kunit"]] <- rep(NA_real_, nrow(df))
  if (!"notes" %in% names(df))
    df[["notes"]] <- rep(NA_character_, nrow(df))

  inv_cols <- grep("^invcost_[0-9]{4}$", names(df), value = TRUE)
  if (length(inv_cols) == 0) {
    df[["invcost_2025"]] <- rep(NA_real_, nrow(df))
    inv_cols <- "invcost_2025"
  }
  fixom_cols <- grep("^fixom_[0-9]{4}$", names(df), value = TRUE)
  if (length(fixom_cols) == 0) {
    df[["fixom_2025"]] <- rep(NA_real_, nrow(df))
    fixom_cols <- "fixom_2025"
  }

  petro_row <- match("INFR_PETRO_REFUEL", df[["name"]])
  if (!is.na(petro_row)) {
    pump <- .petro_pump_defaults()

    df[["desc"]][petro_row] <- "Pump station with fixed gasoline/diesel throughput mix"
    df[["input_comm"]][petro_row] <- "MKT_GSL"
    df[["input_unit"]][petro_row] <- "PJ"
    df[["input_comms"]][petro_row] <- "MKT_GSL,MKT_DSL"
    df[["input_units"]][petro_row] <- "PJ,PJ"
    df[["input_shares"]][petro_row] <- "0.9,0.1"
    df[["input_to_aux"]][petro_row] <- "GSL,DSL"
    df[["cact2cout"]][petro_row] <- 1
    df[["cap2act"]][petro_row] <- pump$throughput_pj_per_station
    df[["land_per_kunit"]][petro_row] <- pump$land_kha_per_station
    df[["cinp_comms"]][petro_row] <- "GSL,DSL,BIO,GSL10"
    df[["cinp2ainp_ratio"]][petro_row] <- 1
    df[["varom"]][petro_row] <- pump$varom_musd_per_pj

    for (col in inv_cols) df[[col]][petro_row] <- pump$invcost_musd
    for (col in fixom_cols) df[[col]][petro_row] <- pump$fixom_musd

    stock_cols <- grep("^stock_[0-9]{4}$", names(df), value = TRUE)
    for (col in stock_cols) {
      val <- as.numeric(df[[col]][petro_row])
      if (!is.na(val)) {
        df[[col]][petro_row] <- val / pump$throughput_pj_per_station
      }
    }

    df[["notes"]][petro_row] <- paste(
      "Hardcoded pump-station override:",
      "$4M capex excluding land,",
      "$0.2M/yr fixom excluding c-store,",
      "$0.15/gal service cost,",
      "1.5M gal/yr throughput, 1 acre land."
    )
  }

  df
}

.normalize_transport_tabs <- function(tabs) {
  if (is.null(tabs[["infra_tech"]])) return(tabs)

  tabs[["infra_tech"]] <- .normalize_infra_tech_df(tabs[["infra_tech"]])
  tabs
}

# ── 5b. Infrastructure as Technology ─────────────────────────────────────────────────

#' Build newTechnology objects from the infra_tech tab.
#'
#' Each row becomes one technology with a MKT_* market commodity as input and a
#' refueling/charging service commodity (PJ) as output. Physical activity units
#' (kt for fuel, GWh for EV) are used; cact2cout converts activity to PJ output.
#' LAND use is capacity-linked via cap2ainp in @aeff (cap2ainp = land_per_kunit *
#' cap2act, because the LP equation is: vTechAInp = Cap * cap2ainp / cap2act).
#' Costs from invcost_YYYY / fixom_YYYY / varom columns are activated.
#'
#' @param tabs    Named list from read_transport_inputs().
#' @param branch  Optional character; when non-NULL only rows whose 'branch'
#'   column (comma-separated) contains this branch are included.
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a list.
build_ldv_infra_techs <- function(tabs, branch = NULL, as_repo = TRUE) {
  tabs <- .normalize_transport_tabs(tabs)
  df <- tabs[["infra_tech"]]
  if (is.null(df)) {
    warning("infra_tech sheet not found in tabs — returning empty.")
    return(if (as_repo) newRepository("repo_infra_tech") else list())
  }

  # Filter to rows applicable to this branch (branch column = comma-sep list)
  if (!is.null(branch) && "branch" %in% names(df)) {
    keep <- vapply(df[["branch"]], function(b) {
      !is.na(b) && branch %in% trimws(strsplit(b, ",")[[1]])
    }, logical(1))
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0)
    return(if (as_repo) newRepository("repo_infra_tech") else list())

  inv_cols   <- sort(grep("^invcost_[0-9]{4}$", names(df), value = TRUE))
  fixom_cols <- sort(grep("^fixom_[0-9]{4}$",  names(df), value = TRUE))
  tech_list <- lapply(seq_len(nrow(df)), function(i) {
    p <- df[i, , drop = FALSE]
    input_spec <- .get_infra_input_spec(p)
    input_comms <- input_spec$comms
    input_units <- input_spec$units
    input_shares <- input_spec$shares
    aux_outputs <- input_spec$aux_outputs

    if (length(input_comms) == 0) {
      stop("infra_tech row '", p[["name"]], "' has no input commodities.")
    }

    cap2act_val   <- as.numeric(p[["cap2act"]])
    cact2cout_val <- if ("cact2cout" %in% names(p) && !is.na(p[["cact2cout"]]))
      as.numeric(p[["cact2cout"]]) else 1

    # @input: MKT_* market commodity, combustion = 0 (no direct emissions)
    inp <- data.frame(
      comm       = input_comms,
      unit       = input_units,
      combustion = 0,
      stringsAsFactors = FALSE
    )
    if (length(input_comms) > 1) inp$group <- "i"

    # @output: service commodity (PJ)
    out <- data.frame(
      comm = p[["output_comm"]],
      unit = p[["output_unit"]],
      stringsAsFactors = FALSE
    )

    # @ceff: input row (cinp2use = 1: 1 physical unit in per unit activity)
    #        output row (use2cact = 1, cact2cout converts kt/GWh → PJ)
    if (length(input_comms) > 1) {
      ceff_rows <- list(
        data.frame(
          comm = input_comms,
          cinp2ginp = 1,
          share.fx = input_shares,
          stringsAsFactors = FALSE
        ),
        data.frame(
          comm = p[["output_comm"]],
          use2cact = 1,
          cact2cout = cact2cout_val,
          stringsAsFactors = FALSE
        )
      )
      geff_arg <- data.frame(group = "i", ginp2use = 1, stringsAsFactors = FALSE)
    } else {
      ceff_rows <- list(
        data.frame(
          comm = input_comms,
          cinp2use = 1,
          share.fx = input_shares,
          stringsAsFactors = FALSE
        ),
        data.frame(
          comm = p[["output_comm"]],
          use2cact = 1,
          cact2cout = cact2cout_val,
          stringsAsFactors = FALSE
        )
      )
      geff_arg <- NULL
    }
    ceff_arg <- do.call(dplyr::bind_rows, ceff_rows)

    # @aeff LAND row: cap2ainp = land_per_kunit * cap2act
    #   LP: vTechAInp_LAND = Cap * cap2ainp / cap2act => cap2ainp/cap2act = land/capacity_unit
    aeff_rows <- list()
    if ("land_per_kunit" %in% names(p) && !is.na(p[["land_per_kunit"]])) {
      land_per_ku <- as.numeric(p[["land_per_kunit"]])
      aeff_rows <- c(aeff_rows, list(
        data.frame(acomm = "LAND", cap2ainp = land_per_ku * cap2act_val,
                   stringsAsFactors = FALSE)
      ))
    }
    # Fugitive CH4 leakage via aeff act2aout (CNG stations only)
    if ("cng_leak" %in% names(p) && !is.na(p[["cng_leak"]])) {
      aeff_rows <- c(aeff_rows, list(
        data.frame(acomm = "CNG_LEAK", act2aout = as.numeric(p[["cng_leak"]]),
                   stringsAsFactors = FALSE)
      ))
    }
    if (length(aux_outputs) > 0) {
      for (j in seq_along(aux_outputs)) {
        aeff_rows <- c(aeff_rows, list(
          data.frame(
            acomm = aux_outputs[[j]],
            comm = input_comms[[j]],
            cinp2aout = 1,
            stringsAsFactors = FALSE
          )
        ))
      }
    }
    aeff_arg <- if (length(aeff_rows) > 0)
      do.call(dplyr::bind_rows, aeff_rows) else NULL

    # @aux: declare LAND, retail fuel outputs, and/or CNG_LEAK
    aux_rows <- list()
    if (!is.null(aeff_arg) && "LAND" %in% aeff_arg[["acomm"]])
      aux_rows <- c(aux_rows, list(data.frame(acomm = "LAND", unit = "kha",
                                              stringsAsFactors = FALSE)))
    if (!is.null(aeff_arg) && "CNG_LEAK" %in% aeff_arg[["acomm"]])
      aux_rows <- c(aux_rows, list(data.frame(acomm = "CNG_LEAK", unit = "PJ",
                                              stringsAsFactors = FALSE)))
    if (length(aux_outputs) > 0) {
      for (j in seq_along(aux_outputs)) {
        aux_rows <- c(aux_rows, list(data.frame(
          acomm = aux_outputs[[j]],
          unit = input_units[[j]],
          stringsAsFactors = FALSE
        )))
      }
    }
    aux_arg <- if (length(aux_rows) > 0) unique(do.call(dplyr::bind_rows, aux_rows)) else NULL

    # Time-varying invcost from invcost_YYYY columns
    inv_arg <- if (length(inv_cols) > 0) {
      yrs  <- as.integer(sub("^invcost_", "", inv_cols))
      vals <- as.numeric(unlist(p[inv_cols]))
      keep <- !is.na(vals)
      if (any(keep)) data.frame(region = NA_character_, year = yrs[keep],
                                invcost = vals[keep]) else NULL
    } else NULL

    # Time-varying fixom from fixom_YYYY columns
    fix_arg <- if (length(fixom_cols) > 0) {
      yrs  <- as.integer(sub("^fixom_", "", fixom_cols))
      vals <- as.numeric(unlist(p[fixom_cols]))
      keep <- !is.na(vals)
      if (any(keep)) data.frame(region = NA_character_, year = yrs[keep],
                                fixom = vals[keep]) else NULL
    } else NULL

    # Variable O&M scalar
    varom_arg <- if ("varom" %in% names(p) && !is.na(p[["varom"]]))
      as.numeric(p[["varom"]]) else NULL

    args <- list(
      name    = p[["name"]],
      desc    = if (!is.na(p[["desc"]])) p[["desc"]] else "",
      input   = inp,
      output  = out,
      ceff    = ceff_arg,
      cap2act = cap2act_val,
      olife   = list(olife = as.integer(p[["olife"]]))
    )
    if (!is.null(geff_arg))  args$geff    <- geff_arg
    if (!is.null(aeff_arg))  args$aeff    <- aeff_arg
    if (!is.null(aux_arg))   args$aux     <- aux_arg
    if (!is.null(inv_arg))   args$invcost <- inv_arg
    if (!is.null(fix_arg))   args$fixom   <- fix_arg
    if (!is.null(varom_arg)) args$varom   <- varom_arg
    do.call(newTechnology, args)
  })
  names(tech_list) <- df[["name"]]
  if (as_repo) Reduce(add, tech_list, newRepository("repo_infra_tech"))
  else tech_list
}

# ── 5b-ii. Infrastructure fleet stock ─────────────────────────────────────────

#' Build newCapacity (stock) objects for infrastructure techs.
#'
#' Reads stock_YYYY columns from the infra_tech tab and returns a list of
#' technology objects with @capacity populated. Analogous to build_ldv_fleet()
#' for vehicle techs.
#'
#' @param tech_list Named list of INFR_* technology objects (from
#'   build_ldv_infra_techs(..., as_repo = FALSE)).
#' @param tabs      Named list from read_transport_inputs().
#' @param branch    Optional character; when non-NULL only rows whose 'branch'
#'   column contains this branch are used.
#' @return Updated named list of technology objects.
build_ldv_infra_fleet <- function(tech_list, tabs, branch = NULL) {
  tabs <- .normalize_transport_tabs(tabs)
  df <- tabs[["infra_tech"]]
  if (is.null(df)) return(tech_list)

  if (!is.null(branch) && "branch" %in% names(df)) {
    keep <- vapply(df[["branch"]], function(b) {
      !is.na(b) && branch %in% trimws(strsplit(b, ",")[[1]])
    }, logical(1))
    df <- df[keep, , drop = FALSE]
  }

  stock_yr_cols <- sort(grep("^stock_[0-9]{4}$", names(df), value = TRUE))
  if (length(stock_yr_cols) == 0) return(tech_list)

  years <- as.integer(sub("^stock_", "", stock_yr_cols))

  for (i in seq_len(nrow(df))) {
    nm <- df[["name"]][i]
    if (!nm %in% names(tech_list)) next
    stock_vals <- as.numeric(unlist(df[i, stock_yr_cols]))
    valid <- !is.na(stock_vals) & stock_vals > 0
    if (!any(valid)) next
    stock_df <- data.frame(
      region = NA_character_,
      year   = years[valid],
      stock  = stock_vals[valid],
      stringsAsFactors = FALSE
    )
    tech_list[[nm]] <- update(tech_list[[nm]], capacity = stock_df)
  }
  tech_list
}

# ── 5c. Infrastructure as Supply ─────────────────────────────────────────────────

#' Build newSupply objects from the infra_tech tab.
#'
#' Each row becomes one unconstrained supply (cost = 0) providing the
#' infrastructure service commodity (output_comm). Used when
#' infra_as_supply = TRUE so that vehicle fuel inputs can still be linked
#' to infra service commodities via cinp2ainp aeff rows without modelling
#' the infrastructure explicitly as a technology.
#'
#' @param tabs      Named list from read_transport_inputs().
#' @param branch    Optional character; when non-NULL only rows applicable to this branch.
#' @param base_year Integer; year used for the zero-cost availability entry.
#' @param as_repo   Logical; if TRUE (default) return a newRepository, else a list.
build_ldv_infra_supply <- function(tabs, branch = NULL, base_year = 2025L, as_repo = TRUE) {
  tabs <- .normalize_transport_tabs(tabs)
  df <- tabs[["infra_tech"]]
  if (is.null(df)) {
    warning("infra_tech sheet not found in tabs — returning empty.")
    return(if (as_repo) newRepository("repo_infra_sup") else list())
  }

  if (!is.null(branch) && "branch" %in% names(df)) {
    keep <- vapply(df[["branch"]], function(b) {
      !is.na(b) && branch %in% trimws(strsplit(b, ",")[[1]])
    }, logical(1))
    df <- df[keep, , drop = FALSE]
  }

  sup_list <- lapply(seq_len(nrow(df)), function(i) {
    p  <- df[i, ]
    nm <- gsub("^INFR_", "SUP_INFR_", p[["name"]])
    newSupply(
      name         = nm,
      commodity    = p[["output_comm"]],
      region       = NA_character_,
      availability = data.frame(
        region           = NA_character_,
        year             = base_year,
        cost             = 0,
        stringsAsFactors = FALSE
      )
    )
  })
  nms <- gsub("^INFR_", "SUP_INFR_", df[["name"]])
  names(sup_list) <- nms
  if (as_repo) Reduce(add, sup_list, newRepository("repo_infra_sup"))
  else sup_list
}

#' Build newCommodity objects for infrastructure service commodities.
#'
#' Creates one commodity per unique output_comm value in the infra_tech tab
#' (e.g. PETRO_REFUEL, CNG_REFUEL in PJ) plus MKT_* market input commodities
#' (e.g. MKT_PETRO in kt) with no emission factors. Also declares LAND (kha)
#' and CNG_LEAK (PJ) as needed.
#'
#' @param tabs    Named list from read_transport_inputs().
#' @param branch  Optional character; when non-NULL only rows applicable to this branch.
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a list.
build_ldv_infra_commodities <- function(tabs, branch = NULL, as_repo = TRUE) {
  tabs <- .normalize_transport_tabs(tabs)
  df <- tabs[["infra_tech"]]
  if (is.null(df)) {
    warning("infra_tech sheet not found — returning empty.")
    return(if (as_repo) newRepository("repo_infra_comm") else list())
  }

  if (!is.null(branch) && "branch" %in% names(df)) {
    keep <- vapply(df[["branch"]], function(b) {
      !is.na(b) && branch %in% trimws(strsplit(b, ",")[[1]])
    }, logical(1))
    df <- df[keep, , drop = FALSE]
  }

  comm_list <- list()

  # ── Output (service) commodities ─────────────────────────────────────────
  # One commodity per unique output_comm / output_unit pair (all in PJ)
  uniq_out <- unique(df[, c("output_comm", "output_unit"), drop = FALSE])
  for (i in seq_len(nrow(uniq_out))) {
    nm <- uniq_out[["output_comm"]][i]
    comm_list[[nm]] <- newCommodity(
      name      = nm,
      unit      = uniq_out[["output_unit"]][i],
      timeframe = "ANNUAL"
    )
  }

  # ── MKT_* input commodities (market/wholesale throughput) ─────────────────
  # One per unique input commodity / unit pair; no emission factors.
  for (i in seq_len(nrow(df))) {
    input_spec <- .get_infra_input_spec(df[i, , drop = FALSE])
    for (j in seq_along(input_spec$comms)) {
      nm <- input_spec$comms[[j]]
      if (!nm %in% names(comm_list)) {
        comm_list[[nm]] <- newCommodity(
          name      = nm,
          unit      = input_spec$units[[j]],
          timeframe = "ANNUAL"
        )
      }
    }
  }

  # ── Auxiliary commodities ─────────────────────────────────────────────────
  # CNG_LEAK: fugitive CH4 from CNG stations
  if ("cng_leak" %in% names(df) && any(!is.na(df[["cng_leak"]]))) {
    if (!"CNG_LEAK" %in% names(comm_list)) {
      comm_list[["CNG_LEAK"]] <- newCommodity(
        name      = "CNG_LEAK",
        unit      = "PJ",
        timeframe = "ANNUAL"
      )
    }
  }

  if (as_repo) Reduce(add, comm_list, newRepository("repo_infra_comm"))
  else comm_list
}

#' Add cinp2ainp aeff rows to vehicle technologies linking fuel use to infra service.
#'
#' Reads the cinp_comms and cinp2ainp_ratio columns from infra_df to build a
#' fuel->infra map, then appends cinp2ainp aeff rows to each vehicle technology
#' whose input commodities match an entry in the map.
#'
#' @param tech_list Named list of technology objects.
#' @param infra_df  Data frame from tabs[["infra_tech"]].
#' @return Updated tech_list with aeff rows added.
add_infra_aeff <- function(tech_list, infra_df) {
  infra_df <- .normalize_infra_tech_df(infra_df)
  if (is.null(infra_df) || !("cinp_comms" %in% names(infra_df))) return(tech_list)

  # Build fuel -> list(acomm, cinp2ainp) map and acomm -> unit map from infra_df rows
  fuel_map    <- list()
  acomm_units <- list()  # acomm -> unit (for @aux)
  for (i in seq_len(nrow(infra_df))) {
    acomm <- infra_df[["output_comm"]][i]
    ratio <- as.numeric(infra_df[["cinp2ainp_ratio"]][i])
    unit  <- if ("output_unit" %in% names(infra_df)) infra_df[["output_unit"]][i] else NA_character_
    fuels <- .split_infra_values(infra_df[["cinp_comms"]][i])
    for (f in fuels) fuel_map[[f]] <- list(acomm = acomm, cinp2ainp = ratio)
    acomm_units[[acomm]] <- unit
  }

  lapply(tech_list, function(tch) {
    # Access input commodity names safely across energyRt slot variations
    inp_data  <- tryCatch(slot(tch, "input"), error = function(e) NULL)
    inp_comms <- if (is.null(inp_data))             character(0)
                 else if (is.data.frame(inp_data))  inp_data$comm
                 else if (isS4(inp_data))            inp_data@data$comm
                 else                               character(0)

    matched <- inp_comms[inp_comms %in% names(fuel_map)]
    if (length(matched) == 0) return(tch)

    new_rows <- do.call(rbind, lapply(matched, function(f) {
      m <- fuel_map[[f]]
      # Use a 1-row NA template from @aeff so column schema matches exactly
      row_df <- tch@aeff[NA_integer_, , drop = FALSE]
      row_df[["acomm"]]     <- m$acomm
      row_df[["comm"]]      <- f
      row_df[["region"]]    <- NA_character_
      row_df[["year"]]      <- NA_integer_
      row_df[["slice"]]     <- NA_character_
      row_df[["cinp2ainp"]] <- m$cinp2ainp
      row_df
    }))
    combined_aeff <- rbind(tch@aeff, new_rows)

    # Declare each new acomm in @aux (acomm + unit); skip ones already present
    new_acomms <- unique(new_rows$acomm)
    new_acomms <- new_acomms[!new_acomms %in% tch@aux$acomm]
    if (length(new_acomms) > 0) {
      new_aux_rows <- data.frame(
        acomm = new_acomms,
        unit  = unlist(acomm_units[new_acomms], use.names = FALSE),
        stringsAsFactors = FALSE
      )
      combined_aux <- rbind(tch@aux, new_aux_rows)
      update(tch, aeff = combined_aeff, aux = combined_aux)
    } else {
      update(tch, aeff = combined_aeff)
    }
  })
}

.build_wholesale_supply_list <- function(sup_list, commodity_map) {
  sup_out <- list()

  for (src_name in names(commodity_map)) {
    if (!src_name %in% names(sup_list)) next

    src_sup <- sup_list[[src_name]]
    dst_comm <- unname(commodity_map[[src_name]])
    dst_name <- paste0("SUP_", dst_comm)

    sup_out[[dst_name]] <- newSupply(
      name         = dst_name,
      desc         = src_sup@desc,
      commodity    = dst_comm,
      unit         = src_sup@unit,
      weather      = src_sup@weather,
      reserve      = src_sup@reserve,
      availability = src_sup@availability,
      region       = src_sup@region,
      misc         = src_sup@misc
    )
  }

  sup_out
}
# ── 6. Fleet (stock) ─────────────────────────────────────────────────────────

#' Apply fleet stock values from the fleet tab to technology objects.
#'
#' Sets the @capacity$stock slot on each matching technology.  Technologies
#' present in tech_list but absent from the fleet tab are left unchanged.
#'
#' @param tech_list  Named list or repository of ideea_vehicle (newTechnology)
#'   objects. If a repository is passed, the updated repository is returned.
#' @param tabs       Named list returned by read_transport_inputs().
#' @param branch     Optional character; when the fleet tab has a 'branch' column
#'   only rows matching this branch are applied. NULL (default) applies all rows.
#' @return Same type as tech_list (named list or repository) with
#'   @capacity$stock populated for each technology that appears in the fleet tab.
build_ldv_fleet <- function(tech_list, tabs, branch = NULL) {
  # Handle both plain list and repository inputs transparently
  is_repo <- inherits(tech_list, "repository")
  obj_list <- if (is_repo) tech_list@data else tech_list

  df <- tabs[["fleet"]]
  if (is.null(df)) {
    warning("'fleet' sheet not found — returning tech_list unchanged.")
    return(tech_list)
  }

  # Filter by branch when available
  if (!is.null(branch) && "branch" %in% names(df)) {
    df <- df[df[["branch"]] == branch, , drop = FALSE]
  }

  # ── Wide format: stock_YYYY columns (one row per tech) ─────────────────────
  # Build names like stock_2024, stock_2025, ... from whatever columns exist.
  stock_yr_cols <- sort(grep("^stock_[0-9]{4}$", names(df), value = TRUE))
  bound_cols    <- c("cap.lo", "cap.up", "cap.fx", "ncap.lo", "ncap.up", "ncap.fx")

  if (length(stock_yr_cols) > 0) {
    years <- as.integer(sub("^stock_", "", stock_yr_cols))

    for (nm in unique(df[["tech"]])) {
      if (!nm %in% names(obj_list)) next

      row        <- df[df[["tech"]] == nm, , drop = FALSE][1L, ]
      stock_vals <- as.numeric(row[stock_yr_cols])
      valid      <- !is.na(stock_vals)
      if (!any(valid)) next

      stock_df <- data.frame(
        region = row[["region"]],
        year   = years[valid],
        stock  = stock_vals[valid],
        stringsAsFactors = FALSE
      )
      # Single-valued bounds (not year-varying): replicate to fill every year row
      for (bnd in bound_cols) {
        if (bnd %in% names(row) && !is.na(row[[bnd]]))
          stock_df[[bnd]] <- row[[bnd]]
      }

      obj_list[[nm]] <- update(obj_list[[nm]], capacity = stock_df)
    }

  } else {
    # ── Long format (backward compat): year + stock columns ──────────────────
    stk <- df[!is.na(df[["stock"]]) & df[["stock"]] >= 0, ]

    for (nm in unique(stk[["tech"]])) {
      if (!nm %in% names(obj_list)) next

      rows <- stk[stk[["tech"]] == nm, , drop = FALSE]
      stock_df <- data.frame(
        region = rows[["region"]],
        year   = as.integer(rows[["year"]]),
        stock  = rows[["stock"]],
        stringsAsFactors = FALSE
      )
      for (bnd in bound_cols) {
        if (bnd %in% names(rows) && any(!is.na(rows[[bnd]])))
          stock_df[[bnd]] <- rows[[bnd]]
      }

      obj_list[[nm]] <- update(obj_list[[nm]], capacity = stock_df)
    }
  }

  if (is_repo) {
    tech_list@data <- obj_list
    tech_list
  } else {
    obj_list
  }
}

# ── 7. Fuel commodities (shared across branches) ──────────────────────────────

#' Return only the shared fuel commodity objects (branch == "shared").
#'
#' Useful when assembling a combined transport model where fuel commodities
#' are included once, not once per branch.
#'
#' @param tabs    Named list returned by read_transport_inputs().
#' @param as_repo Logical; if TRUE (default) return a newRepository, else a list.
build_fuel_commodities <- function(tabs, as_repo = TRUE) {
  build_ldv_commodities(tabs, branch = "shared", as_repo = as_repo)
}

# ── 8. Branch repository builder ──────────────────────────────────────────────

#' Build a complete mini-model repository for one transport branch.
#'
#' Combines service commodities (branch-specific + shared fuels), fuel supply,
#' demands, and technologies (with fleet stock applied) for the given branch.
#' Returns a newRepository ready to pass to newModel().
#'
#' Infra service commodities (PETRO_REFUEL, CNG_REFUEL, LPG_REFUEL, EV_CHARGING)
#' are included automatically when infra_as_tech or infra_as_supply is TRUE.
#'
#' @param tabs         Named list from read_transport_inputs().
#' @param branch       Character; branch identifier, e.g. "LDV_psgr", "HDV_frgt".
#' @param fuel_energy  Named numeric vector of fuel energy content passed to
#'   build_ldv_technologies().
#' @param start_year   Integer; technology availability start year (default 2020).
#' @return A \code{repository} object containing all model objects for the branch.
build_branch_repo <- function(tabs, branch, fuel_energy = c(), start_year = 2020L,
                              infra_as_tech   = getOption("infra_as_tech",   FALSE),
                              infra_as_supply = getOption("infra_as_supply", FALSE)) {
  # browser()
  tabs <- .normalize_transport_tabs(tabs)

  # Names of the simple pass-through supplies in the infrastructure tab that are
  # replaced when an explicit infra mode is active.
  infra_sup_names <- c("SUP_PETRO_REFUEL", "SUP_CNG_REFUEL", "SUP_LPG_REFUEL", "SUP_EV_CHARGING")

  # Detect whether this is a road branch (INFR_* applies only to road transport)
  road_branches <- c("LDV_psgr", "LDV_frgt", "HDV_psgr", "HDV_frgt",
                     "motorbikes_psgr", "motorbikes_frgt")
  is_road_branch <- branch %in% road_branches

  comm_list <- build_ldv_commodities(tabs, branch = branch, as_repo = FALSE)
  dem_list  <- build_ldv_demands(tabs, branch = branch, as_repo = FALSE)
  tech_list <- build_ldv_technologies(tabs, branch = branch, fuel_energy = fuel_energy,
                                      start_year = start_year, as_repo = FALSE)
  tech_list <- build_ldv_fleet(tech_list, tabs, branch = branch)

  sup_list  <- build_ldv_supply(tabs, as_repo = FALSE)
  fuel_sup  <- sup_list

  infra_tech_list <- list()
  infra_sup_list  <- list()
  infra_comm_list <- list()
  land_comm_obj   <- NULL
  land_sup_obj    <- NULL
  mkt_sup_list    <- list()

  if (isTRUE(infra_as_tech) && is_road_branch) {
    infra_df        <- tabs[["infra_tech"]]
    wholesale_supply_map <- c(SUP_GSL = "MKT_GSL", SUP_DSL = "MKT_DSL")
    wholesale_sup_list <- .build_wholesale_supply_list(sup_list, wholesale_supply_map)
    fuel_sup        <- sup_list[!names(sup_list) %in% c(infra_sup_names, names(wholesale_supply_map))]

    # Infra commodities: output (PJ) + MKT_* input (kt/GWh) + CNG_LEAK
    infra_comm_list <- build_ldv_infra_commodities(tabs, branch = branch, as_repo = FALSE)

    # INFR_* technology objects (branch-filtered)
    infra_tech_list <- build_ldv_infra_techs(tabs, branch = branch, as_repo = FALSE)

    # Apply stock to infra techs
    infra_tech_list <- build_ldv_infra_fleet(infra_tech_list, tabs, branch = branch)

    # Link vehicle fuel inputs to infra service commodities via cinp2ainp aeff rows
    tech_list       <- add_infra_aeff(tech_list, infra_df)

    # LAND commodity (kha)
    land_comm_name <- "LAND"
    land_comm_unit <- "kha"
    if (!land_comm_name %in% names(comm_list) &&
        !land_comm_name %in% names(infra_comm_list)) {
      land_comm_obj <- tryCatch(
        newCommodity(name = land_comm_name, unit = land_comm_unit, timeframe = "ANNUAL"),
        error = function(e) { warning("Could not create LAND commodity: ", e$message); NULL }
      )
    }

    # SUP_LAND: unconstrained land supply at negligible shadow cost
    land_sup_obj <- tryCatch(
      newSupply(
        name         = "SUP_LAND",
        commodity    = land_comm_name,
        region       = "IND",
        availability = data.frame(region = "IND",
                                  year = c(2020, 2100),
                                  cost = 1e-2,
                                  stringsAsFactors = FALSE)
      ),
      error = function(e) { warning("Could not create SUP_LAND: ", e$message); NULL }
    )

    # SUP_MKT_*: carry over retail supply costs to wholesale inputs when available;
    # fall back to zero-cost throughput commodities only for unmapped inputs.
    infra_branch_df <- if ("branch" %in% names(infra_df)) {
      infra_df[
        vapply(infra_df[["branch"]], function(b)
          !is.na(b) && branch %in% trimws(strsplit(b, ",")[[1]]), logical(1)),
        , drop = FALSE
      ]
    } else {
      infra_df
    }
    mkt_comms <- if (nrow(infra_branch_df) > 0) {
      unique(unlist(lapply(seq_len(nrow(infra_branch_df)), function(i) {
        .get_infra_input_spec(infra_branch_df[i, , drop = FALSE])$comms
      }), use.names = FALSE))
    } else {
      character(0)
    }
    mkt_comms <- mkt_comms[grepl("^MKT_", mkt_comms)]
    fallback_mkt_comms <- setdiff(mkt_comms, sub("^SUP_", "", names(wholesale_sup_list)))
    fallback_mkt_sup_list <- lapply(fallback_mkt_comms, function(mc) {
      newSupply(
        name         = paste0("SUP_", mc),
        commodity    = mc,
        region       = NA_character_,
        availability = data.frame(region = NA_character_,
                                  year = 2020L,
                                  cost = 0,
                                  stringsAsFactors = FALSE)
      )
    })
    if (length(fallback_mkt_comms) > 0)
      names(fallback_mkt_sup_list) <- paste0("SUP_", fallback_mkt_comms)
    mkt_sup_list <- c(wholesale_sup_list, fallback_mkt_sup_list)

  } else if (isTRUE(infra_as_supply) && is_road_branch) {
    infra_df        <- tabs[["infra_tech"]]
    fuel_sup        <- sup_list[!names(sup_list) %in% infra_sup_names]
    infra_comm_list <- build_ldv_infra_commodities(tabs, branch = branch, as_repo = FALSE)
    infra_sup_list  <- build_ldv_infra_supply(tabs, branch = branch, as_repo = FALSE)
    tech_list       <- add_infra_aeff(tech_list, infra_df)
  } else {
    # Default / non-road branches:
    #   * Road branch in default mode: declare infra service output commodities
    #     so SUP_PETRO_REFUEL etc. (kept in fuel_sup) have valid commodities.
    #   * Non-road branches: drop the infra-related supplies entirely, since
    #     PETRO_REFUEL / CNG_REFUEL / LPG_REFUEL / EV_CHARGING commodities are
    #     not declared for them (no INFR_* objects, no MKT_*, no LAND).
    if (is_road_branch) {
      if (!is.null(tabs[["infra_tech"]])) {
        infra_comm_list <- build_ldv_infra_commodities(tabs, branch = branch, as_repo = FALSE)
      }
    } else {
      fuel_sup <- sup_list[!names(sup_list) %in% infra_sup_names]
    }
  }

  all_objs <- c(
    unname(comm_list),
    unname(infra_comm_list),
    unname(fuel_sup),
    if (!is.null(land_comm_obj)) list(land_comm_obj),
    if (!is.null(land_sup_obj))  list(land_sup_obj),
    unname(mkt_sup_list),
    unname(dem_list),
    unname(tech_list),
    unname(infra_tech_list),
    unname(infra_sup_list)
  )

  repo_name <- paste0("repo_", gsub("[^A-Za-z0-9]", "_", branch))
  Reduce(add, all_objs, newRepository(repo_name))
}
