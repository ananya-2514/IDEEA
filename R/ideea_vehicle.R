# ideea_vehicle.R - to add to IDEEA lib
# Wrapper around energyRt::newTechnology() for road vehicles.
# Accepts conventional physical parameters (fuel efficiency in common units,
# annual km, occupancy, USD costs) and converts them to model-native quantities.

# ── Default fuel energy content (MJ/L, LHV) ──────────────────────────────────
.fuel_energy_defaults <- c(
  GSL  = 34.2,   # gasoline / petrol
  DSL  = 36.9,   # diesel
  BIO  = 26.8,   # ethanol/biodiesel blend (~E10 average)
  LPG  = 26.0,   # liquefied petroleum gas
  CNG  = 35.8,   # compressed natural gas (MJ/m³ … treated as MJ/L equivalent)
  HYD  = 10.8    # liquid hydrogen (LH2)
)

# ── Unit conversion helper ────────────────────────────────────────────────────

#' Convert fuel efficiency to use2cact (million vehicle-km per PJ of fuel)
#'
#' @param eff   Numeric efficiency value.
#' @param unit  Character. One of:
#'   \code{"L/100km"}, \code{"mpg"} (US), \code{"mpg_uk"} (Imperial),
#'   \code{"km/kWh"}, \code{"kWh/100km"},
#'   \code{"BTU/mi"}, \code{"MJ/km"}, \code{"PJ/MPKm"}.
#' @param primary_fuel  Name of the primary fuel (used for energy-content
#'   lookup when the unit is volumetric, e.g. \code{"L/100km"} or \code{"mpg"}).
#' @param fuel_energy  Named numeric vector of fuel energy content (MJ/L).
#'
#' @return Numeric. Million vehicle-km per PJ of fuel (use2cact).
#'
#' @details
#' The energyRt model equation relating inputs to outputs is:
#'   \code{vTechInp * cinp2use = vTechOut / (use2cact * cact2cout)}
#'
#' Therefore \code{use2cact} for an output commodity row equals:
#'   million vehicle-km produced per PJ of fuel consumed by that mode.
#'
#' @examples
#' eff_to_use2cact(8, "L/100km", "GSL")        # petrol car, 8 L/100 km
#' eff_to_use2cact(50, "mpg", "GSL")            # 50 US mpg petrol car
#' eff_to_use2cact(6, "km/kWh", "ELCPJ")        # EV (energy-content free)
#' eff_to_use2cact(250, "BTU/mi", "GSL")        # US DOE reference format
eff_to_use2cact <- function(eff, unit, primary_fuel = NULL,
                             fuel_energy = .fuel_energy_defaults) {
  stopifnot(is.numeric(eff), length(eff) == 1, eff > 0)

  # merge user-supplied energy content with defaults (user values take priority)
  fe <- .fuel_energy_defaults
  fe[names(fuel_energy)] <- fuel_energy

  # volumetric units need fuel energy content
  needs_energy <- unit %in% c("L/100km", "mpg", "mpg_uk")

  if (needs_energy) {
    if (is.null(primary_fuel) || !nzchar(primary_fuel)) {
      stop("`primary_fuel` must be specified for efficiency unit '", unit, "'")
    }
    if (!primary_fuel %in% names(fe)) {
      stop(
        "Energy content not found for fuel '", primary_fuel,
        "'. Supply it via the `fuel_energy` argument, e.g. ",
        "fuel_energy = c(", primary_fuel, " = <MJ/L>)"
      )
    }
    mj_per_l <- fe[[primary_fuel]]
  }

  use2cact <- switch(unit,
    # ── volumetric ────────────────────────────────────────────────────────────
    # L/100km  →  eff L per 100 km; energy per km = eff * mj_per_l / 100 [MJ/km]
    #          →  1 PJ = 1e9 MJ  →  km per PJ = 1e9 / (eff * mj_per_l / 100)
    #          →  Mveh-km per PJ = 1e9 * 100 / (eff * mj_per_l) / 1e6
    #                            = 1e5 / (eff * mj_per_l)
    "L/100km"  = 1e5 / (mj_per_l * eff),

    # US mpg: 1 US mpg = 235.215 L/100km
    "mpg"      = 1e5 / (mj_per_l * (235.215 / eff)),

    # Imperial (UK) mpg: 1 UK mpg = 282.481 L/100km
    "mpg_uk"   = 1e5 / (mj_per_l * (282.481 / eff)),

    # ── electrical ───────────────────────────────────────────────────────────
    # km/kWh: 1 PJ = 1e15 J / 3.6e6 J/kWh = 2.7778e8 kWh
    #       → Mveh-km per PJ = eff [km/kWh] * 2.7778e8 [kWh/PJ] / 1e6
    #                        = eff * 277.78
    "km/kWh"   = eff * 1e9 / (3.6 * 1e6),   # = eff * 277.78

    # kWh/100km → km/kWh = 100/eff
    "kWh/100km" = (100 / eff) * 1e9 / (3.6 * 1e6),

    # ── US DOE ───────────────────────────────────────────────────────────────
    # BTU/mi: 1 BTU = 1055.06 J = 1.05506e-12 PJ; 1 mile = 1.60934 km
    # BTU per PJ = 1/1.05506e-12 = 9.479e11
    # km per PJ = 9.479e11 / eff [BTU/mi] * 1.60934 [km/mi]
    # Mveh-km per PJ = km per PJ / 1e6
    "BTU/mi"   = 1.60934 / (eff * 1.05506e-12) / 1e6,

    # ── SI ───────────────────────────────────────────────────────────────────
    # MJ/km: energy per km = eff MJ; 1 PJ = 1e9 MJ
    #       → km per PJ = 1e9 / eff; Mveh-km per PJ = 1e3 / eff
    "MJ/km"    = 1e3 / eff,

    # PJ/MVKm: model-native; direct inverse (million vehicle-km per PJ)
    "PJ/MPKm"  = 1 / eff,

    stop("Unknown efficiency unit '", unit, "'. Supported: ",
         "\"L/100km\", \"mpg\", \"mpg_uk\", \"km/kWh\", \"kWh/100km\", ",
         "\"BTU/mi\", \"MJ/km\", \"PJ/MPKm\"")
  )

  use2cact
}


#' Convert use2cact back to human-readable fuel efficiency
#'
#' @param use2cact Numeric. Million vehicle-km per PJ of fuel.
#' @param unit     Character. Target display unit (same set as \code{eff_to_use2cact}).
#' @param primary_fuel Character. Fuel name for energy-content lookup (volumetric units).
#' @param fuel_energy  Named numeric. Fuel energy content (MJ/L).
#'
#' @return Numeric efficiency in the requested unit.
use2cact_to_eff <- function(use2cact, unit, primary_fuel = NULL,
                             fuel_energy = c()) {
  fe <- .fuel_energy_defaults
  fe[names(fuel_energy)] <- fuel_energy

  needs_energy <- unit %in% c("L/100km", "mpg", "mpg_uk")
  if (needs_energy) {
    if (is.null(primary_fuel) || !nzchar(primary_fuel)) {
      stop("`primary_fuel` must be specified for efficiency unit '", unit, "'")
    }
    mj_per_l <- fe[[primary_fuel]]
  }

  switch(unit,
    "L/100km"   = 1e5 / (use2cact * mj_per_l),
    "mpg"       = 235.215 / (1e5 / (use2cact * mj_per_l)),   # L/100km → mpg
    "mpg_uk"    = 282.481 / (1e5 / (use2cact * mj_per_l)),
    "km/kWh"    = use2cact * 3.6 * 1e6 / 1e9,                # = use2cact / 277.78
    "kWh/100km" = 100 / (use2cact * 3.6 * 1e6 / 1e9),
    "BTU/mi"    = 1.60934 / (use2cact * 1e6 * 1.05506e-12),
    "MJ/km"     = 1e3 / use2cact,
    "PJ/MPKm"   = 1 / use2cact,
    stop("Unknown unit: ", unit)
  )
}


#' Build a human-readable fuel efficiency summary for a vehicle technology
#'
#' Given the \code{ceff} slot of a technology and its fuel inputs, returns a
#' tidy data.frame showing efficiency in the most natural units for each fuel.
#' Rows correspond to active service modes (highway / city).
#'
#' @param tech  An energyRt \code{technology} S4 object created by
#'   \code{ideea_vehicle()}.
#' @param fuel_energy Named numeric. Additional fuel energy content (MJ/L)
#'   beyond the built-in defaults.
#'
#' @return A data.frame with columns \code{mode}, \code{fuel}, \code{eff},
#'   \code{unit}, plus convenience columns \code{L_per_100km}, \code{mpg_us},
#'   \code{km_per_kWh}, and \code{MJ_per_km}.
vehicle_eff_table <- function(tech, fuel_energy = c()) {
  fe <- .fuel_energy_defaults
  fe[names(fuel_energy)] <- fuel_energy

  ceff <- tech@ceff
  if (is.null(ceff) || nrow(ceff) == 0) return(NULL)

  # Output rows have use2cact set; input rows have it NA
  out_rows <- ceff[!is.na(ceff$use2cact), , drop = FALSE]
  if (nrow(out_rows) == 0) return(NULL)

  # Fuel inputs (first non-blend input or all inputs)
  inp_rows <- tech@input
  fuels    <- unique(inp_rows$comm)

  # Detect primary fuel type for display unit selection
  is_electric <- any(grepl("ELC|ELCPJ|BEV", fuels, ignore.case = TRUE))
  is_hydrogen <- any(grepl("^H2|HYD|FCEV", fuels, ignore.case = TRUE))
  has_liquid  <- any(fuels %in% names(fe))

  primary_fuel <- fuels[fuels %in% names(fe)][1]
  if (is.na(primary_fuel)) primary_fuel <- NULL

  rows <- lapply(seq_len(nrow(out_rows)), function(i) {
    r        <- out_rows[i, ]
    u2c      <- r$use2cact
    mode_lbl <- if (grepl("HWY|hwy|highway", r$comm, ignore.case = TRUE)) "Highway"
                else if (grepl("CTY|cty|city", r$comm, ignore.case = TRUE)) "City"
                else r$comm

    # Compute all equivalent efficiencies
    L100km    <- if (!is.null(primary_fuel) && has_liquid)
                   round(use2cact_to_eff(u2c, "L/100km", primary_fuel, fuel_energy), 2)
                 else NA_real_
    mpg_us    <- if (!is.null(primary_fuel) && has_liquid)
                   round(use2cact_to_eff(u2c, "mpg",     primary_fuel, fuel_energy), 2)
                 else NA_real_
    km_kWh    <- round(use2cact_to_eff(u2c, "km/kWh", fuel_energy = fuel_energy), 2)
    MJ_km     <- round(use2cact_to_eff(u2c, "MJ/km"), 2)

    # Choose the "primary" display unit
    primary_unit <- if (is_electric) "km/kWh"
                    else if (is_hydrogen) "MJ/km"
                    else "L/100km"
    primary_val  <- switch(primary_unit,
      "km/kWh"  = km_kWh,
      "MJ/km"   = MJ_km,
      "L/100km" = L100km
    )

    data.frame(
      mode           = mode_lbl,
      fuel           = paste(fuels, collapse = " + "),
      eff            = primary_val,
      unit           = primary_unit,
      L_per_100km    = L100km,
      mpg_us         = mpg_us,
      km_per_kWh     = km_kWh,
      MJ_per_km      = MJ_km,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


# ── Service-unit resolver ─────────────────────────────────────────────────────

.resolve_service_unit <- function(service_unit, load_unit) {
  if (!is.null(service_unit)) return(service_unit)
  switch(load_unit,
    passengers = "MPKm",
    tons       = "MTKm",
    load_unit  # custom string: use as-is
  )
}


# ── Main function ─────────────────────────────────────────────────────────────

#' Create a vehicle technology object
#'
#' @description
#' A convenience wrapper around \code{\link[energyRt]{newTechnology}} for road
#' vehicles. Accepts physical parameters in familiar engineering units and
#' builds the \code{ceff}, \code{input}, \code{output}, \code{cap2act},
#' \code{invcost}, and \code{fixom} structures required by the energyRt model.
#'
#' @param name Character. Technology name (required).
#' @param desc Character. Short description (optional).
#'
#' @param service_hwy Character. Name of the highway output commodity, or
#'   \code{NULL} to omit (creates a city-only vehicle).
#' @param service_cty Character. Name of the city output commodity, or
#'   \code{NULL} to omit (creates a highway-only vehicle).
#' @param service_unit Character or \code{NULL}. Unit for output commodities.
#'   Defaults to \code{"MPKm"} (passengers) or \code{"MTKm"} (tons) based on
#'   \code{load_unit}.
#'
#' @param fuels Character vector. Fuel input commodity names, e.g.
#'   \code{c("GSL")}, \code{c("GSL", "BIO")}, \code{c("ELCPJ")}.
#' @param fuel_unit Character. Unit for fuel inputs (default \code{"PJ"}).
#'
#' @param eff_hwy Numeric. Fuel efficiency for highway driving. Required when
#'   \code{service_hwy} is not \code{NULL}.
#' @param eff_cty Numeric. Fuel efficiency for city driving. Required when
#'   \code{service_cty} is not \code{NULL}.
#' @param eff_unit Character. Unit of the efficiency values. One of:
#'   \code{"L/100km"}, \code{"mpg"} (US), \code{"mpg_uk"} (Imperial),
#'   \code{"km/kWh"}, \code{"kWh/100km"}, \code{"BTU/mi"},
#'   \code{"MJ/km"}, \code{"PJ/MPKm"}.
#' @param primary_fuel Character. Which fuel in \code{fuels} to use for energy-
#'   content lookup when \code{eff_unit} is volumetric. Defaults to
#'   \code{fuels[1]}.
#' @param fuel_energy Named numeric. Fuel energy content in MJ/L (LHV).
#'   Pre-loaded defaults: GSL = 34.2, DSL = 36.9, BIO = 26.8, LPG = 26.0,
#'   CNG = 35.8, HYD = 10.8. Supply additional fuels or override defaults here.
#'
#' @param avg_km_yr Numeric. Average annual distance per vehicle (km/year).
#'   Used to compute \code{cap2act = avg_km_yr * capacity_units / 1e6}.
#' @param capacity_units Numeric. Number of vehicles per model capacity unit
#'   (default 1000). Determines scaling for \code{cap2act}, \code{invcost},
#'   and \code{fixom}.
#'
#' @param load_unit Character. Type of load carried. \code{"passengers"} sets
#'   the default \code{service_unit} to \code{"MPKm"}; \code{"tons"} sets it
#'   to \code{"MTKm"}. A custom string is passed through unchanged.
#' @param load_per_vehicle_hwy Numeric. Average load per vehicle on highway
#'   (passengers or tons). Becomes \code{cact2cout} for the highway output row.
#'   Ignored if \code{service_hwy = NULL}.
#' @param load_per_vehicle_cty Numeric. Average load per vehicle in city.
#'   Becomes \code{cact2cout} for the city output row.
#'   Ignored if \code{service_cty = NULL}.
#'
#' @param share_hwy_up Numeric (0–1). Maximum share of vehicle activity on
#'   highway (\code{share.up} for the highway output row). Only applied when
#'   both services are present.
#' @param share_cty_up Numeric (0–1). Maximum share of vehicle activity in
#'   city. Only applied when both services are present.
#'
#' @param blend_fuel Character or \code{NULL}. Name of a secondary/blend fuel
#'   already included in \code{fuels} (e.g. \code{"BIO"} for a
#'   gasoline-ethanol blend). Receives a \code{share.up = blend_share_up}
#'   constraint in \code{ceff} and is excluded from energy-balance calculations.
#' @param blend_share_up Numeric (0–1). Maximum blend fraction for
#'   \code{blend_fuel}.
#'
#' @param vehicle_cost_usd Numeric or \code{NULL}. Purchase price per vehicle
#'   in USD. Converted to \code{invcost} in MUSD per \code{capacity_units}.
#' @param annual_cost_usd Numeric or \code{NULL}. Annual operating cost per
#'   vehicle in USD (maintenance, taxes, fees). Converted to \code{fixom}.
#'
#' @param olife Numeric. Technical lifetime in years (default 10).
#' @param region Character. Region for the \code{capacity} slot
#'   (default \code{NA_character_} = all regions).
#' @param start_year Integer. First year entry in the \code{capacity} slot
#'   (default 2022).
#'
#' @param ... Additional arguments forwarded directly to
#'   \code{\link[energyRt]{newTechnology}}.
#'
#' @return An energyRt \code{technology} S4 object.
#'
#' @examples
#' # Gasoline car (8 L/100km city, 6 L/100km highway)
#' LDVG <- ideea_vehicle(
#'   name  = "LDVG",
#'   desc  = "Gasoline Light Duty Vehicle",
#'   fuels = c("GSL", "BIO"),
#'   blend_fuel     = "BIO",
#'   blend_share_up = 0.1,
#'   eff_hwy = 6, eff_cty = 8, eff_unit = "L/100km",
#'   avg_km_yr       = 10000,
#'   vehicle_cost_usd = 15000,
#'   annual_cost_usd  = 500,
#'   load_per_vehicle_hwy = 2,
#'   load_per_vehicle_cty = 3
#' )
#'
#' # Electric car (6 km/kWh highway, 4.5 km/kWh city)
#' LDV_EV <- ideea_vehicle(
#'   name  = "LDV_EV",
#'   desc  = "Electric Light Duty Vehicle",
#'   fuels = c("ELCPJ"),
#'   eff_hwy = 6, eff_cty = 4.5, eff_unit = "km/kWh",
#'   avg_km_yr        = 10000,
#'   vehicle_cost_usd = 30000,
#'   annual_cost_usd  = 500
#' )
#'
#' # Freight truck (city-only, diesel, 5 L/100km, 8 tons payload)
#' HDV_CTY <- ideea_vehicle(
#'   name         = "HDV_CTY",
#'   fuels        = c("DSL"),
#'   service_hwy  = NULL,           # no highway service
#'   service_cty  = "FHDVCTY",
#'   load_unit    = "tons",
#'   load_per_vehicle_cty = 8,
#'   eff_cty      = 25, eff_unit = "L/100km",
#'   avg_km_yr    = 50000,
#'   capacity_units   = 100,
#'   vehicle_cost_usd = 80000,
#'   annual_cost_usd  = 5000
#' )
#'
#' @export
ideea_vehicle <- function(
  name,
  desc = "",

  # Services — set either to NULL for a single-mode vehicle
  service_hwy  = "PLDVHWY",
  service_cty  = "PLDVCTY",
  service_unit = NULL,       # auto: "MPKm" (passengers) or "MTKm" (tons)

  # Fuels
  fuels     = character(),
  fuel_unit = "PJ",

  # Efficiency — required for each active service
  eff_hwy      = NULL,
  eff_cty      = NULL,
  eff_unit     = "L/100km",
  primary_fuel = NULL,
  fuel_energy  = c(),

  # Physics
  avg_km_yr      = 10000,
  capacity_units = 1000,

  # Load
  load_unit            = "passengers",
  load_per_vehicle_hwy = 2,
  load_per_vehicle_cty = 3,

  # Mode-split bounds (only applied when both services present)
  share_hwy_up = 0.4,
  share_cty_up = 0.8,

  # Optional blend fuel
  blend_fuel     = NULL,
  blend_share_up = 0.1,

  # Economics (USD/vehicle → MUSD/capacity_units)
  vehicle_cost_usd = NULL,
  annual_cost_usd  = NULL,

  # Lifetime & model setup
  olife      = 10,
  region     = NA_character_,
  start_year = 2022,

  # Report option
  include_params = TRUE,

  ...
) {
  # ── Input validation ───────────────────────────────────────────────────────
  if (missing(name) || !nzchar(name)) stop("`name` is required.")
  if (length(fuels) == 0) stop("`fuels` must name at least one fuel commodity.")
  if (is.null(service_hwy) && is.null(service_cty)) {
    stop("At least one of `service_hwy` or `service_cty` must be non-NULL.")
  }
  if (!is.null(service_hwy) && is.null(eff_hwy)) {
    stop("`eff_hwy` is required when `service_hwy` is set.")
  }
  if (!is.null(service_cty) && is.null(eff_cty)) {
    stop("`eff_cty` is required when `service_cty` is set.")
  }
  if (!is.null(blend_fuel) && !blend_fuel %in% fuels) {
    stop("`blend_fuel` ('", blend_fuel, "') must be listed in `fuels`.")
  }

  dual_mode <- !is.null(service_hwy) && !is.null(service_cty)

  # ── Resolve primary fuel for energy-content lookup ──────────────────────────
  if (is.null(primary_fuel) || !nzchar(primary_fuel)) {
    primary_fuel <- fuels[1]
  }

  # ── Resolve service unit ───────────────────────────────────────────────────
  svc_unit <- .resolve_service_unit(service_unit, load_unit)

  # ── Convert efficiencies to use2cact ──────────────────────────────────────
  use2cact_hwy <- if (!is.null(service_hwy)) {
    eff_to_use2cact(eff_hwy, eff_unit, primary_fuel, fuel_energy)
  } else NULL

  use2cact_cty <- if (!is.null(service_cty)) {
    eff_to_use2cact(eff_cty, eff_unit, primary_fuel, fuel_energy)
  } else NULL

  # ── cap2act ────────────────────────────────────────────────────────────────
  # capacity_units vehicles, each travelling avg_km_yr km → million km
  cap2act <- avg_km_yr * capacity_units / 1e6

  # ── Build input data.frame ────────────────────────────────────────────────
  input_df <- data.frame(
    comm  = fuels,
    unit  = fuel_unit,
    group = "i",
    stringsAsFactors = FALSE
  )

  # ── Build output data.frame ───────────────────────────────────────────────
  active_services <- c(service_hwy, service_cty)  # NULLs silently dropped by c()
  output_df <- data.frame(
    comm  = active_services,
    unit  = svc_unit,
    group = "o",
    stringsAsFactors = FALSE
  )

  # ── Build ceff data.frame ─────────────────────────────────────────────────
  # --- fuel input rows ---
  fuel_share_up <- rep(NA_real_, length(fuels))
  if (!is.null(blend_fuel)) {
    fuel_share_up[fuels == blend_fuel] <- blend_share_up
  }
  ceff_inputs <- data.frame(
    comm      = fuels,
    use2cact  = NA_real_,
    share.up  = fuel_share_up,
    cact2cout = NA_real_,
    stringsAsFactors = FALSE
  )

  # --- service output rows ---
  # use2cact is per-service (replicated for both modes from the same formula)
  # share.up is only meaningful in dual-mode (constrains highway vs city split)
  if (dual_mode) {
    ceff_outputs <- data.frame(
      comm      = c(service_hwy, service_cty),
      use2cact  = c(use2cact_hwy, use2cact_cty),
      share.up  = c(share_hwy_up, share_cty_up),
      cact2cout = c(load_per_vehicle_hwy, load_per_vehicle_cty),
      stringsAsFactors = FALSE
    )
  } else if (!is.null(service_hwy)) {
    ceff_outputs <- data.frame(
      comm      = service_hwy,
      use2cact  = use2cact_hwy,
      share.up  = NA_real_,
      cact2cout = load_per_vehicle_hwy,
      stringsAsFactors = FALSE
    )
  } else {
    ceff_outputs <- data.frame(
      comm      = service_cty,
      use2cact  = use2cact_cty,
      share.up  = NA_real_,
      cact2cout = load_per_vehicle_cty,
      stringsAsFactors = FALSE
    )
  }

  ceff_df <- rbind(ceff_inputs, ceff_outputs)

  # ── Economics ─────────────────────────────────────────────────────────────
  invcost_list <- if (!is.null(vehicle_cost_usd)) {
    list(invcost = vehicle_cost_usd * capacity_units / 1e6)
  } else list()

  fixom_list <- if (!is.null(annual_cost_usd)) {
    list(fixom = annual_cost_usd * capacity_units / 1e6)
  } else list()

  # ── Units metadata ────────────────────────────────────────────────────────
  capacity_label <- paste0(capacity_units, " Vehicles")
  activity_mode  <- if (!is.null(service_cty)) "city" else "highway"
  units_list <- list(
    capacity = capacity_label,
    activity = paste0("million km, ", activity_mode),
    costs    = "MUSD"
  )

  # ── Assemble newTechnology() call ─────────────────────────────────────────
  tech_args <- list(
    name     = name,
    desc     = desc,
    input    = input_df,
    output   = output_df,
    units    = units_list,
    cap2act  = cap2act,
    ceff     = ceff_df,
    olife    = list(olife = olife),
    capacity = list(region = region, year = start_year),
    ...
  )

  if (length(invcost_list) > 0) tech_args$invcost <- invcost_list
  if (length(fixom_list)   > 0) tech_args$fixom   <- fixom_list

  tech <- do.call(newTechnology, tech_args)

  if (include_params) {
    vp <- Filter(Negate(is.null), list(
      fuels          = paste(fuels, collapse = ", "),
      primary_fuel   = primary_fuel,
      eff_hwy        = if (!is.null(eff_hwy)) paste(eff_hwy, eff_unit),
      eff_cty        = if (!is.null(eff_cty)) paste(eff_cty, eff_unit),
      avg_km_yr      = paste(format(avg_km_yr, big.mark = ",", scientific = FALSE), "km/yr"),
      capacity_units = paste(capacity_units, "vehicles"),
      load_hwy       = if (!is.null(service_hwy)) paste(load_per_vehicle_hwy, load_unit),
      load_cty       = if (!is.null(service_cty)) paste(load_per_vehicle_cty, load_unit),
      share_hwy_up   = if (dual_mode) paste0(share_hwy_up * 100, "%"),
      share_cty_up   = if (dual_mode) paste0(share_cty_up * 100, "%"),
      blend_fuel     = if (!is.null(blend_fuel))
                         paste0(blend_fuel, " (max ", round(blend_share_up * 100), "%)"),
      vehicle_cost   = if (!is.null(vehicle_cost_usd))
                         paste(format(vehicle_cost_usd, big.mark = ",", scientific = FALSE), "USD"),
      annual_cost    = if (!is.null(annual_cost_usd))
                         paste(format(annual_cost_usd, big.mark = ",", scientific = FALSE), "USD/yr"),
      olife          = paste(olife, "yr")
    ))
    tech@misc[["vehicle_params"]] <- vp
  }

  tech
}
