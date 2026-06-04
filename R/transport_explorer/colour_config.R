# colour_config.R -- fixed colour palettes for transport model visualisation.
#
# Each config is a data.frame with columns:
#   name    - human-readable label shown in the legend
#   colour  - hex colour string
#   pattern - perl-compatible regex (ignore.case) matched against fill-column
#             values.  First matching row wins; unmatched values fall back to
#             the `fallback` colour in resolve_colours().
#
# Edit this file to adjust colours or add new fuel/branch/mode entries.
# IMPORTANT: more specific patterns must come BEFORE shorter overlapping ones
# (e.g. "PHEV" before "HEV", "LDVF_" before "LDV_").

# -- Fuel / energy-carrier colour map -----------------------------------------
# Patterns match commodity codes (GSL, DSL, ELCPJ ...) AND technology /
# process names that embed the fuel token (LDV_GSL_2025, RAIL_ELCP_2030 ...).
FUEL_COLOUR_CONFIG <- data.frame(
  name = c(
    # MKT_* wholesale / market layer — same hue as retail, ~20% darker (×0.80 RGB).
    # These rows MUST come before their retail counterparts (first-match wins).
    "MKT PHEV",          "MKT Hybrid (HEV)",
    "MKT Gasoline",      "MKT E10 blend",      "MKT E85 / Ethanol",
    "MKT Diesel",        "MKT CNG / LNG",
    "MKT LPG",           "MKT Electric / BEV",
    "MKT Hydrogen",      "MKT Biofuel",
    # Retail / end-use layer
    "PHEV",              "Hybrid (HEV)",
    "Gasoline",          "E10 blend",          "E85 / Ethanol",
    "Diesel",            "CNG / LNG",
    "LPG",               "Electric / BEV",
    "Hydrogen / FCEV",   "Biofuel"
  ),
  colour = c(
    # MKT colours (retail × 0.80)
    "#caa682",   # MKT PHEV       (#fdd0a2 × 0.80)
    "#ca7147",   # MKT HEV        (#fc8d59 × 0.80)
    "#b8440a",   # MKT Gasoline   (#e6550d × 0.80)
    "#ca7130",   # MKT E10        (#fd8d3c × 0.80)
    "#81ae7c",   # MKT E85/Ethanol (#a1d99b × 0.80)
    "#5e568e",   # MKT Diesel     (#756bb1 × 0.80)
    "#5d9d5e",   # MKT CNG        (#74c476 × 0.80)
    "#cba34a",   # MKT LPG        (#fecc5c × 0.80)
    "#276897",   # MKT Electric   (#3182bd × 0.80)
    "#568bab",   # MKT Hydrogen   (#6baed6 × 0.80)
    "#278243",   # MKT Biofuel    (#31a354 × 0.80)
    # Retail colours
    # Gasoline family: PHEV and HEV share the orange hue but progressively lighter
    # Ordering darkest→lightest: Gasoline > E10 > HEV > PHEV
    "#fdd0a2",   # PHEV  - light peach-orange (most diverged from pure ICE)
    "#fc8d59",   # HEV   - medium orange
    "#e6550d",   # Gasoline - dark orange-red
    "#fd8d3c",   # E10 blend
    "#a1d99b",   # E85 / Ethanol (lighter biofuel-green)
    "#756bb1",   # Diesel
    "#74c476",   # CNG / LNG
    "#fecc5c",   # LPG
    "#3182bd",   # Electric / BEV
    "#6baed6",   # Hydrogen / FCEV
    "#31a354"    # Biofuel
  ),
  pattern = c(
    # MKT patterns — anchored at ^ so they only match MKT_* names.
    # PHEV before HEV within the MKT block too.
    "^MKT_PHEV",
    "^MKT_HEV",
    "^MKT_GSL(?!10)|^MKT_PETRO|^MKT_ICE",
    "^MKT_GSL10|^MKT_E10",
    "^MKT_E85",
    "^MKT_DSL",
    "^MKT_CNG|^MKT_LNG",
    "^MKT_LPG",
    "^MKT_ELCP|^MKT_BEV|^MKT_EV|^MKT_CHARG",
    "^MKT_H2|^MKT_FCEV|^MKT_HFC",
    "^MKT_BIO",
    # Retail patterns.
    # \b fails on underscore-separated tokens (e.g. LDV_BEV_2025, INFR_CNG_REFUEL)
    # because _ is a word character in Perl regex.  Use (?<![A-Za-z0-9]) /
    # (?![A-Za-z0-9]) instead to treat _ as a delimiter.
    "PHEV|plug.?in",
    "HEV|[Hh]ybrid",
    "(?<![A-Za-z0-9])GSL(?!10)|PETRO|ICE[V]?|[Gg]asoline|[Pp]etrol",
    "GSL10|E10",
    "(?<![A-Za-z0-9])E85(?![A-Za-z0-9])|[Ee]thanol",
    "DSL|[Dd]iesel",
    "CNG|LNG",
    "LPG",
    "ELCP|BEV|CHARGING|(?<![A-Za-z])EV(?![A-Za-z])|[Ee]lectric",
    "FCEV|HFC|(?<![A-Za-z])H2(?![A-Za-z0-9])|[Hh]ydrogen",
    "BIO|[Bb]iofuel"
  ),
  stringsAsFactors = FALSE
)

# -- Branch colour map (psgr / freight as light-dark shades per mode) ----------
# Patterns match:
#   1. Verbose branch labels: "LDV psgr", "Rail freight" ...
#   2. Tech-prefix strings:   ^LDV_, ^RAILF_ ... (process column)
#   3. Demand commodity names: ^PLDV/^FLDV, ^PHDV/^FHDV, ^PRAIL/^FRAIL ...
#      (P- = passenger, F- = freight prefix convention)
BRANCH_COLOUR_CONFIG <- data.frame(
  name = c(
    "LDV psgr",          "LDV freight",
    "HDV psgr",          "HDV freight",
    "Rail psgr",         "Rail freight",
    "Aircraft psgr",     "Aircraft freight",
    "Ships psgr",        "Ships freight",
    "Motorbikes psgr",   "Motorbikes freight"
  ),
  colour = c(
    "#1f77b4", "#aec7e8",   # blue   family - LDV
    "#ff7f0e", "#ffbb78",   # orange family - HDV
    "#2ca02c", "#98df8a",   # green  family - Rail
    "#d62728", "#ff9896",   # red    family - Aircraft
    "#9467bd", "#c5b0d5",   # purple family - Ships
    "#8c564b", "#c49c94"    # brown  family - Motorbikes
  ),
  pattern = c(
    "^PLDV|^DEM_PLDV|^LDV_|LDV[. _]psgr",
    "^FLDV|^DEM_FLDV|^LDVF_|LDV[. _]frgt|LDV[. _]freight",
    "^PHDV|^HPSGR|^DEM_PHDV|^DEM_HPSGR|^HDV_|HDV[. _]psgr",
    "^FHDV|^HFRGT|^DEM_FHDV|^DEM_HFRGT|^HDVF_|HDV[. _]frgt|HDV[. _]freight",
    "^PRAIL|^RPSGR|^DEM_PRAIL|^DEM_RPSGR|^RAIL_|Rail[. _]psgr",
    "^FRAIL|^RFRGT|^DEM_FRAIL|^DEM_RFRGT|^RAILF_|Rail[. _]frgt|Rail[. _]freight",
    "^PARC|^APSGR|^DEM_PARC|^DEM_APSGR|^ARC_|Aircraft[. _]psgr",
    "^FARC|^AFRGT|^DEM_FARC|^DEM_AFRGT|^ARCF_|Aircraft[. _]frgt|Aircraft[. _]freight",
    "^PSHIP|^SPSGR|^DEM_PSHIP|^DEM_SPSGR|^SHIP_|Ships[. _]psgr",
    "^FSHIP|^SFRGT|^DEM_FSHIP|^DEM_SFRGT|^SHIPF_|Ships[. _]frgt|Ships[. _]freight",
    "^PMOTO|^MPSGR|^DEM_PMOTO|^DEM_MPSGR|^MOTO_|[Mm]otorbikes[. _]psgr",
    "^FMOTO|^MFRGT|^DEM_FMOTO|^DEM_MFRGT|^MOTOF_|[Mm]otorbikes[. _]frgt"
  ),
  stringsAsFactors = FALSE
)

# -- Vehicle-type colour map (psgr + freight collapse to one colour per mode) --
# Uses substring matching (no word boundaries) so demand comms like PLDV/FLDV
# are automatically captured by the mode token they contain.
VEHICLE_TYPE_COLOUR_CONFIG <- data.frame(
  name = c(
    "LDV", "HDV", "Rail", "Aircraft", "Ships", "Motorbikes"
  ),
  colour = c(
    "#1f77b4",   # blue   - LDV
    "#ff7f0e",   # orange - HDV
    "#2ca02c",   # green  - Rail
    "#d62728",   # red    - Aircraft
    "#9467bd",   # purple - Ships
    "#8c564b"    # brown  - Motorbikes
  ),
  pattern = c(
    # Substring patterns: PLDV, FLDV, LDV_*, LDVF_* all contain "LDV"
    "LDV",
    "HDV",
    "RAIL|[Rr]ail",
    "ARC|[Aa]ircr",
    "SHIP|[Ss]hip",
    "MOTO|[Mm]oto"
  ),
  stringsAsFactors = FALSE
)

# -- Lookup helper -------------------------------------------------------------

#' Map a vector of fill-column values to hex colours using a colour config.
#'
#' @param values   Character (or coercible) vector of fill-column values.
#' @param config   data.frame with columns: name, colour, pattern.
#' @param fallback Hex colour for values that match no pattern.  Default grey.
#' @return Named character vector: names = unique input values, values = hex.
resolve_colours <- function(values, config, fallback = "#bdbdbd") {
  uvals <- unique(as.character(values))
  cols  <- vapply(uvals, function(v) {
    for (i in seq_len(nrow(config))) {
      if (grepl(config$pattern[i], v, ignore.case = TRUE, perl = TRUE))
        return(config$colour[i])
    }
    fallback
  }, character(1L))
  stats::setNames(cols, uvals)
}
