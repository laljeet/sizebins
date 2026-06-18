# =====================================================================
# 04c_county_panel_corrected.R -- Stage 4c: county panel, suppression-safe
# Project: structural shift in irrigated cropland by farm size bin
#
# Rebuilds the county-bin shift panel with a critical correction over the
# earlier Stage 4: a cell that was OBSERVED in 2012 but SUPPRESSED in 2022
# must NOT be treated as zero. fill=0 conflates two different things:
#   - true structural absence (bin had farms in 2012, no row in 2022) -> 0 is correct
#   - suppression (bin still present, acres masked)                    -> value is UNKNOWN
# An audit of this file found 2,354 observed->suppressed county-bins versus
# 1,036 observed->absent. Zeroing the suppressed ones manufactures ~2,354
# false collapses on any bin-level map. This script keeps them as NA.
#
# Output: ./data/county_panel.rds  (one row per county x bin, shift + status)
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
})

bins <- as.data.table(readRDS("./data/bins_long.rds"))

# ---- 1. observed acres + a presence flag, per county-bin-year -------
# acres_obs is NA when suppressed. present=TRUE means the bin had a row at
# all that year (operations published), regardless of acres suppression.
cell <- bins[year %in% c("2012", "2022"),
             .(acres_obs = acres,            # NA if suppressed
               present   = TRUE,             # row exists -> bin present
               ops),
             by = .(fips, state_alpha, bin, year, cell)]

wide <- dcast(cell, fips + state_alpha + bin ~ year,
              value.var = c("acres_obs", "present", "ops", "cell"))

# default missing presence to FALSE (no row that year = bin absent)
for (col in c("present_2012", "present_2022")) {
  if (!col %in% names(wide)) wide[, (col) := NA]
  wide[is.na(get(col)), (col) := FALSE]
}

# ---- 2. classify the 2012 -> 2022 transition ------------------------
# status drives how the shift is interpreted and coloured:
#   observed_both : acres published both years -> real, usable shift
#   to_absent     : present 2012, no row 2022  -> true drop to zero
#   from_absent   : no row 2012, present 2022  -> true rise from zero
#   suppressed_end: present both, but >=1 year suppressed -> shift UNKNOWN
wide[, status := fifelse(
  cell_2012 == "observed" & cell_2022 == "observed", "observed_both",
  fifelse(present_2012 & !present_2022, "to_absent",
          fifelse(!present_2012 & present_2022, "from_absent",
                  "suppressed_end")))]

# ---- 3. shift, only where it is genuinely known ---------------------
# observed_both: difference of observed acres.
# to_absent / from_absent: a real zero on one side, observed on the other.
# suppressed_end: NA -- the map must show these as "unknown", not a number.
wide[, acres_2012 := fifelse(cell_2012 == "observed", acres_obs_2012,
                             fifelse(!present_2012, 0, NA_real_))]
wide[, acres_2022 := fifelse(cell_2022 == "observed", acres_obs_2022,
                             fifelse(!present_2022, 0, NA_real_))]
wide[, acre_shift := acres_2022 - acres_2012]          # NA where any side unknown
wide[, ops_2012c := fifelse(is.na(ops_2012), 0, ops_2012)]
wide[, ops_2022c := fifelse(is.na(ops_2022), 0, ops_2022)]
wide[, ops_shift := ops_2022c - ops_2012c]

# ---- 4. audit the transition mix ------------------------------------
cat("\n===== county-bin transition mix =====\n")
print(wide[, .N, by = status][order(-N)])
cat("\nbin-level shifts that are KNOWN (observed_both/to_absent/from_absent):",
    wide[status != "suppressed_end", .N],
    "| UNKNOWN (suppressed_end -> NA on map):",
    wide[status == "suppressed_end", .N], "\n")

# ---- 5. save --------------------------------------------------------
keep <- wide[, .(fips, state_alpha, bin, status, acre_shift, ops_shift,
                 acres_2012, acres_2022)]
saveRDS(keep, "./data/county_panel.rds")
cat("\nwrote ./data/county_panel.rds (", nrow(keep), "rows )\n")
