# =====================================================================
# 02_bin_shift.R -- Stage 2: The Spine (bin-level shift, 2012 -> 2022)
# Project: structural shift in irrigated cropland by farm size bin
#
# Supersedes 02_bin_shift_bounds.R (wrong: 2022-ceiling vs 2012-floor,
# an impossibility test) and 02a (right structure, but 2,000+ ceiling
# anchored to a single outlier). This is the single canonical Stage 2.
#
# Logic:
#   PRIMARY  = observed-only acres, floor vs floor. No imputation. This
#              is the headline; it does not depend on what is hidden.
#   CONFIRM  = parallel bounds (floor-vs-floor AND ceil-vs-ceil). A bin's
#              direction is robust only if BOTH bounds agree. The 2,000+
#              open ceiling uses the 95th percentile of observed acres/op,
#              never the max, so one extreme county cannot set the roof.
#
# Outputs:
#   ./data/shift_bounds.rds  - bin-level bounds + robustness flags
#   ./data/bins_bounded.rds  - cell-level bounds (feeds Stages 4 & 7)
#   ./results/RESULTS02.md   - single Stage 2 table (overwrites)
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
})

bins <- as.data.table(readRDS("./data/bins_long.rds"))

# ---- 1. bin caps ----------------------------------------------------
# Closed bins use explicit min/max. The open 2,000+ bin gets an empirical
# ceiling from the 95th percentile of observed acres/op in that bin, and a
# floor of its lower edge (2,000).
bin_caps <- data.table(
  bin = factor(c(
    "(1.0 TO 9.9 ACRES)", "(10.0 TO 49.9 ACRES)", "(50.0 TO 69.9 ACRES)",
    "(70.0 TO 99.9 ACRES)", "(100 TO 139 ACRES)", "(140 TO 179 ACRES)",
    "(180 TO 219 ACRES)", "(220 TO 259 ACRES)", "(260 TO 499 ACRES)",
    "(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)"
  ), levels = levels(bins$bin), ordered = TRUE),
  cap_min = c(1.0, 10.0, 50.0, 70.0, 100.0, 140.0, 180.0, 220.0, 260.0, 500.0, 1000.0, 2000.0),
  cap_max = c(9.9, 49.9, 69.9, 99.9, 139.0, 179.0, 219.0, 259.0, 499.0, 999.0, 1999.0, NA_real_)
)

# p95 (not max) for the open bin -- robust to a single outlier county
emp_cap_2k <- bins[bin == "(2,000 OR MORE ACRES)" & cell == "observed",
                   quantile(acres / ops, 0.95, na.rm = TRUE)]
bin_caps[is.na(cap_max), cap_max := emp_cap_2k]
cat("2,000+ ceiling (p95 of observed acres/op):", round(emp_cap_2k), "ac/op\n")

bins <- merge(bins, bin_caps, by = "bin")

# ---- 2. cell-level series ------------------------------------------
# observed acres (NA where suppressed) -- used for the PRIMARY result
bins[, acres_obs := fifelse(cell == "observed", acres, NA_real_)]
# parallel bounds -- used for the CONFIRMATION
bins[, acres_floor := fifelse(cell == "observed", acres, ops * cap_min)]
bins[, acres_ceil  := fifelse(cell == "observed", acres, ops * cap_max)]

# ---- 3. aggregate to CONUS bin totals, 2012 & 2022 ------------------
agg <- bins[year %in% c("2012", "2022"), .(
  obs_mil   = sum(acres_obs,   na.rm = TRUE) / 1e6,
  floor_mil = sum(acres_floor, na.rm = TRUE) / 1e6,
  ceil_mil  = sum(acres_ceil,  na.rm = TRUE) / 1e6
), by = .(year, bin)]

shift <- dcast(agg, bin ~ year, value.var = c("obs_mil", "floor_mil", "ceil_mil"))

# PRIMARY: observed-only shift + percent
shift[, obs_shift := obs_mil_2022 - obs_mil_2012]
shift[, obs_pct   := round(100 * obs_shift / obs_mil_2012, 1)]

# CONFIRMATION: parallel bounds, robust only if both agree
shift[, floor_shift := floor_mil_2022 - floor_mil_2012]
shift[, ceil_shift  := ceil_mil_2022  - ceil_mil_2012]
shift[, robust_dir := fifelse(floor_shift < 0 & ceil_shift < 0, "Definitive Decline",
                              fifelse(floor_shift > 0 & ceil_shift > 0, "Definitive Growth",
                                      "Ambiguous"))]

setorder(shift, bin)

cat("\n===== Stage 2: bin-level shift 2012 -> 2022 (M acres) =====\n")
print(shift[, .(bin,
                obs_2012 = round(obs_mil_2012, 2),
                obs_2022 = round(obs_mil_2022, 2),
                obs_pct,
                floor_shift = round(floor_shift, 2),
                ceil_shift  = round(ceil_shift, 2),
                robust_dir)])

# ---- 4. write the single Stage 2 results table ----------------------
dir.create("./results", showWarnings = FALSE)
res <- "./results/RESULTS02.md"

md_table <- function(dt) {
  dt <- as.data.table(dt)
  hdr  <- paste0("| ", paste(names(dt), collapse = " | "), " |")
  sep  <- paste0("| ", paste(rep("---", ncol(dt)), collapse = " | "), " |")
  rows <- apply(dt, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(hdr, sep, rows)
}
cat_md <- function(...) cat(..., "\n", file = res, sep = "", append = TRUE)

cat("# Results - Stage 2\n", file = res, append = FALSE)  # overwrite, single table
cat_md("\n## Stage 2 - The Spine: Bin-Level Shift 2012-2022\n")
cat_md("\n_Primary result is observed-only acres (no imputation): the sum of ",
       "published bin acres each year. Confirmation columns report parallel ",
       "bounds - the direction is Definitive only when both the floor-vs-floor ",
       "and ceiling-vs-ceiling differences agree in sign. The open 2,000+ ceiling ",
       "uses the 95th percentile of observed acres per operation (",
       round(emp_cap_2k), " ac/op), not the maximum, so no single county sets the roof. ",
       "Values in millions of acres._\n\n")

md_out <- shift[, .(
  `Size Bin`        = bin,
  `2012 (obs)`      = round(obs_mil_2012, 2),
  `2022 (obs)`      = round(obs_mil_2022, 2),
  `Shift (obs)`     = round(obs_shift, 2),
  `% Change`        = obs_pct,
  `Floor Shift`     = round(floor_shift, 2),
  `Ceil Shift`      = round(ceil_shift, 2),
  `Robustness`      = robust_dir
)]
cat_md(paste(md_table(md_out), collapse = "\n"), "\n")

# ---- 5. save --------------------------------------------------------
dir.create("./data", showWarnings = FALSE)
saveRDS(shift, "./data/shift_bounds.rds")
saveRDS(bins,  "./data/bins_bounded.rds")   # cell-level bounds for Stages 4 & 7
cat("\nwrote ./data/shift_bounds.rds, ./data/bins_bounded.rds, ./results/RESULTS02.md\n")
