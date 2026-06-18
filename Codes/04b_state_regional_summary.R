# =====================================================================
# 04b_state_regional_summary.R -- Stage 4b: State Regional Summary
# Project: structural shift in irrigated cropland by farm size bin
#
# The citable per-state table. For each state it reports the MEASURED
# quantities and derives a regime label PURELY FROM THE SIGN of three
# changes -- no magnitude thresholds anywhere. A label is therefore a
# definition of a sign pattern, not a judgment about how big a shift is,
# so there is no arbitrary cutoff to defend.
#
# Three signed measures (2012 -> 2022):
#   large_ac : observed acre change summed over 500-999, 1,000-1,999, 2,000+
#   top_ac   : observed acre change in the 2,000+ class alone
#   all_ops  : operator change summed over all 12 bins
#
# Regime (sign logic only):
#   Retreat               large<0 AND top<0      large-farm acreage falling at every scale
#   Upward consolidation  large<0 AND top>0      mid-large falling, 2,000+ rising
#   Expansion             large>0 AND top>0      large-farm acreage rising at every scale
#   Mixed                 any other sign combo
#   (operator clause)     appended when all_ops<0: "+ operators exit"
#                         appended when all_ops>0: "+ operators grow"
# Near-zero values are not special-cased; the printed number lets the
# reader calibrate whether a small shift is meaningful.
#
# Reads bins_long.rds (independent of Stage 2). Outputs:
#   ./data/state_summary.rds
#   ./results/RESULTS04b.md
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
})

bins <- as.data.table(readRDS("./data/bins_long.rds"))

big_bins <- c("(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)")
top_bin  <- "(2,000 OR MORE ACRES)"

# ---- 1. observed acre shift per county-bin --------------------------
ac <- dcast(bins[cell == "observed"], state_alpha + fips + bin ~ year,
            value.var = "acres")
if (!"2012" %in% names(ac)) ac[, `2012` := NA_real_]
if (!"2022" %in% names(ac)) ac[, `2022` := NA_real_]
ac[, ac_shift := `2022` - `2012`]

# ---- 2. operator shift per county-bin (ops rarely suppressed) -------
op <- dcast(bins, state_alpha + fips + bin ~ year, value.var = "ops")
if (!"2012" %in% names(op)) op[, `2012` := NA_real_]
if (!"2022" %in% names(op)) op[, `2022` := NA_real_]
op[, op_shift := `2022` - `2012`]

# ---- 3. aggregate to state level ------------------------------------
st <- merge(
  ac[bin %in% big_bins, .(large_ac = sum(ac_shift, na.rm = TRUE)), by = state_alpha],
  ac[bin == top_bin,    .(top_ac   = sum(ac_shift, na.rm = TRUE)), by = state_alpha],
  by = "state_alpha", all = TRUE
)
st <- merge(st,
            op[bin %in% big_bins, .(large_ops = sum(op_shift, na.rm = TRUE)), by = state_alpha],
            by = "state_alpha", all = TRUE)
st <- merge(st,
            op[, .(all_ops = sum(op_shift, na.rm = TRUE)), by = state_alpha],
            by = "state_alpha", all = TRUE)

st[, large_ac_k := large_ac / 1e3]
st[, top_ac_k   := top_ac   / 1e3]

# ---- 4. regime from SIGNS only (no magnitude thresholds) ------------
acreage_regime <- function(large, top) {
  if (is.na(large) || is.na(top)) return("Insufficient large-scale data")
  if (large < 0 && top < 0) return("Retreat")
  if (large < 0 && top > 0) return("Upward consolidation")
  if (large > 0 && top > 0) return("Expansion")
  return("Mixed")
}

operator_clause <- function(all_ops) {
  if (is.na(all_ops) || all_ops == 0) return("")
  if (all_ops < 0) return(" + operators exit")
  return(" + operators grow")
}

st[, regime := mapply(acreage_regime, large_ac, top_ac)]
st[, regime_full := paste0(regime, mapply(operator_clause, all_ops))]

# Order: states with a large-bin story first (TX most negative -> CA most
# positive), then push insufficient-data states (NA large_ac) to the bottom
# so a no-data state never outranks Texas at the top of the table.
st[, has_data := !is.na(large_ac)]
setorder(st, -has_data, large_ac_k)
st[, has_data := NULL]

# ---- 5. write the citable table (numbers AND regime) ----------------
dir.create("./results", showWarnings = FALSE)
res <- "./results/RESULTS04b.md"

md_table <- function(dt) {
  dt <- as.data.table(dt)
  hdr  <- paste0("| ", paste(names(dt), collapse = " | "), " |")
  sep  <- paste0("| ", paste(rep("---", ncol(dt)), collapse = " | "), " |")
  rows <- apply(dt, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(hdr, sep, rows)
}
cat_md <- function(...) cat(..., "\n", file = res, sep = "", append = TRUE)

cat("# Results - Stage 4b: State Regional Summary\n", file = res, append = FALSE)
cat_md("\n## Structural shift in irrigated cropland by state, 2012-2022\n")
cat_md("\n_Large 500+ sums the observed acre change across the 500-999, ",
       "1,000-1,999, and 2,000+ classes. 2,000+ isolates the largest class. ",
       "Operators sums published operation counts across all twelve classes. ",
       "Acre values in thousands of acres. The regime is derived from the SIGN ",
       "of these changes only, not their magnitude: Retreat = large and 2,000+ ",
       "both falling; Upward consolidation = mid-large falling while 2,000+ rises; ",
       "Expansion = both rising; the operator clause reflects the sign of the ",
       "all-class operator change. Acres reflect published (non-suppressed) cells. ",
       "States ordered from largest large-bin loss to largest gain._\n\n")

md_out <- st[, .(
  State                 = state_alpha,
  `Large 500+ (k ac)`   = round(large_ac_k, 0),
  `2,000+ (k ac)`       = round(top_ac_k, 0),
  `Operators (all)`     = round(all_ops, 0),
  `Regime`              = regime_full
)]
cat_md(paste(md_table(md_out), collapse = "\n"), "\n")

saveRDS(st, "./data/state_summary.rds")
cat("\nwrote ./data/state_summary.rds and ./results/RESULTS04b.md\n")
