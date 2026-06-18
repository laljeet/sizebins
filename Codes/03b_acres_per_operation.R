# =====================================================================
# 03b_acres_per_operation.R -- Stage 3b: the consolidation mechanism
# Project: structural shift in irrigated cropland by farm size bin
#
# Stages 2 + 3 established that mid/large bins lost BOTH acres and
# operators. This stage asks the mechanism question: did the operators
# who REMAINED in each bin get bigger? Acres-per-operation (APO) answers
# it. Rising APO in a shrinking bin = consolidation (land absorbed by
# fewer, larger operations). Falling APO in a growing-operator bin =
# fragmentation.
#
# METHOD NOTE - matched numerator/denominator:
# APO must divide OBSERVED acres by OBSERVED operations (same cells).
# Using all operations in the denominator while acres are suppressed in
# the numerator biases APO downward. We therefore restrict BOTH to
# observed cells. Suppressed cells are excluded from APO entirely (they
# cannot inform a ratio whose numerator is hidden).
#
# Outputs:
#   ./data/apo_shift.rds    - APO by bin, 2012 & 2022, change
#   ./results/RESULTS03b.md - Stage 3b table
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
})

bins <- as.data.table(readRDS("./data/bins_bounded.rds"))

# ---- 1. APO from observed cells only (matched num/denom) ------------
apo <- bins[year %in% c("2012", "2022") & cell == "observed", .(
  acres_obs = sum(acres, na.rm = TRUE),
  ops_obs   = sum(ops,   na.rm = TRUE),
  n_cells   = .N
), by = .(year, bin)]
apo[, apo := acres_obs / ops_obs]

# ---- 2. cast wide, compute change -----------------------------------
apo_w <- dcast(apo, bin ~ year, value.var = c("apo", "n_cells"))
apo_w[, apo_change := apo_2022 - apo_2012]
apo_w[, apo_pct    := round(100 * apo_change / apo_2012, 1)]

# ---- 3. classify the mechanism (joins Stage 2 + Stage 3 direction) --
# Pull bin-level acre and operator directions to label each bin's story.
shift  <- as.data.table(readRDS("./data/shift_bounds.rds"))   # has obs_shift
ops    <- as.data.table(readRDS("./data/ops_shift.rds"))      # has net_change (ops)

lab <- merge(apo_w[, .(bin, apo_2012, apo_2022, apo_change, apo_pct)],
             shift[, .(bin, acre_dir = fifelse(obs_shift < 0, "acres down", "acres up"))],
             by = "bin")
lab <- merge(lab,
             ops[, .(bin, ops_dir = fifelse(net_change < 0, "operators down", "operators up"))],
             by = "bin")
lab[, mechanism := fifelse(
  acre_dir == "acres down" & ops_dir == "operators down" & apo_change > 0,
  "Consolidation (exit + survivors grow)",
  fifelse(acre_dir == "acres up" & ops_dir == "operators up" & apo_change > 0,
          "Expansion (more & larger operations)",
          fifelse(ops_dir == "operators up" & apo_change < 0,
                  "Fragmentation (more & smaller operations)",
                  "Mixed")))]

setorder(lab, bin)

cat("\n===== Stage 3b: acres per operation, 2012 -> 2022 =====\n")
print(lab[, .(bin,
              apo_2012 = round(apo_2012, 1),
              apo_2022 = round(apo_2022, 1),
              apo_pct,
              mechanism)])

# ---- 4. write results -----------------------------------------------
dir.create("./results", showWarnings = FALSE)
res <- "./results/RESULTS03b.md"

md_table <- function(dt) {
  dt <- as.data.table(dt)
  hdr  <- paste0("| ", paste(names(dt), collapse = " | "), " |")
  sep  <- paste0("| ", paste(rep("---", ncol(dt)), collapse = " | "), " |")
  rows <- apply(dt, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(hdr, sep, rows)
}
cat_md <- function(...) cat(..., "\n", file = res, sep = "", append = TRUE)

cat("# Results - Stage 3b\n", file = res, append = FALSE)
cat_md("\n## Stage 3b - The Consolidation Mechanism: Acres per Operation\n")
cat_md("\n_Acres per operation (APO) computed from observed cells only, with ",
       "matched numerator and denominator (observed acres / observed operations ",
       "in the same cells). Suppressed cells are excluded because their hidden ",
       "acres cannot inform the ratio. Mechanism column reads APO alongside the ",
       "bin-level acre direction (Stage 2) and operator direction (Stage 3): a ",
       "shrinking bin whose survivors hold more acres each is consolidating; a ",
       "growing-operator bin whose operations hold fewer acres each is fragmenting._\n\n")

md_out <- lab[, .(
  `Size Bin`          = bin,
  `2012 APO (ac/op)`  = round(apo_2012, 1),
  `2022 APO (ac/op)`  = round(apo_2022, 1),
  `APO Change`        = round(apo_change, 1),
  `% Change`          = apo_pct,
  `Mechanism`         = mechanism
)]
cat_md(paste(md_table(md_out), collapse = "\n"), "\n")

# ---- 5. save --------------------------------------------------------
saveRDS(lab, "./data/apo_shift.rds")
cat("\nwrote ./data/apo_shift.rds and ./results/RESULTS03b.md\n")
