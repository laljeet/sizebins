# =====================================================================
# 03_operations_and_consolidation.R -- Stage 3: The Human Element
# Project: structural shift in irrigated cropland by farm size bin
#
# This stage analyzes the shift in the number of operations (farms)
# to disambiguate acreage loss (are farms exiting, or graduating up?).
# Operations are rarely suppressed, so we use raw counts.
#
# Outputs:
#   ./data/ops_shift.rds     - the calculated operations shift
#   results/RESULTS.md       - appends Stage 3 summary tables
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# ---- load trustworthy inputs ----------------------------------------
# We can use the bounded bins from Stage 2, which has the ops column
bins <- as.data.table(readRDS("./data/bins_bounded.rds"))

# ---- 1. aggregate CONUS operations by year and bin ------------------
ops_totals <- bins[year %in% c("2012", "2022"), .(
  total_ops = sum(ops, na.rm = TRUE)
), by = .(year, bin)]

# ---- 2. cast wide to calculate the shift ----------------------------
ops_shift <- dcast(ops_totals, bin ~ year, value.var = "total_ops")
setnames(ops_shift, c("2012", "2022"), c("ops_2012", "ops_2022"))

# Calculate net change and percentage change in operations
ops_shift[, net_change := ops_2022 - ops_2012]
ops_shift[, pct_change := round((net_change / ops_2012) * 100, 1)]

# ---- 3. calculate the total macro attrition (all bins combined) -----
macro_attrition <- ops_shift[, .(
  bin = "ALL BINS (MACRO TOTAL)",
  ops_2012 = sum(ops_2012, na.rm = TRUE),
  ops_2022 = sum(ops_2022, na.rm = TRUE),
  net_change = sum(net_change, na.rm = TRUE)
)]
macro_attrition[, pct_change := round((net_change / ops_2012) * 100, 1)]

# Combine for the final table
final_ops_table <- rbind(ops_shift, macro_attrition)

cat("\n===== 2012 -> 2022 Operator Shift by Bin =====\n")
print(final_ops_table)

# ---- 4. write results to markdown (cumulative) ----------------------
res <- "./results/RESULTS03.md"

md_table <- function(dt) {
  dt <- as.data.table(dt)
  hdr  <- paste0("| ", paste(names(dt), collapse = " | "), " |")
  sep  <- paste0("| ", paste(rep("---", ncol(dt)), collapse = " | "), " |")
  rows <- apply(dt, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(hdr, sep, rows)
}
cat_md <- function(...) cat(..., "\n", file = res, sep = "", append = TRUE)

cat_md("\n## Stage 3 - The Human Element: Operator Consolidation vs. Exit (2012-2022)\n")
cat_md("\n_Calculated using raw, unsuppressed operation counts to determine if acreage loss is driven by farm attrition or upward graduation._\n\n")

md_out <- final_ops_table[, .(
  `Size Bin` = bin,
  `2012 Operations` = ops_2012,
  `2022 Operations` = ops_2022,
  `Net Change` = net_change,
  `Percent Change (%)` = pct_change
)]

cat_md(paste(md_table(md_out), collapse = "\n"), "\n")

# ---- save stage 3 outputs -------------------------------------------
saveRDS(ops_shift, "./data/ops_shift.rds")
cat("\nwrote ./data/ops_shift.rds and appended Stage 3 to ./results/RESULTS03.md\n")
