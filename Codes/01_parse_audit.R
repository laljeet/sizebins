# =====================================================================
# 01_parse_audit.R  -- Stage 1: parse + suppression audit
# Project: structural shift in irrigated cropland by farm size bin
# Unit of analysis = SIZE BIN. CONUS county level. 2012/2017/2022.
# Single source of truth: ./data/raw_quickstats_api.rds (saved from raw_tidy)
#
# This stage builds TRUSTWORTHY INPUTS only. No analysis, no differencing.
# Outputs:
#   ./data/bins_long.rds   - clean long table, 12 bins, acres+ops side by side
#   ./data/totals_long.rds - county TOTAL irrigated acres (domain_desc==TOTAL)
#   console audit          - suppression load by bin x year; bin-sum vs TOTAL
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# ---- load -----------------------------------------------------------
# RDS preserves leading-zero FIPS strings; do not re-pad.
raw <- as.data.table(readRDS("./data/raw_quickstats_api.rds"))

stopifnot(all(c("year","fips","short_desc","domain_desc",
                "domaincat_desc","value_raw") %in% names(raw)))

# year and fips stay CHARACTER throughout (leading zeros).
raw[, year := as.character(year)]
raw[, fips := as.character(fips)]

# ---- parse value ----------------------------------------------------
# value_raw carries literal "NA" for suppressed cells; no thousands commas.
# Parse to numeric; "NA" -> NA. is_supp flags a masked-but-present cell.
raw[, value := suppressWarnings(as.numeric(value_raw))]

# ---- split the three domains ----------------------------------------
# domain_desc == "AREA OPERATED" -> the 12 size bins (the analysis frame)
# domain_desc == "TOTAL"         -> county irrigated total (NOT SPECIFIED), a check
# domain_desc == "IRRIGATION STATUS" -> 2017-only artifact, dropped
measure <- function(x) fifelse(grepl("OPERATIONS", x), "ops", "acres")
raw[, meas := measure(short_desc)]

bins_raw <- raw[domain_desc == "AREA OPERATED"]
tot_raw  <- raw[domain_desc == "TOTAL"]

# ---- harmonize bin labels into an ordered factor --------------------
# domaincat_desc looks like "AREA OPERATED: (1.0 TO 9.9 ACRES)".
# Strip to the bracketed range; assign canonical order small -> large.
bins_raw[, bin := trimws(gsub("^AREA OPERATED:\\s*", "", domaincat_desc))]

bin_levels <- c(
  "(1.0 TO 9.9 ACRES)", "(10.0 TO 49.9 ACRES)", "(50.0 TO 69.9 ACRES)",
  "(70.0 TO 99.9 ACRES)", "(100 TO 139 ACRES)", "(140 TO 179 ACRES)",
  "(180 TO 219 ACRES)", "(220 TO 259 ACRES)", "(260 TO 499 ACRES)",
  "(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)"
)
# Gate: every bin label in the data must be a known level (catch format drift).
unknown <- setdiff(unique(bins_raw$bin), bin_levels)
if (length(unknown)) stop("Unknown bin label(s): ", paste(unknown, collapse=" | "))
bins_raw[, bin := factor(bin, levels = bin_levels, ordered = TRUE)]

# ---- cast acres + ops side by side on year x fips x bin -------------
keycols <- c("year","fips","state_alpha","county_name","bin")
keycols <- intersect(keycols, names(bins_raw))  # state/county names optional
bins <- dcast(bins_raw, paste(paste(keycols, collapse=" + "), "~ meas"),
              value.var = "value")
setnames(bins, c("acres","ops"), c("acres","ops"), skip_absent = TRUE)

# ---- cell typology --------------------------------------------------
# In THIS extract every NA is acres-suppressed-with-ops-present:
#   observed  : acres present (& ops present)
#   suppressed: acres NA, ops present  -> boundable
# A bin simply ABSENT from a county-year has no row at all (handled later
# at the panel-balancing stage, not here).
bins[, cell := fifelse(!is.na(acres), "observed",
                       fifelse(!is.na(acres) | is.na(acres) & !is.na(ops),
                               "suppressed", "other"))]
bins[is.na(acres) & !is.na(ops), cell := "suppressed"]
bins[!is.na(acres),              cell := "observed"]

# ---- AUDIT 1: suppression load by bin x year ------------------------
audit_bin <- bins[, .(
  n_cells    = .N,
  n_obs      = sum(cell == "observed"),
  n_supp     = sum(cell == "suppressed"),
  pct_supp   = round(100 * mean(cell == "suppressed"), 1),
  ops_in_supp = sum(ops[cell == "suppressed"], na.rm = TRUE)
), by = .(year, bin)][order(year, bin)]

cat("\n===== suppression load: bin x year =====\n")
print(audit_bin, nrow = 100)

cat("\n===== suppression load: by year (all bins) =====\n")
print(bins[, .(n = .N, pct_supp = round(100*mean(cell=="suppressed"),1)),
           by = year][order(year)])

# ---- AUDIT 2: bin-sum vs published county TOTAL ---------------------
# Where a county-year has NO suppressed bins, the sum of observed bin
# acres should equal the published TOTAL. This is an internal consistency
# check sourced entirely from the file (TOTAL = domain_desc=="TOTAL").
tot <- tot_raw[meas == "acres", .(year, fips, total_acres = value)]
clean_counties <- bins[, .(any_supp = any(cell == "suppressed"),
                           bin_sum  = sum(acres, na.rm = TRUE)),
                       by = .(year, fips)]
chk <- merge(clean_counties[any_supp == FALSE], tot, by = c("year","fips"))
chk[, diff := bin_sum - total_acres]
cat("\n===== bin-sum vs TOTAL, fully-observed counties only =====\n")
cat("counties checked:", nrow(chk),
    "| exact matches:", sum(chk$diff == 0, na.rm = TRUE),
    "| max |diff|:", max(abs(chk$diff), na.rm = TRUE), "\n")
if (any(chk$diff != 0, na.rm = TRUE)) {
  cat("NON-MATCHING (investigate):\n"); print(head(chk[diff != 0][order(-abs(diff))], 10))
}

# ---- write results to markdown (survives without re-running) ---------
dir.create("./results", showWarnings = FALSE)
res <- "./results/RESULTS.md"

# helper: append a data.table as a github-markdown table
md_table <- function(dt) {
  dt <- as.data.table(dt)
  hdr  <- paste0("| ", paste(names(dt), collapse = " | "), " |")
  sep  <- paste0("| ", paste(rep("---", ncol(dt)), collapse = " | "), " |")
  rows <- apply(dt, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(hdr, sep, rows)
}
cat_md <- function(...) cat(..., "\n", file = res, sep = "", append = TRUE)

# start fresh on Stage 1 (later stages APPEND, do not truncate)
cat("# Results: irrigated cropland by farm size bin\n",
    file = res, append = FALSE)
cat_md("_USDA Census of Ag 2012/2017/2022, CONUS county level. ",
       "Source: ./data/raw_quickstats_api.rds. Generated ", as.character(Sys.Date()), "._\n")

cat_md("\n## Stage 1 - parse + suppression audit\n")
cat_md("\n12 size bins (domain_desc==\"AREA OPERATED\"); TOTAL and 2017-only ",
       "IRRIGATION STATUS rows excluded. bins_long.rds = ", nrow(bins), " rows.\n")

cat_md("\n**Cell typology (12 bins):** observed ",
       sum(bins$cell=="observed"), ", suppressed ", sum(bins$cell=="suppressed"), ".\n")

cat_md("\n**Suppression load by year (all bins):**\n\n")
yr_tab <- bins[, .(n = .N, pct_supp = round(100*mean(cell=="suppressed"),1)), by=year][order(year)]
cat_md(paste(md_table(yr_tab), collapse = "\n"), "\n")

cat_md("\n**Suppression load by bin x year:**\n\n")
cat_md(paste(md_table(audit_bin), collapse = "\n"), "\n")

cat_md("\n**Internal check - bin-sum vs published TOTAL (fully-observed counties):** ",
       nrow(chk), " counties checked, ", sum(chk$diff == 0, na.rm=TRUE),
       " exact matches, max |diff| = ", max(abs(chk$diff), na.rm=TRUE), ".\n")

# ---- save trustworthy inputs ---------------------------------------
saveRDS(bins, "./data/bins_long.rds")
saveRDS(tot,  "./data/totals_long.rds")
cat("\nwrote ./data/bins_long.rds (", nrow(bins), "rows), ./data/totals_long.rds, ./results/RESULTS01.md\n")
