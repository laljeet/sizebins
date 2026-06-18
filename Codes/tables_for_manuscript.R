# =====================================================================
# tables_for_manuscript.R  -- all main-text tables, one place
# Project: structural shift in irrigated cropland by farm size bin
# Target journal: Land Use Policy (Elsevier)
#
# Companion to figures_for_manuscript.R. Regenerates the four manuscript
# tables from the data so they are reproducible artifacts, not hand-kept
# markdown. Keep revisiting this one file through review.
#
# Produces ./results/tables_for_manuscript.md with:
#   Table 1  observed acreage change by size class, with bounds
#   Table 2  operations change by size class
#   Table 3  acres per operation change by size class
#   Table 4  state summary of large-class change
#
# Reads ./data/bins_long.rds (from 01_parse_audit.R).
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
})

bins <- as.data.table(readRDS("./data/bins_long.rds"))

bin_order <- c(
  "(1.0 TO 9.9 ACRES)", "(10.0 TO 49.9 ACRES)", "(50.0 TO 69.9 ACRES)",
  "(70.0 TO 99.9 ACRES)", "(100 TO 139 ACRES)", "(140 TO 179 ACRES)",
  "(180 TO 219 ACRES)", "(220 TO 259 ACRES)", "(260 TO 499 ACRES)",
  "(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)"
)
# print labels: "1.0 to 9.9", "2,000 or more"
plabel <- function(x) {
  x <- gsub(" ACRES", "", x); x <- gsub("[()]", "", x)
  x <- gsub(" TO ", " to ", x); x <- gsub(" OR MORE", " or more", x)
  x
}

cap_min <- c(1.0, 10.0, 50.0, 70.0, 100.0, 140.0, 180.0, 220.0, 260.0, 500.0, 1000.0, 2000.0)
cap_max <- c(9.9, 49.9, 69.9, 99.9, 139.0, 179.0, 219.0, 259.0, 499.0, 999.0, 1999.0, NA)
names(cap_min) <- names(cap_max) <- bin_order

# p95 ceiling for the open 2,000+ class
emp_cap_2k <- bins[bin == "(2,000 OR MORE ACRES)" & cell == "observed",
                   quantile(acres / ops, 0.95, na.rm = TRUE)]
cap_max["(2,000 OR MORE ACRES)"] <- emp_cap_2k

bins[, cmin := cap_min[as.character(bin)]]
bins[, cmax := cap_max[as.character(bin)]]
bins[, acres_obs   := fifelse(cell == "observed", acres, NA_real_)]
bins[, acres_floor := fifelse(cell == "observed", acres, ops * cmin)]
bins[, acres_ceil  := fifelse(cell == "observed", acres, ops * cmax)]

# ---- markdown helpers -----------------------------------------------
md_table <- function(dt) {
  dt <- as.data.table(dt)
  hdr  <- paste0("| ", paste(names(dt), collapse = " | "), " |")
  sep  <- paste0("| ", paste(rep("---", ncol(dt)), collapse = " | "), " |")
  rows <- apply(dt, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(hdr, sep, rows), collapse = "\n")
}
# format signed numbers with a true minus glyph
sgn  <- function(x, d = 2) ifelse(x >= 0, sprintf(paste0("+%.", d, "f"), x),
                                  sprintf(paste0("\u2212%.", d, "f"), abs(x)))
sgni <- function(x) ifelse(x >= 0, paste0("+", format(round(x), big.mark = ",")),
                           paste0("\u2212", format(abs(round(x)), big.mark = ",")))
num  <- function(x) format(round(x), big.mark = ",")

res <- "./results/tables_for_manuscript.md"
dir.create("./results", showWarnings = FALSE)
cat("# Tables\n\n", file = res, append = FALSE)

# ---- TABLE 1: acreage change with bounds ----------------------------
agg <- bins[year %in% c("2012", "2022"), .(
  obs   = sum(acres_obs,   na.rm = TRUE) / 1e6,
  floor = sum(acres_floor, na.rm = TRUE) / 1e6,
  ceil  = sum(acres_ceil,  na.rm = TRUE) / 1e6
), by = .(year, bin)]
t1 <- dcast(agg, bin ~ year, value.var = c("obs", "floor", "ceil"))
t1[, bin := factor(bin, levels = bin_order, ordered = TRUE)]; setorder(t1, bin)
t1[, ch := obs_2022 - obs_2012]
t1[, pct := 100 * ch / obs_2012]
t1[, flch := floor_2022 - floor_2012]
t1[, cech := ceil_2022 - ceil_2012]
t1[, dir := fifelse(flch < 0 & cech < 0, "Definitive decline",
             fifelse(flch > 0 & cech > 0, "Definitive growth", "Ambiguous"))]
t1out <- t1[, .(
  `Size class (acres)` = plabel(as.character(bin)),
  `2012 (M ac)` = sprintf("%.2f", obs_2012),
  `2022 (M ac)` = sprintf("%.2f", obs_2022),
  `Change (M ac)` = sgn(ch),
  `Change (%)` = sgn(pct, 1),
  `Floor change (M ac)` = sgn(flch),
  `Ceiling change (M ac)` = sgn(cech),
  `Direction` = dir
)]
cat("## Table 1. Change in observed irrigated acreage by farm size class, conterminous United States, 2012 to 2022.\n\n",
    file = res, append = TRUE)
cat("Acreage in millions of acres. Observed columns sum published (non-suppressed) cells. Floor and ceiling columns give the bounded change, where the floor treats each suppressed cell at its class lower limit and the ceiling at its class upper limit (95th percentile of observed acres per operation for the open 2,000-acre class). A class is definitive when floor and ceiling changes agree in sign.\n\n",
    file = res, append = TRUE)
cat(md_table(t1out), "\n\n", file = res, append = TRUE)

# ---- TABLE 2: operations change -------------------------------------
op <- dcast(bins[year %in% c("2012","2022")], bin ~ year,
            value.var = "ops", fun.aggregate = sum, na.rm = TRUE)
op[, bin := factor(bin, levels = bin_order, ordered = TRUE)]; setorder(op, bin)
op[, ch := `2022` - `2012`][, pct := 100 * ch / `2012`]
t2out <- op[, .(
  `Size class (acres)` = plabel(as.character(bin)),
  `2012` = num(`2012`), `2022` = num(`2022`),
  `Change` = sgni(ch), `Change (%)` = sgn(pct, 1)
)]
allrow <- data.table(`Size class (acres)` = "All classes",
  `2012` = num(sum(op$`2012`)), `2022` = num(sum(op$`2022`)),
  `Change` = sgni(sum(op$ch)), `Change (%)` = sgn(100*sum(op$ch)/sum(op$`2012`),1))
t2out <- rbind(t2out, allrow)
cat("## Table 2. Change in number of operations reporting irrigated land by farm size class, 2012 to 2022.\n\n",
    file = res, append = TRUE)
cat("Operation counts are published for every class in every year, including cells where acreage is suppressed.\n\n",
    file = res, append = TRUE)
cat(md_table(t2out), "\n\n", file = res, append = TRUE)

# ---- TABLE 3: acres per operation -----------------------------------
apo <- bins[cell == "observed" & year %in% c("2012","2022"), .(
  apo = sum(acres, na.rm = TRUE) / sum(ops, na.rm = TRUE)), by = .(year, bin)]
t3 <- dcast(apo, bin ~ year, value.var = "apo")
t3[, bin := factor(bin, levels = bin_order, ordered = TRUE)]; setorder(t3, bin)
t3[, ch := `2022` - `2012`][, pct := 100 * ch / `2012`]
# one decimal for the change everywhere (small classes need it)
t3out <- t3[, .(
  `Size class (acres)` = plabel(as.character(bin)),
  `2012 (ac/op)` = sprintf("%.1f", `2012`),
  `2022 (ac/op)` = sprintf("%.1f", `2022`),
  `Change (ac/op)` = sgn(ch, 1),
  `Change (%)` = sgn(pct, 1)
)]
cat("## Table 3. Change in irrigated acres per operation by farm size class, 2012 to 2022.\n\n",
    file = res, append = TRUE)
cat("Acres per operation computed from observed cells only, with matched numerator and denominator.\n\n",
    file = res, append = TRUE)
cat(md_table(t3out), "\n\n", file = res, append = TRUE)

# ---- TABLE 4: state summary -----------------------------------------
big <- c("(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)")
acw <- dcast(bins[cell == "observed"], state_alpha + fips + bin ~ year, value.var = "acres")
acw[, d := `2022` - `2012`]
opw <- dcast(bins, state_alpha + fips + bin ~ year, value.var = "ops")
opw[, d := `2022` - `2012`]
st <- merge(
  acw[bin %in% big, .(large = sum(d, na.rm = TRUE) / 1e3), by = state_alpha],
  acw[bin == "(2,000 OR MORE ACRES)", .(top = sum(d, na.rm = TRUE) / 1e3), by = state_alpha],
  by = "state_alpha", all = TRUE)
st <- merge(st, opw[, .(ops = sum(d, na.rm = TRUE)), by = state_alpha], by = "state_alpha", all = TRUE)
regime <- function(large, top, ops) {
  base <- if (is.na(large) || is.na(top)) "Insufficient large-scale data"
    else if (large < 0 && top < 0) "Retreat"
    else if (large < 0 && top > 0) "Upward consolidation"
    else if (large > 0 && top > 0) "Expansion"
    else "Mixed"
  if (!is.na(ops) && ops < 0) base <- paste0(base, ", operators exit")
  base
}
st[, reg := mapply(regime, large, top, ops)]
st[, has := !is.na(large)]
setorder(st, -has, large)
ins <- st[is.na(top) | is.na(large), sort(state_alpha)]
t4 <- st[!(is.na(top) | is.na(large)), .(
  State = state_alpha,
  `Large 500+ (k ac)` = sgni(large), `2,000+ (k ac)` = sgni(top),
  `Operators (all)` = sgni(ops), Regime = reg
)]
cat("## Table 4. State summary of large-class irrigated acreage change, 2012 to 2022.\n\n",
    file = res, append = TRUE)
cat("Large 500+ sums the observed acre change across the 500-to-999, 1,000-to-1,999, and 2,000-acre-or-more classes. The 2,000+ column isolates the largest class. Operators is the all-class change in operation counts. Acreage in thousands of acres. The regime is derived from the sign of the large-class and largest-class changes; the operator clause reflects the sign of the all-class operator change. States are ordered from largest large-class loss to largest gain.\n\n",
    file = res, append = TRUE)
cat(md_table(t4), "\n\n", file = res, append = TRUE)
cat("States omitted for insufficient large-scale irrigation (no reportable 2,000-acre class): ",
    paste(ins, collapse = ", "), ".\n", sep = "", file = res, append = TRUE)

cat("wrote", res, "\n")
