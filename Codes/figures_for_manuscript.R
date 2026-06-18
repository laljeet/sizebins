# =====================================================================
# figures_for_manuscript.R  -- all main-text figures, one place
# Project: structural shift in irrigated cropland by farm size bin
# Target journal: Land Use Policy (Elsevier)
#
# This is the PUBLICATION figure script. It supersedes the exploratory
# map scripts (05b, 05c) for anything going into the manuscript. Keep
# revisiting and editing this one file through review; leave the
# exploratory scripts untouched as the record of how the figures were
# developed.
#
# Produces, at 300 dpi into ./figures/manuscript/ :
#   Figure 1  fig1_lollipop_facet.png   12-panel county shift, one per bin
#   Figure 2  fig2_apo_levels.png       acres per operation, 2012 | 2022
#   Figure 3  fig3_apo_change.png       change in acres per operation
#
# Shared conventions (edit once here):
#   - purple-green diverging scale (replaces red-blue)
#   - EPSG:5070 equal-area projection
#   - suppressed counties shown grey / omitted, never zero
#   - within-bin scaling for the lollipop (per-class 95th percentile)
#
# Reads ./data/county_panel.rds (from 04c) and ./data/bins_long.rds.
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(tigris)
  library(ggplot2)
})
options(tigris_use_cache = TRUE)

# ---- shared parameters (edit here, applies to all figures) ----------
PAL_NEG   <- "#762a83"   # purple = decline / shrinking operations
PAL_POS   <- "#1b7837"   # green  = growth / larger operations
PAL_MID   <- "#f7f7f7"   # near-zero
PAL_NA    <- "grey85"    # suppressed / no data
BASE_SIZE <- 9
DPI       <- 300
OUTDIR    <- "./figures/manuscript"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

signed_sqrt <- function(x) sign(x) * sqrt(abs(x))

bin_order <- c(
  "(1.0 TO 9.9 ACRES)", "(10.0 TO 49.9 ACRES)", "(50.0 TO 69.9 ACRES)",
  "(70.0 TO 99.9 ACRES)", "(100 TO 139 ACRES)", "(140 TO 179 ACRES)",
  "(180 TO 219 ACRES)", "(220 TO 259 ACRES)", "(260 TO 499 ACRES)",
  "(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)"
)
# cleaner facet labels for print
bin_label <- function(x) {
  x <- gsub(" ACRES", "", x); x <- gsub("[()]", "", x)
  gsub("\\.0", "", x)
}

# ---- geometries (loaded once, reused) -------------------------------
drop_fips <- c("02", "15", "60", "66", "69", "72", "78")
conus <- counties(cb = TRUE, resolution = "20m", year = 2022, class = "sf")
conus <- conus[!conus$STATEFP %in% drop_fips, c("GEOID", "geometry")]
names(conus)[1] <- "fips"
conus <- st_transform(conus, 5070)
states <- tigris::states(cb = TRUE, year = 2022, class = "sf")
states <- st_transform(states[!states$STATEFP %in% drop_fips, "geometry"], 5070)

# ---- shared theme ---------------------------------------------------
theme_map <- function() {
  theme_void(base_size = BASE_SIZE) +
    theme(
      legend.position = "right",
      strip.text = element_text(face = "bold", size = BASE_SIZE - 1),
      plot.title = element_text(face = "bold", size = BASE_SIZE + 3),
      plot.subtitle = element_text(size = BASE_SIZE - 1),
      plot.caption = element_text(size = BASE_SIZE - 2, colour = "grey40")
    )
}

# =====================================================================
# FIGURE 1 -- 12-panel county lollipop facet
# =====================================================================
panel <- as.data.table(readRDS("./data/county_panel.rds"))
panel[, bin := factor(bin, levels = bin_order, ordered = TRUE)]

ctr <- suppressWarnings(st_centroid(conus))
xy  <- st_coordinates(ctr)
cen <- data.table(fips = conus$fips, x = xy[, 1], y = xy[, 2])

STEM_MAX   <- 60000   # map units for a within-bin p95 stem
STEM_ALPHA <- 0.8

d <- panel[status != "suppressed_end" & !is.na(acre_shift) & acre_shift != 0]
d <- merge(d, cen, by = "fips")
# within-bin p95 cap, computed per facet so each class is legible
d[, cap := quantile(abs(acre_shift), 0.95, na.rm = TRUE), by = bin]
d[, len := pmin(abs(acre_shift) / cap, 1) * STEM_MAX]
d[, yend := y + fifelse(acre_shift > 0, len, -len)]
d[, dir := fifelse(acre_shift > 0, "Increase", "Decrease")]
d[, bin_f := factor(bin_label(as.character(bin)),
                    levels = bin_label(bin_order))]

fig1 <- ggplot() +
  geom_sf(data = states, fill = "grey97", colour = "grey75", linewidth = 0.12) +
  geom_segment(data = d, aes(x = x, y = y, xend = x, yend = yend, colour = dir),
               linewidth = 0.3, alpha = STEM_ALPHA) +
  geom_point(data = d, aes(x = x, y = yend, colour = dir), size = 0.35) +
  facet_wrap(~ bin_f, ncol = 3) +
  scale_colour_manual(values = c("Decrease" = PAL_NEG, "Increase" = PAL_POS),
                      name = NULL) +
  coord_sf(crs = 5070, datum = NA) +
  labs(
    title = "County change in irrigated acres by farm size class, 2012 to 2022",
    subtitle = "Stem length scaled within each class to its 95th-percentile change.",
    caption = "Counties suppressed in either year omitted."
  ) +
  theme_map()

ggsave(file.path(OUTDIR, "fig1_lollipop_facet.png"), fig1,
       width = 11, height = 10, dpi = DPI, bg = "white")
cat("wrote Figure 1\n")

# =====================================================================
# FIGURES 2 & 3 -- acres per operation, levels and change
# =====================================================================
bins <- as.data.table(readRDS("./data/bins_long.rds"))
MIN_CELLS <- 3
apo <- bins[cell == "observed" & year %in% c("2012", "2022"), .(
  acres = sum(acres, na.rm = TRUE),
  ops   = sum(ops,   na.rm = TRUE),
  ncell = .N
), by = .(fips, year)]
apo[, apo := acres / ops]
apo[, ok  := ncell >= MIN_CELLS]
apo_w <- dcast(apo, fips ~ year, value.var = c("apo", "ok"))
apo_w[, usable := ok_2012 == TRUE & ok_2022 == TRUE]
apo_w[usable == FALSE, c("apo_2012", "apo_2022") := NA_real_]
apo_w[, apo_change := apo_2022 - apo_2012]

m <- st_as_sf(merge(conus, apo_w, by = "fips", all.x = TRUE))

# ---- Figure 2: levels, 2012 | 2022 (sequential, log scale) ----------
h12 <- m["apo_2012"]; names(h12)[1] <- "apo"; h12$year <- "2012"
h22 <- m["apo_2022"]; names(h22)[1] <- "apo"; h22$year <- "2022"
lvl_long <- rbind(h12, h22)

fig2 <- ggplot(lvl_long) +
  geom_sf(aes(fill = apo), colour = NA) +
  geom_sf(data = states, fill = NA, colour = "grey40", linewidth = 0.12) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(trans = "log10", option = "D", na.value = PAL_NA,
                       name = "Acres per\noperation") +
  coord_sf(crs = 5070, datum = NA) +
  labs(
    title = "Irrigated acres per operation by county, 2012 and 2022",
    subtitle = "Counties with fewer than three observed size classes shown grey.",
    caption = "Observed cells only, matched numerator and denominator"
  ) +
  theme_map() +
  theme(strip.text = element_text(face = "bold", size = BASE_SIZE + 1))

ggsave(file.path(OUTDIR, "fig2_apo_levels.png"), fig2,
       width = 11, height = 5, dpi = DPI, bg = "white")
cat("wrote Figure 2\n")

# ---- Figure 3: change in APO (purple-green diverging) ---------------
brks <- c(-500, -100, -25, 0, 25, 100, 500)
fig3 <- ggplot(m) +
  geom_sf(aes(fill = signed_sqrt(apo_change)), colour = NA) +
  geom_sf(data = states, fill = NA, colour = "grey40", linewidth = 0.12) +
  scale_fill_gradient2(low = PAL_NEG, mid = PAL_MID, high = PAL_POS,
                       midpoint = 0, na.value = PAL_NA,
                       name = "Change in\nacres per\noperation",
                       breaks = signed_sqrt(brks), labels = brks) +
  coord_sf(crs = 5070, datum = NA) +
  labs(
    title = "Change in irrigated acres per operation by county, 2012 to 2022",
    caption = "Observed cells only, three or more size classes in both years"
  ) +
  theme_map()

ggsave(file.path(OUTDIR, "fig3_apo_change.png"), fig3,
       width = 9, height = 6, dpi = DPI, bg = "white")
cat("wrote Figure 3\n")

cat("\nAll manuscript figures written to", OUTDIR, "\n")
