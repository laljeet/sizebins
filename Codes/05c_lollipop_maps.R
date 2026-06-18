# =====================================================================
# 05c_lollipop_maps.R -- Stage 5c: county shift lollipop maps, per bin
# Project: structural shift in irrigated cropland by farm size bin
#
# One map per size bin. Each county with a KNOWN shift gets a vertical
# stem at its centroid: length proportional to the absolute acre change,
# pointing UP and blue for growth, DOWN and red for decline. This encodes
# direction AND magnitude in one mark, which a choropleth cannot: at the
# county level GROWTH counties outnumber DECLINE counties in every bin,
# yet the national total falls, because the declining counties lose far
# more acres each. Length-scaled marks make that visible (many short
# blue stems, fewer long red stems) instead of misleading by count.
#
# Scaling is WITHIN-BIN: bin magnitudes span 11 ac (smallest) to ~4,000
# ac (largest) at the median, so a shared scale is impossible. Each map
# is normalised to its own bin's 95th-percentile magnitude (outliers
# drawn at max length), with the legend stating the acre reference.
# Counties with suppressed/unknown shifts are omitted (no stem), not
# drawn at zero.
#
# Reads ./data/county_panel.rds (from 04c) + tigris geometries.
# Output: ./figures/lollipop_<bin>.png  (one per bin)
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(tigris)
  library(ggplot2)
})
options(tigris_use_cache = TRUE)

panel <- as.data.table(readRDS("./data/county_panel.rds"))

# ---- 1. CONUS county centroids + state outlines (equal-area) --------
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2022, class = "sf")
drop_fips <- c("02", "15", "60", "66", "69", "72", "78")
conus <- counties_sf[!counties_sf$STATEFP %in% drop_fips, c("GEOID", "geometry")]
names(conus)[1] <- "fips"
conus <- st_transform(conus, 5070)

states <- tigris::states(cb = TRUE, year = 2022, class = "sf")
states <- st_transform(states[!states$STATEFP %in% drop_fips, "geometry"], 5070)

# centroid coordinates per county (suppress the lon/lat warning; 5070 is planar)
ctr <- suppressWarnings(st_centroid(conus))
xy  <- st_coordinates(ctr)
cen <- data.table(fips = conus$fips, x = xy[, 1], y = xy[, 2])

# ---- 2. stem geometry: vertical, length scaled within-bin -----------
# stem half-height in map units. Map spans ~2.9M units tall (5070 metres);
# a full-length stem is ~2.5% of that so even max stems stay readable.
STEM_MAX <- 70000  # map units for a p95-magnitude stem
STEM_ALPHA <- 0.85 # stem opacity; lower (e.g. 0.6) reveals depth in dense clusters like the Delta

bin_order <- c(
  "(1.0 TO 9.9 ACRES)", "(10.0 TO 49.9 ACRES)", "(50.0 TO 69.9 ACRES)",
  "(70.0 TO 99.9 ACRES)", "(100 TO 139 ACRES)", "(140 TO 179 ACRES)",
  "(180 TO 219 ACRES)", "(220 TO 259 ACRES)", "(260 TO 499 ACRES)",
  "(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)"
)

make_map <- function(bn) {
  d <- panel[bin == bn & status != "suppressed_end" & !is.na(acre_shift) & acre_shift != 0]
  d <- merge(d, cen, by = "fips")
  if (nrow(d) == 0) { cat("no data for", bn, "\n"); return(invisible()) }

  # within-bin p95 cap; outliers clamp to full length
  cap <- quantile(abs(d$acre_shift), 0.95, na.rm = TRUE)
  d[, len := pmin(abs(acre_shift) / cap, 1) * STEM_MAX]
  d[, yend := y + fifelse(acre_shift > 0, len, -len)]
  d[, dir := fifelse(acre_shift > 0, "Growth", "Decline")]

  ndec <- d[dir == "Decline", .N]; ngro <- d[dir == "Growth", .N]
  net  <- sum(d$acre_shift) / 1e3  # k acres, known cells

  p <- ggplot() +
    geom_sf(data = states, fill = "grey97", colour = "grey70", linewidth = 0.2) +
    geom_segment(data = d, aes(x = x, y = y, xend = x, yend = yend, colour = dir),
                 linewidth = 0.35, alpha = STEM_ALPHA) +
    geom_point(data = d, aes(x = x, y = yend, colour = dir), size = 0.45) +
    scale_colour_manual(values = c("Decline" = "#b2182b", "Growth" = "#2166ac"),
                        name = NULL) +
    coord_sf(crs = 5070, datum = NA) +
    labs(
      title = paste0("County change in irrigated acres: ", bn, " bin, 2012-2022"),
      subtitle = sprintf("Stem length = acres changed (full = %s ac). Up/blue grew, down/red declined.  %d decline vs %d growth counties; net %+.0fk ac.",
                         format(round(cap), big.mark = ","), ndec, ngro, net),
      caption = "Suppressed counties omitted. Length scaled within bin to the 95th percentile. Equal-area projection (EPSG:5070)."
    ) +
    theme_void(base_size = 10) +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8))

  safe <- gsub("[^0-9A-Za-z]+", "_", bn)
  out  <- paste0("./figures/lollipop_", safe, ".png")
  ggsave(out, p, width = 10, height = 6.5, dpi = 300, bg = "white")
  cat("wrote", out, "\n")
}

# ---- 3. generate all 12 ---------------------------------------------
dir.create("./figures", showWarnings = FALSE)
invisible(lapply(bin_order, make_map))
cat("\nall bin maps written to ./figures/\n")
