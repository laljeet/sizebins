# =====================================================================
# 05d_direction_maps.R -- Stage 5d: two-color direction choropleth, per bin
# Project: structural shift in irrigated cropland by farm size bin
#
# Comparison candidate to the lollipop (05c). Each county is filled by the
# DIRECTION of its acre shift in that bin: blue = growth, red = decline,
# grey = suppressed/unknown, faint = no large change. This deliberately
# drops magnitude (the table carries magnitude; the POC showed county
# magnitudes are diffuse, not concentrated). The map's only job here is to
# show WHERE decline vs growth counties sit -- the spatial clustering that
# Moran's I quantifies -- so direction-only is the honest encoding.
#
# Suppressed counties are grey, NOT zero (the 04c correction).
#
# Reads ./data/county_panel.rds (from 04c) + tigris geometries.
# Output: ./figures/direction_<bin>.png  (one per bin)
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(tigris)
  library(ggplot2)
})
options(tigris_use_cache = TRUE)

panel <- as.data.table(readRDS("./data/county_panel.rds"))

# ---- 1. CONUS geometries (equal-area) -------------------------------
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2022, class = "sf")
drop_fips <- c("02", "15", "60", "66", "69", "72", "78")
conus <- counties_sf[!counties_sf$STATEFP %in% drop_fips, c("GEOID", "geometry")]
names(conus)[1] <- "fips"
conus <- st_transform(conus, 5070)
states <- tigris::states(cb = TRUE, year = 2022, class = "sf")
states <- st_transform(states[!states$STATEFP %in% drop_fips, "geometry"], 5070)

bin_order <- c(
  "(1.0 TO 9.9 ACRES)", "(10.0 TO 49.9 ACRES)", "(50.0 TO 69.9 ACRES)",
  "(70.0 TO 99.9 ACRES)", "(100 TO 139 ACRES)", "(140 TO 179 ACRES)",
  "(180 TO 219 ACRES)", "(220 TO 259 ACRES)", "(260 TO 499 ACRES)",
  "(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)"
)

# ---- 2. classify each county's direction within a bin ---------------
classify <- function(dt) {
  dt[, category := fifelse(status == "suppressed_end" | is.na(acre_shift),
                           "Suppressed / unknown",
                           fifelse(abs(acre_shift) < 1, "No large change",
                                   fifelse(acre_shift < 0, "Decline", "Growth")))]
  dt
}

dir_cols <- c("Decline" = "#b2182b", "Growth" = "#2166ac",
              "No large change" = "#f7f7f7", "Suppressed / unknown" = "grey80")

make_map <- function(bn) {
  d <- classify(panel[bin == bn])
  m <- merge(conus, d[, .(fips, category)], by = "fips", all.x = TRUE)
  m$category[is.na(m$category)] <- "Suppressed / unknown"
  m$category <- factor(m$category, levels = names(dir_cols))
  m <- st_as_sf(m)

  ndec <- sum(d$category == "Decline"); ngro <- sum(d$category == "Growth")
  net  <- sum(d$acre_shift, na.rm = TRUE) / 1e3

  p <- ggplot(m) +
    geom_sf(aes(fill = category), colour = "grey60", linewidth = 0.05) +
    geom_sf(data = states, fill = NA, colour = "grey30", linewidth = 0.2) +
    scale_fill_manual(values = dir_cols, drop = FALSE, name = NULL) +
    coord_sf(crs = 5070, datum = NA) +
    labs(
      title = paste0("County direction of change: ", bn, " bin, 2012-2022"),
      subtitle = sprintf("%d decline vs %d growth counties; net %+.0fk ac (known cells).",
                         ndec, ngro, net),
      caption = "Direction only; magnitude in the bin-shift table. Suppressed counties grey, not zero. Equal-area (EPSG:5070)."
    ) +
    theme_void(base_size = 10) +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8))

  safe <- gsub("[^0-9A-Za-z]+", "_", bn)
  out  <- paste0("./figures/direction_", safe, ".png")
  ggsave(out, p, width = 10, height = 6.5, dpi = 300, bg = "white")
  cat("wrote", out, "\n")
}

dir.create("./figures", showWarnings = FALSE)
invisible(lapply(bin_order, make_map))
cat("\nall direction maps written to ./figures/\n")
