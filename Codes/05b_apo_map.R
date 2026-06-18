# =====================================================================
# 05b_apo_map.R -- Stage 5b: national acres-per-operation map
# Project: structural shift in irrigated cropland by farm size bin
#
# The summary spatial figure. Average irrigated acres per operation, by
# county, 2012 and 2022 side by side, plus the change. This is the size
# dimension projected onto space: where operations are large vs small and
# whether they grew. It is consolidation in one view, and it stays
# on-thesis because APO IS the size variable -- this is not a map of
# "where irrigation is".
#
# APO uses observed cells only with matched numerator/denominator
# (observed acres / observed operations). A county needs >=3 observed
# cells in BOTH years to be mapped; sparser counties are drawn grey
# ("insufficient data") rather than shown as noise. An audit found 1,608
# counties clear this floor.
#
# Reads ./data/bins_long.rds + tigris geometries.
# Outputs: ./figures/fig_apo_levels.png  (2012 | 2022)
#          ./figures/fig_apo_change.png  (per-operation change)
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(tigris)
  library(ggplot2)
  library(patchwork)
})
options(tigris_use_cache = TRUE)

bins <- as.data.table(readRDS("./data/bins_long.rds"))

# ---- 1. county APO, observed cells only, with cell-count floor ------
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

cat("counties usable (>=", MIN_CELLS, "observed cells both years):",
    apo_w[usable == TRUE, .N], "\n")

# ---- 2. geometries (equal-area) -------------------------------------
counties_sf <- counties(cb = TRUE, resolution = "20m", year = 2022, class = "sf")
drop_fips <- c("02", "15", "60", "66", "69", "72", "78")
conus <- counties_sf[!counties_sf$STATEFP %in% drop_fips, c("GEOID", "geometry")]
names(conus)[1] <- "fips"
conus <- st_transform(conus, 5070)
states <- tigris::states(cb = TRUE, year = 2022, class = "sf")
states <- st_transform(states[!states$STATEFP %in% drop_fips, "geometry"], 5070)

m <- merge(conus, apo_w, by = "fips", all.x = TRUE)
m <- st_as_sf(m)

# ---- 3. levels map: 2012 | 2022 -------------------------------------
# log scale: APO spans single-digit to >1,000 ac/op.
# Build each year's layer with IDENTICAL column names, then stack. Using
# differently-named source columns (apo_2012 vs apo_2022) directly breaks
# rbind, so each half is reduced to (apo, year, geometry) first.
h2012 <- m["apo_2012"]; names(h2012)[1] <- "apo"; h2012$year <- "2012"
h2022 <- m["apo_2022"]; names(h2022)[1] <- "apo"; h2022$year <- "2022"
long <- rbind(h2012, h2022)

lvl <- ggplot(long) +
  geom_sf(aes(fill = apo), colour = NA) +
  geom_sf(data = states, fill = NA, colour = "grey40", linewidth = 0.15) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(trans = "log10", na.value = "grey85",
                       name = "Acres per\noperation") +
  labs(title = "Average irrigated acres per operation by county",
       subtitle = "2012 and 2022; grey = fewer than 3 observed size classes",
       caption = "Observed cells only, matched numerator/denominator. Equal-area projection (EPSG:5070).") +
  theme_void(base_size = 9) +
  theme(legend.position = "right", strip.text = element_text(face = "bold"))

ggsave("./figures/fig_apo_levels.png", lvl, width = 12, height = 5, dpi = 300, bg = "white")

# ---- 4. change map --------------------------------------------------
signed_sqrt <- function(x) sign(x) * sqrt(abs(x))
brks <- c(-500, -100, -25, 0, 25, 100, 500)
chg <- ggplot(m) +
  geom_sf(aes(fill = signed_sqrt(apo_change)), colour = NA) +
  geom_sf(data = states, fill = NA, colour = "grey40", linewidth = 0.15) +
  scale_fill_gradient2(low = "#762a83", mid = "#f7f7f7", high = "#1b7837",
                       midpoint = 0, na.value = "grey85",
                       name = "Change in\nac/operation",
                       breaks = signed_sqrt(brks), labels = brks) +
  labs(title = "Change in irrigated acres per operation by county, 2012-2022",
       subtitle = "Green = operations grew larger; purple = operations shrank",
       caption = "Observed cells only, >=3 size classes both years. Equal-area projection (EPSG:5070).") +
  theme_void(base_size = 9) + theme(legend.position = "right")

ggsave("./figures/fig_apo_change.png", chg, width = 9, height = 6, dpi = 300, bg = "white")
cat("wrote ./figures/fig_apo_levels.png and ./figures/fig_apo_change.png\n")
