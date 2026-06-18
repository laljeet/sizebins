# =====================================================================
# 06_interactive_leaflet_map.R -- Stage 6: Interactive Web Map
# Project: structural shift in irrigated cropland by farm size bin
#
# This script generates a standalone interactive HTML Leaflet map.
# Users can select a size bin via a toggle menu and click on any
# county to view the exact bounding metrics and operator shifts.
#
# Outputs:
#   ./results/interactive_consolidation_map.html
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(leaflet)
  library(htmlwidgets)
  library(htmltools)
  library(tigris)
})

# ---- 1. load and prep the spatial panel -----------------------------
spatial_panel <- readRDS("./data/spatial_panel.rds")
spatial_panel <- st_transform(spatial_panel, 4326)

# Explicitly set the factor levels so Leaflet doesn't alphabetize them
my_levels <- c("Definitive Decline", "Definitive Growth", "Ambiguous/Stable")
spatial_panel$robust_dir <- factor(spatial_panel$robust_dir, levels = my_levels)

# ---- 2. fetch human-readable county names ---------------------------
cat("\nFetching county names for pop-ups...\n")
options(tigris_use_cache = TRUE)
county_names <- counties(cb = TRUE, resolution = "20m", year = 2022)
county_names <- as.data.frame(county_names)[, c("GEOID", "NAMELSAD", "STATE_NAME")]
names(county_names)[1] <- "fips"

spatial_panel <- merge(spatial_panel, county_names, by = "fips", all.x = TRUE)

# ---- 3. configure the map aesthetics --------------------------------
# Force Leaflet to respect our exact factor levels instead of sorting alphabetically
pal <- colorFactor(
  palette = c("#d95f02", "#1b9e77", "#cccccc"),
  domain = factor(my_levels, levels = my_levels),
  na.color = "transparent"
)

bin_levels <- levels(spatial_panel$bin)

l_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Base Map")

# ---- 4. build the map layers dynamically ----------------------------
cat("Building interactive map layers...\n")

for (current_bin in bin_levels) {

  bin_data <- subset(spatial_panel, bin == current_bin)

  # Rich HTML pop-up
  popup_content <- paste0(
    "<div style='font-family: Arial; font-size: 13px;'>",
    "<b>", bin_data$NAMELSAD, ", ", bin_data$STATE_NAME, "</b><br/>",
    "<hr style='margin: 4px 0;'/>",
    "<b>Size Class:</b> ", current_bin, "<br/>",
    "<b>Status:</b> <span style='color:",
    ifelse(bin_data$robust_dir == "Definitive Decline", "#d95f02",
           ifelse(bin_data$robust_dir == "Definitive Growth", "#1b9e77", "#555555")),
    "; font-weight: bold;'>", bin_data$robust_dir, "</span><br/>",
    "<br/>",
    "<b>Operator Shift:</b> ", format(bin_data$ops_net, big.mark = ","), " farms<br/>",
    "<b>Acreage Floor Shift:</b> ", format(round(bin_data$shift_floor, 1), big.mark = ","), " ac<br/>",
    "<b>Acreage Ceiling Shift:</b> ", format(round(bin_data$shift_ceil, 1), big.mark = ","), " ac<br/>",
    "</div>"
  )

  l_map <- l_map %>%
    addPolygons(
      data = bin_data,
      fillColor = ~pal(robust_dir),
      weight = 0.5,
      color = "#ffffff",
      fillOpacity = 0.8,
      group = current_bin,
      popup = popup_content,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#333333",
        fillOpacity = 1,
        bringToFront = TRUE
      )
    )
}

# ---- 5. add controls and legend -------------------------------------
l_map <- l_map %>%
  addLayersControl(
    baseGroups = bin_levels,
    options = layersControlOptions(collapsed = FALSE),
    position = "topright"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = factor(spatial_panel$robust_dir, levels = my_levels),
    title = "Bounding Result", # Dropped "Strict"
    opacity = 1
  )

# ---- 6. save the widget ---------------------------------------------
out_path <- "./results/interactive_consolidation_map.html"
saveWidget(l_map, file = out_path, selfcontained = TRUE)

cat("\nInteractive map successfully saved to:", out_path, "\n")
