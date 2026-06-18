# =====================================================================
# supplement_leaflet.R  -- interactive supplementary map
# Project: structural shift in irrigated cropland by farm size bin
# Target journal: Land Use Policy (Elsevier), supplementary material
#
# Builds a single self-contained HTML file: a county choropleth of
# irrigated acreage change, with a layer toggle to switch the displayed
# size class (all twelve bins). Clicking a county opens a popup with the
# selected bin's 2012/2022 acres, change, and operator counts, plus a
# small horizontal bar chart of that county's change across all twelve
# size classes (diverging purple-green, zero line centred).
#
# No server required. The output .html opens in any browser and can be
# submitted as supplementary material.
#
# Reads ./data/county_panel.rds (from 04c) and ./data/bins_long.rds.
# Output: ./figures/supplement/irrigated_size_bins_map.html
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(tigris)
  library(leaflet)
  library(htmlwidgets)
})
options(tigris_use_cache = TRUE)

PAL_NEG <- "#762a83"   # purple = decrease
PAL_POS <- "#1b7837"   # green  = increase
OUTDIR  <- "./figures/supplement"
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

bin_order <- c(
  "(1.0 TO 9.9 ACRES)", "(10.0 TO 49.9 ACRES)", "(50.0 TO 69.9 ACRES)",
  "(70.0 TO 99.9 ACRES)", "(100 TO 139 ACRES)", "(140 TO 179 ACRES)",
  "(180 TO 219 ACRES)", "(220 TO 259 ACRES)", "(260 TO 499 ACRES)",
  "(500 TO 999 ACRES)", "(1,000 TO 1,999 ACRES)", "(2,000 OR MORE ACRES)"
)
short_lab <- function(x) {
  x <- gsub(" ACRES", "", x); x <- gsub("[()]", "", x)
  x <- gsub(" TO ", "-", x);  x <- gsub(" OR MORE", "+", x); x <- gsub("\\.0", "", x)
  x
}
labs <- short_lab(bin_order)

# ---- data: county x bin shift + operator counts ---------------------
panel <- as.data.table(readRDS("./data/county_panel.rds"))
bins  <- as.data.table(readRDS("./data/bins_long.rds"))

ops <- dcast(bins[year %in% c("2012","2022")], fips + bin ~ year, value.var = "ops")
setnames(ops, c("2012","2022"), c("ops12","ops22"), skip_absent = TRUE)
panel <- merge(panel, ops, by = c("fips","bin"), all.x = TRUE)

# wide matrices keyed by fips, one column per bin, for fast popup build
shift_w <- dcast(panel, fips ~ bin, value.var = "acre_shift")
a12_w   <- dcast(panel, fips ~ bin, value.var = "acres_2012")
a22_w   <- dcast(panel, fips ~ bin, value.var = "acres_2022")
op12_w  <- dcast(panel, fips ~ bin, value.var = "ops12")
op22_w  <- dcast(panel, fips ~ bin, value.var = "ops22")
for (dt in list(shift_w,a12_w,a22_w,op12_w,op22_w))
  setcolorder(dt, c("fips", bin_order[bin_order %in% names(dt)]))

# combined national: net acre change across all twelve classes per county
shift_w[, combined := rowSums(.SD, na.rm = TRUE), .SDcols = bin_order]

# ---- geometry (WGS84 for leaflet) -----------------------------------
drop_fips <- c("02","15","60","66","69","72","78")
conus <- counties(cb = TRUE, resolution = "20m", year = 2022, class = "sf")
conus <- conus[!conus$STATEFP %in% drop_fips, c("GEOID","NAME","STATE_NAME","geometry")]
names(conus)[1] <- "fips"
conus <- st_transform(conus, 4326)

# Simplify polygons to shrink the embedded HTML (national zoom does not
# need full coordinate precision). keep = 0.04 retains ~4% of vertices;
# keep_shapes = TRUE prevents small counties from vanishing. This is the
# main lever on output file size. Requires the rmapshaper package.
if (requireNamespace("rmapshaper", quietly = TRUE)) {
  conus <- rmapshaper::ms_simplify(conus, keep = 0.04, keep_shapes = TRUE)
} else {
  message("rmapshaper not installed; output HTML will be large. ",
          "Install with install.packages('rmapshaper') to shrink it.")
}

# ---- inline SVG horizontal bar chart per county ---------------------
# twelve bars, smallest class on top to largest on bottom, each row
# labelled at the left, centred zero line, purple (decrease) / green
# (increase). Labels make each bar self-identifying.
bar_labs <- c("1-9.9","10-49.9","50-69.9","70-99.9","100-139","140-179",
              "180-219","220-259","260-499","500-999","1,000-1,999","2,000+")
mk_svg <- function(vals) {
  vals[is.na(vals)] <- 0
  mx <- max(abs(vals)); if (mx == 0) mx <- 1
  gutter <- 100           # left space for row labels
  W <- 440; H <- 270; bh <- 15; gap <- 4.5; top <- 14
  midx <- gutter + (W - gutter) / 2          # zero line, centred in plot area
  halfspan <- (W - gutter) / 2 - 10
  rows <- vapply(seq_along(vals), function(i) {
    y <- top + (i-1)*(bh+gap)
    w <- (abs(vals[i])/mx) * halfspan
    x <- if (vals[i] >= 0) midx else midx - w
    col <- if (vals[i] >= 0) PAL_POS else PAL_NEG
    paste0(
      sprintf('<text x="%d" y="%.1f" font-size="13" text-anchor="end" fill="#444">%s</text>',
              gutter - 6, y + bh - 3, bar_labs[i]),
      sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%d" fill="%s"/>', x, y, w, bh, col)
    )
  }, character(1))
  paste0('<svg xmlns="http://www.w3.org/2000/svg" width="', W, '" height="', H, '">',
         sprintf('<line x1="%.1f" y1="8" x2="%.1f" y2="%d" stroke="#999" stroke-width="0.7"/>', midx, midx, H-16),
         paste(rows, collapse = ""),
         sprintf('<text x="%.1f" y="%d" font-size="12" fill="#888" text-anchor="middle">decrease &larr; | &rarr; increase (acres)</text>', midx, H-2),
         '</svg>')
}

# ---- build popup HTML per county (selected bin block + chart) -------
fmt <- function(x) ifelse(is.na(x), "suppressed", format(round(x), big.mark = ","))
popup_for <- function(i, sel_bin) {
  f <- shift_w$fips[i]
  nm <- conus$NAME[match(f, conus$fips)]; stt <- conus$STATE_NAME[match(f, conus$fips)]
  svg <- mk_svg(as.numeric(shift_w[i, ..bin_order]))
  if (is.null(sel_bin)) {
    # combined national view: net across all classes
    net <- shift_w$combined[i]
    head <- paste0(
      "<span style='color:#555'>all size classes combined</span><br>",
      "Net change: <b>", fmt(net), " ac</b><br>")
  } else {
    sb <- sel_bin
    a12 <- a12_w[[sb]][i]; a22 <- a22_w[[sb]][i]; ch <- shift_w[[sb]][i]
    o12 <- op12_w[[sb]][i]; o22 <- op22_w[[sb]][i]
    head <- paste0(
      "<span style='color:#555'>", short_lab(sb), " acre class</span><br>",
      "2012: ", fmt(a12), " ac &nbsp; 2022: ", fmt(a22), " ac<br>",
      "Change: <b>", fmt(ch), " ac</b><br>",
      "Operations: ", fmt(o12), " &rarr; ", fmt(o22), "<br>")
  }
  paste0(
    "<div style='font-family:sans-serif;font-size:15px;line-height:1.5;min-width:460px'>",
    "<b style='font-size:17px'>", nm, " County, ", stt, "</b><br>",
    head,
    "<div style='margin-top:10px;color:#555;font-size:14px'>Change across all size classes:</div>",
    svg,
    "</div>"
  )
}

# ---- assemble leaflet with one toggleable layer per bin -------------
pal_fun <- function(v) {
  rng <- max(abs(v), na.rm = TRUE); if (!is.finite(rng) || rng == 0) rng <- 1
  colorNumeric(c(PAL_NEG, "#f7f7f7", PAL_POS), domain = c(-rng, rng), na.color = "#dddddd")
}

m <- leaflet() |> addProviderTiles("CartoDB.Positron")

COMBINED_LAB <- "All classes (net)"

# combined national layer first (the default view)
vals_c <- shift_w$combined[match(conus$fips, shift_w$fips)]
pal_c  <- pal_fun(vals_c)
pop_c  <- vapply(seq_len(nrow(shift_w)), function(i) popup_for(i, NULL), character(1))
pop_cv <- pop_c[match(conus$fips, shift_w$fips)]
m <- m |> addPolygons(
  data = conus, group = COMBINED_LAB,
  fillColor = pal_c(vals_c), fillOpacity = 0.85,
  color = "white", weight = 0.3, popup = pop_cv,
  popupOptions = popupOptions(maxWidth = 520, minWidth = 480),
  label = ~paste0(NAME, ", ", STATE_NAME)
)

for (sb in bin_order) {
  vals <- shift_w[[sb]][match(conus$fips, shift_w$fips)]
  pal  <- pal_fun(vals)
  popups <- vapply(seq_len(nrow(shift_w)),
                   function(i) popup_for(i, sb), character(1))
  popv <- popups[match(conus$fips, shift_w$fips)]
  m <- m |> addPolygons(
    data = conus, group = short_lab(sb),
    fillColor = pal(vals), fillOpacity = 0.85,
    color = "white", weight = 0.3,
    popup = popv,
    popupOptions = popupOptions(maxWidth = 520, minWidth = 480),
    label = ~paste0(NAME, ", ", STATE_NAME)
  )
}

all_groups <- c(COMBINED_LAB, labs)
m <- m |>
  addLayersControl(
    baseGroups = all_groups,
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  addControl(
    html = "<b>Irrigated acreage change by farm size class, 2012-2022</b><br><span style='font-size:11px'>Showing all classes combined. Select a size class at right. Click a county for detail. Green = gain, purple = loss.</span>",
    position = "topleft"
  ) |>
  hideGroup(labs)   # show the combined national layer first

out <- file.path(OUTDIR, "irrigated_size_bins_map.html")
saveWidget(m, out, selfcontained = TRUE, title = "Irrigated cropland by farm size class")
cat("wrote", out, "\n")
