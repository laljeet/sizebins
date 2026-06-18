# Irrigated cropland by farm size class, United States, 2012 to 2022

County-level analysis of how irrigated cropland shifted across twelve
farm-size classes between the 2012 and 2022 Census of Agriculture. The
size class is the unit of analysis. The headline finding: irrigated
acreage fell in every class from 10 to 1,999 acres and rose only in the
2,000-acre-or-more class, a shift that holds under bounds on the
suppressed census cells and resolves into distinct regional patterns.

**Journal:** TBD

## Interactive map

A lollipop map of county-level acreage change, with a toggle for each
size class, is hosted at:
https://laljeet.github.io/sizebins/

## Repository layout

- `Codes/` — R analysis pipeline and figure/table/map scripts
- `supplement/` — the interactive map (`irrigated_size_bins_map.html`)

## Pipeline

The scripts run in order and rebuild every number, figure, and table
from the raw Census extract.

- `01_parse_audit.R` — parse the Census extract to `bins_long.rds`,
  check bin sums against published county totals
- `02_bin_shift.R` — observed acreage shift per class, with floor and
  ceiling bounds on the suppressed cells
- `03b_acres_per_operation.R` — acres per operation, observed cells only
- `04b_state_regional_summary.R` — state-level regime summary
- `04c_county_panel_corrected.R` — county panel (suppressed cells held
  as missing, never zero)
- `tables_for_manuscript.R` — regenerates Tables 1 to 4
- `figures_for_manuscript.R` — manuscript figures (lollipop, acres per
  operation levels and change)
- `supplement_leaflet.R` — builds the interactive map above
- `verify_national_accounting.R` — lost / gained / net balance check

## Method note

Suppressed acreage cells are bounded, not imputed. Each suppressed cell
is framed by its published operation count times the class lower and
upper limits. A class is assigned a definitive direction only when both
bounds change in the same sign. Imputing the size distribution under
study would build the conclusion into its inputs, so the analysis
reports only the range the masked cells can occupy.

## Data

USDA Census of Agriculture, area-operated irrigated-land tables (2012,
2017, 2022), retrieved from NASS Quick Stats. Conterminous United
States only.
