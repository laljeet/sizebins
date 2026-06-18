# ============================================================
# 01_pull_quickstats.R   (simple: 6 calls, year x short_desc)
# Pull irrigated ag-land (acres + operations), county level,
# all CONUS at once, for 2012/2017/2022, from NASS via rnassqs.
#
# Output: ./data/raw_quickstats_api.rds
# Validates the API 2022 pull against the bulk-file slice.
# ============================================================

library(rnassqs)
library(tidyverse)

# ---- auth ---------------------------------------------------
key <- Sys.getenv("NASSQS_TOKEN")
if (key == "") key <- Sys.getenv("NASS_API_KEY")
stopifnot(nchar(key) > 0)
nassqs_auth(key = key)

years <- c(2012, 2017, 2022)
short_descs <- c("AG LAND, IRRIGATED - ACRES",
                 "AG LAND, IRRIGATED - NUMBER OF OPERATIONS")

# CONUS only: pull all counties nationally, then drop AK/HI/terr
# by state FIPS after the fact (one national call per year-stat).
drop_fips <- c("02","15","60","66","69","72","78")  # AK, HI, territories

# ---- pull: one call per year per short_desc (6 total) -------
pull_year_sd <- function(yr, sd) {
  params <- list(
    source_desc    = "CENSUS",
    short_desc     = sd,
    agg_level_desc = "COUNTY",
    year           = yr
  )
  n <- tryCatch(as.integer(nassqs_record_count(params)$count),
                error = function(e) NA_integer_)
  message(sprintf("  %d | %-45s | records: %s", yr, sd, n))
  out <- nassqs(params)
  out
}

message("Pulling 6 calls (3 years x 2 short_descs)...")
raw_list <- list()
for (yr in years) {
  for (sd in short_descs) {
    raw_list[[paste(yr, sd)]] <-
      tryCatch(pull_year_sd(yr, sd),
               error = function(e) {
                 warning("FAILED ", yr, " ", sd, ": ", conditionMessage(e))
                 NULL
               })
  }
}
raw <- bind_rows(raw_list)
names(raw) <- tolower(names(raw))
message("Pulled rows (pre-CONUS-filter): ", nrow(raw))

# ---- CONUS filter + normalize ------------------------------
raw_tidy <- raw %>%
  filter(!state_fips_code %in% drop_fips) %>%
  transmute(
    year,
    state_fips = state_fips_code,
    county_code,
    fips = paste0(state_fips_code, county_code),
    state_alpha,
    county_name,
    asd_desc,
    short_desc,
    domain_desc,
    domaincat_desc,
    statisticcat_desc,
    unit_desc,
    value_raw = value
  )

saveRDS(raw_tidy, "./data/raw_quickstats_api.rds")
message("Saved -> ./data/raw_quickstats_api.rds  (rows: ", nrow(raw_tidy), ")")
write_csv(raw_tidy,"./data/raw_tidy.csv")
cat("\n=== rows by year x domain ===\n")
print(raw_tidy %>% count(year, domain_desc))

# ============================================================
# VALIDATION: API 2022 vs bulk-file 2022 slice
# ============================================================
bulk_path <- "./data/irr_2022_county_raw.rds"
if (file.exists(bulk_path)) {
  bulk <- readRDS(bulk_path) %>% as_tibble()
  names(bulk) <- tolower(names(bulk))

  api22 <- raw %>%
    filter(year == 2022, !state_fips_code %in% drop_fips) %>%
    mutate(fips = paste0(state_fips_code, county_code),
           v = str_trim(value)) %>%
    select(fips, short_desc, domain_desc, domaincat_desc, v) %>%
    arrange(fips, short_desc, domain_desc, domaincat_desc)

  bulk22 <- bulk %>%
    filter(agg_level_desc == "COUNTY", !state_fips_code %in% drop_fips) %>%
    mutate(fips = paste0(state_fips_code, county_code),
           v = str_trim(value)) %>%
    select(fips, short_desc, domain_desc, domaincat_desc, v) %>%
    arrange(fips, short_desc, domain_desc, domaincat_desc)

  cat("\n=== VALIDATION: API 2022 vs bulk 2022 ===\n")
  cat("API rows:  ", nrow(api22), "\n")
  cat("Bulk rows: ", nrow(bulk22), "\n")

  cmp <- full_join(api22, bulk22,
                   by = c("fips","short_desc","domain_desc","domaincat_desc"),
                   suffix = c("_api","_bulk"))
  mism <- cmp %>% filter(is.na(v_api) | is.na(v_bulk) | v_api != v_bulk)
  cat("Mismatched / unmatched cells: ", nrow(mism), "\n")
  if (nrow(mism) > 0) print(head(mism, 20)) else
    cat("PERFECT MATCH. API path validated.\n")
} else {
  message("Bulk slice not found - skipping validation.")
}
