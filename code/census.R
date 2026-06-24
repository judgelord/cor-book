##### Devin's modification of Ellie's modification of Justin's Census.R Code
##### Updated June 23, 2026

############################################################
# 1. Packages
############################################################

library(tidyverse)
library(tidycensus)
library(janitor)
library(tidyr)
library(tibble)
library(here)
library(knitr)


############################################################
# 1A. Census API key
############################################################

census_API_key <- read_file(here::here(".secrets", "census_API_key")) |>
  str_remove("\n")

census_api_key(census_API_key)


############################################################
# 1B. Optional: inspect available variables
############################################################

v22 <- load_variables(2022, "acs5")

v22 |>
  filter(
    concept %in% c(
      "Educational Attainment by Employment Status for the Population 25 to 64 Years",
      "Public Assistance Income or Food Stamps/SNAP in the Past 12 Months for Households"
    ) |
      str_detect(label, "Federal government workers|Public Assistance Income or Food|for-profit"),
    geography %in% c("tract", "block group")
  ) |>
  kable()


############################################################
# 2. User inputs
############################################################

# Named ACS variable vector.
#
# Left side  = final variable name used in your output dataframe.
# Right side = ACS variable ID.
#
# Example:
#   bachelor_or_higher = "B23006_023"
#
# Final output columns:
#   bachelor_or_higher_estimate
#   bachelor_or_higher_moe

acs_variables <- c(
  population_total = "B01003_001",
  public_assistance_or_snap_households = "B19058_002",
  # education
  education_total = "B23006_001",
  education_bachelor_or_higher = "B23006_023",
  # employment
  workers_total = "C24060_001",
  workers_federal = "B24081_008", # Class of Worker by Median Earnings in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) for the Civilian Employed Population 16 Years and Over
  workers_nonprofit = "C24060_019",
  workers_self_employed = "C24060_013",
  workers_private_sector = "C24060_007",
  workers_government = "C24060_025",
  workers_nonprofit = "C24070_043"
)

# Years to pull.
years_to_pull <- 2010:2024

# ACS survey.
acs_survey <- "acs5"

# Metadata year used for labels/concepts in the crosswalk.
metadata_year <- 2022


############################################################
# 2A. Optional test
############################################################

# This should work. Important: do not use output = "long".
get_acs(
  geography = "congressional district",
  variables = "B19058_002",
  state = "WI",
  year = 2010,
  survey = "acs5"
)


############################################################
# 2B. Check whether variables are available by year
############################################################

check_acs_vars_by_year <- function(years, variables, survey = "acs5") {
  variable_ids <- unname(variables)

  map_dfr(years, function(yr) {
    metadata <- load_variables(
      year = yr,
      dataset = survey
    )

    tibble(
      year = yr,
      variable_name = names(variables),
      variable = variable_ids,
      available = variable_ids %in% metadata$name
    )
  })
}

availability_check <- check_acs_vars_by_year(
  years = years_to_pull,
  variables = acs_variables,
  survey = acs_survey
)

# Show unavailable variables, if any.
availability_check |>
  filter(!available)


############################################################
# 3. Helper function: clean user-supplied variable names
############################################################

clean_user_variable_names <- function(x) {
  x |>
    make_clean_names() |>
    make.unique(sep = "_")
}


############################################################
# 4. Build variable lookup from named vector plus metadata
############################################################

make_var_lookup_from_named_vector <- function(
    variables,
    metadata_year = 2022,
    survey = "acs5"
) {
  # Require names on the vector.
  if (is.null(names(variables)) || any(names(variables) == "")) {
    stop(
      "`variables` must be a named character vector.\n\n",
      "Example:\n",
      "acs_variables <- c(\n",
      "  bachelor_or_higher = 'B23006_023',\n",
      "  total_population = 'B01003_001'\n",
      ")"
    )
  }

  # Load metadata for labels/concepts.
  metadata <- load_variables(
    year = metadata_year,
    dataset = survey
  )

  # Create lookup, preserving user input order.
  lookup <- tibble(
    stable_name_raw = names(variables),
    variable = unname(variables),
    input_order = seq_along(variables)
  ) |>
    mutate(
      stable_name = clean_user_variable_names(stable_name_raw)
    ) |>
    left_join(
      metadata |>
        select(
          variable = name,
          label,
          concept
        ),
      by = "variable"
    ) |>
    arrange(input_order)

  # Stop if any requested variables are not in the metadata year.
  missing_vars <- lookup |>
    filter(is.na(label)) |>
    pull(variable)

  if (length(missing_vars) > 0) {
    stop(
      "The following variables were not found in the ",
      metadata_year,
      " ",
      survey,
      " metadata: ",
      paste(missing_vars, collapse = ", ")
    )
  }

  lookup |>
    mutate(
      metadata_year = metadata_year
    ) |>
    select(
      variable,
      stable_name,
      stable_name_raw,
      label,
      concept,
      metadata_year
    )
}


############################################################
# 5. Pull ACS data for one year
############################################################

pull_acs_one_year <- function(
    year,
    geography,
    variables,
    survey = "acs5",
    metadata_year = 2022,
    wide = TRUE,
    cache_table = TRUE,
    ...
) {
  # Create lookup using user-specified names plus metadata labels.
  lookup <- make_var_lookup_from_named_vector(
    variables = variables,
    metadata_year = metadata_year,
    survey = survey
  )

  # Check whether variables are available in this ACS year.
  year_metadata <- load_variables(
    year = year,
    dataset = survey
  )

  missing_for_year <- setdiff(lookup$variable, year_metadata$name)

  if (length(missing_for_year) > 0) {
    stop(
      "The following variables are in the ",
      metadata_year,
      " metadata but are not available in the ",
      year,
      " ",
      survey,
      " API: ",
      paste(missing_for_year, collapse = ", "),
      "\n\n",
      "This usually means the variable was introduced later, renamed, ",
      "renumbered, or otherwise changed across ACS vintages. ",
      "Use a later start year or create a year-specific variable crosswalk."
    )
  }

  # Pull ACS data.
  #
  # Important:
  # Do NOT use output = "long".
  #
  # The default get_acs() output already has:
  #   GEOID, NAME, variable, estimate, moe
  #
  # Using output = "long" can trigger the tidycensus error:
  #   object 'dat2' not found
  # especially for older congressional district pulls.
  raw <- get_acs(
    geography = geography,
    variables = unique(lookup$variable),
    year = year,
    survey = survey,
    cache_table = cache_table,
    ...
  )

  # Join user names and metadata back onto the ACS pull.
  out_tidy <- raw |>
    left_join(lookup, by = "variable") |>
    mutate(
      year = year,
      geography_type = geography,
      .before = 1
    )

  # Return tidy/default tidycensus format if requested.
  if (!wide) {
    return(out_tidy)
  }

  # Pivot to wide format.
  #
  # Final columns are based on the user-specified names:
  #   bachelor_or_higher_estimate
  #   bachelor_or_higher_moe
  out_wide <- out_tidy |>
    select(
      year,
      geography_type,
      GEOID,
      NAME,
      stable_name,
      estimate,
      moe
    ) |>
    pivot_wider(
      names_from = stable_name,
      values_from = c(estimate, moe),
      names_glue = "{stable_name}_{.value}"
    )

  return(out_wide)
}


############################################################
# 6. Pull ACS data for multiple years
############################################################

pull_acs_years <- function(
    years,
    geography,
    variables,
    survey = "acs5",
    metadata_year = 2022,
    wide = TRUE,
    cache_table = TRUE,
    ...
) {
  map_dfr(
    years,
    \(yr) pull_acs_one_year(
      year = yr,
      geography = geography,
      variables = variables,
      survey = survey,
      metadata_year = metadata_year,
      wide = wide,
      cache_table = cache_table,
      ...
    )
  )
}


############################################################
# 7. Create variable-label crosswalk
############################################################

# This lets you inspect:
#   - your chosen final variable name
#   - ACS variable ID
#   - ACS metadata label
#   - ACS concept
#   - final estimate/moe column names

acs_variable_label_crosswalk <- make_var_lookup_from_named_vector(
  variables = acs_variables,
  metadata_year = metadata_year,
  survey = acs_survey
) |>
  mutate(
    estimate_column = paste0(stable_name, "_estimate"),
    moe_column = paste0(stable_name, "_moe")
  )

acs_variable_label_crosswalk |>
  kable()


############################################################
# 8. Pull House/congressional district data
############################################################

acs_house <- pull_acs_years(
  years = years_to_pull,
  geography = "congressional district",
  variables = acs_variables,
  survey = acs_survey,
  metadata_year = metadata_year,
  wide = TRUE
)


############################################################
# 9. Pull Senate/state data
############################################################

acs_senate <- pull_acs_years(
  years = years_to_pull,
  geography = "state",
  variables = acs_variables,
  survey = acs_survey,
  metadata_year = metadata_year,
  wide = TRUE
)


############################################################
# 10. Quick checks
############################################################

glimpse(acs_house)
glimpse(acs_senate)

acs_variable_label_crosswalk

############################################################
# 9. Save output
############################################################
library(dplyr)
library(stringr)
library(tibble)

state_fips_xwalk <- tribble(
  ~state_abbrev, ~state_fips,
  "AL", "01",
  "AK", "02",
  "AZ", "04",
  "AR", "05",
  "CA", "06",
  "CO", "08",
  "CT", "09",
  "DE", "10",
  "DC", "11",
  "FL", "12",
  "GA", "13",
  "HI", "15",
  "ID", "16",
  "IL", "17",
  "IN", "18",
  "IA", "19",
  "KS", "20",
  "KY", "21",
  "LA", "22",
  "ME", "23",
  "MD", "24",
  "MA", "25",
  "MI", "26",
  "MN", "27",
  "MS", "28",
  "MO", "29",
  "MT", "30",
  "NE", "31",
  "NV", "32",
  "NH", "33",
  "NJ", "34",
  "NM", "35",
  "NY", "36",
  "NC", "37",
  "ND", "38",
  "OH", "39",
  "OK", "40",
  "OR", "41",
  "PA", "42",
  "RI", "44",
  "SC", "45",
  "SD", "46",
  "TN", "47",
  "TX", "48",
  "UT", "49",
  "VT", "50",
  "VA", "51",
  "WA", "53",
  "WV", "54",
  "WI", "55",
  "WY", "56",
  "AS", "60",
  "GU", "66",
  "MP", "69",
  "PR", "72",
  "VI", "78"
)


acs_variables

census <- full_join(acs_house |> mutate(chamber = "House") |>
                      mutate(
                        state_fips = str_sub(GEOID, 1, 2),
                        district_code = as.integer(str_sub(GEOID, 3, 4)),
                        district_code = if_else(district_code == 0L, 1L, district_code)
                      ),
                    acs_senate |> mutate(chamber = "Senate") |>
                      mutate(
                        state_fips = GEOID,
                        district_code = 0
                      )
                    ) |>
  left_join(state_fips_xwalk, by = "state_fips")

census |>
  select(year, GEOID, NAME, state_fips, state_abbrev, district_code)

names(census) <- names(census) |>
  stringr::str_remove("_estimate")

census <- census |>
  mutate(
    percent_public_assistance_or_snap_households =
      public_assistance_or_snap_households / population_total,

    percent_education_bachelor_or_higher =
      education_bachelor_or_higher / education_total,

    percent_workers_federal =
      workers_federal / workers_total,

    percent_workers_nonprofit =
      workers_nonprofit / workers_total,

    percent_workers_self_employed =
      workers_self_employed / workers_total,

    percent_workers_private_sector =
      workers_private_sector / workers_total,

    percent_workers_government =
      workers_government / workers_total
  )

census_variables <- acs_variable_label_crosswalk

save(census, census_variables, file =  here::here("data", "census", "census.rda"))

############################################################
# 10. Quick checks
############################################################
census |> filter(chamber == "Senate") |> select(year, GEOID, NAME)


glimpse(census)

acs_variable_label_crosswalk
