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

##API Key
census_API_key <- read_file(here::here(".secrets", "census_API_key")) |> str_remove("\n")
census_api_key(census_API_key)



# Final wide-column names are automatically generated from
# the 2022 ACS variable metadata label associated with each
# variable name.
#
# Example:
#   B01001A_002
#   label in 2022 metadata: "Estimate!!Total:!!Male:"
#   generated name: total_male_estimate, total_male_moe
############################################################
# available variables
v22 <- load_variables(2022, "acs5")

v22 |>
  filter(
    concept %in% c(
    "Educational Attainment by Employment Status for the Population 25 to 64 Years",
    "Public Assistance Income or Food Stamps/SNAP in the Past 12 Months for Households"
    ) | str_detect(label, "Federal government workers|Public Assistance Income or Food|for-profit"),
    geography %in% c("tract", "block group")
      ) |>
  kable()


############################################################
# 2. User inputs
############################################################

acs_variable_names <- c(
  "B23006_023",
  "B24081_008",
  "B19058_002",
  "B23006_001",
  "B01003_001", # pop
  "C24060_001", # total employed
  "C24070_043", # nonprofit employment
  "C24060_019", # non profit
  "C24060_013", # self employed
  "C24060_007", # private
  "C24060_025" # gov employment

)

# test
# this works
get_acs(
  geography = "congressional district",
  variables = "B19058_002",
  state = "WI",
  year = 2010,
  survey = "acs5"
)


# Years to pull.
years_to_pull <- 2010:2024

# ACS survey.
acs_survey <- "acs5"

# Geography to pull.
# Examples:
#   "state"
#   "congressional district"
acs_geography <- "state"

# Metadata year used to create final variable labels/names.
# Per your request, this uses 2022 metadata.
metadata_year <- 2022


# CHECK IF VARS ARE AVIALABLE
check_acs_vars_by_year <- function(years, variable_names, survey = "acs5") {
  map_dfr(years, function(yr) {
    metadata <- load_variables(
      year = yr,
      dataset = survey
    )

    tibble(
      year = yr,
      variable = variable_names,
      available = variable_names %in% metadata$name
    )
  })
}

availability_check <- check_acs_vars_by_year(
  years = years_to_pull,
  variable_names = acs_variable_names,
  survey = acs_survey
)

# availability_check
# Then summarize missing variables:
  availability_check |>
  filter(!available)


############################################################
# 3. Helper function: clean ACS metadata labels into names
############################################################

clean_acs_label_to_name <- function(label) {
  label |>
    # Remove common ACS metadata prefix.
    str_remove("^Estimate!!") |>
    str_remove("^Annotation of Estimate!!") |>

    # Replace ACS hierarchy separators with spaces.
    str_replace_all("!!", " ") |>

    # Remove trailing colons and other punctuation clutter.
    str_replace_all(":", " ") |>
    str_replace_all(",", " ") |>
    str_replace_all(";", " ") |>

    # Normalize whitespace.
    str_squish() |>

    # Convert to clean snake_case.
    make_clean_names()
}


############################################################
# 4. Build variable lookup from 2022 ACS metadata
############################################################

make_var_lookup_from_v22_metadata <- function(
    variable_names,
    metadata_year = 2022,
    survey = "acs5"
) {
  # Load ACS variable metadata.
  # This returns columns including:
  #   name
  #   label
  #   concept
  v22_metadata <- load_variables(
    year = metadata_year,
    dataset = survey
  )

  # Keep only requested variables and preserve the user's input order.
  lookup <- tibble(
    name = variable_names,
    input_order = seq_along(variable_names)
  ) |>
    left_join(v22_metadata, by = "name") |>
    arrange(input_order)

  # Stop early if any requested variable names were not found.
  missing_vars <- lookup |>
    filter(is.na(label)) |>
    pull(name)

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
      stable_name = clean_acs_label_to_name(label),

      # Make names unique in case two ACS labels clean to the same value.
      stable_name = make.unique(stable_name, sep = "_"),

      metadata_year = metadata_year
    ) |>
    select(
      variable = name,
      stable_name,
      label,
      concept,
      metadata_year
    )
}


  ############################################################
  # 5. Pull ACS data for one year
  ############################################################

  pull_congressional_districts_all_states <- function(
    year,
    variables,
    survey = "acs5",
    states = state.abb,
    cache_table = TRUE,
    ...
  ) {
    map_dfr(
      states,
      function(st) {
        get_acs(
          geography = "congressional district",
          variables = variables,
          year = year,
          survey = survey,
          state = st,
          cache_table = cache_table,
          ...
        )
      }
    )
  }

  pull_acs_one_year_option_b <- function(
    year,
    geography,
    variable_names,
    survey = "acs5",
    metadata_year = 2022,
    wide = TRUE,
    cache_table = TRUE,
    ...
  ) {
    lookup <- make_var_lookup_from_v22_metadata(
      variable_names = variable_names,
      metadata_year = metadata_year,
      survey = survey
    )

    year_metadata <- load_variables(
      year = year,
      dataset = survey
    )

    missing_for_year <- setdiff(variable_names, year_metadata$name)

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

    if (identical(geography, "congressional district")) {
      raw <- pull_congressional_districts_all_states(
        year = year,
        variables = unique(lookup$variable),
        survey = survey,
        cache_table = cache_table,
        ...
      )
    } else {
      raw <- get_acs(
        geography = geography,
        variables = unique(lookup$variable),
        year = year,
        survey = survey,
        cache_table = cache_table,
        ...
      )
    }

    out_longish <- raw |>
      left_join(lookup, by = "variable") |>
      mutate(
        year = year,
        geography_type = geography,
        .before = 1
      )

    if (!wide) {
      return(out_longish)
    }

    out_longish |>
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
  }

  ############################################################
  # 6. Pull ACS data for multiple years
  ############################################################

  pull_acs_years_option_b <- function(
    years,
    geography,
    variable_names,
    survey = "acs5",
    metadata_year = 2022,
    wide = TRUE,
    cache_table = TRUE,
    ...
  ) {
    map_dfr(
      years,
      \(yr) pull_acs_one_year_option_b(
        year = yr,
        geography = geography,
        variable_names = variable_names,
        survey = survey,
        metadata_year = metadata_year,
        wide = wide,
        cache_table = cache_table,
        ...
      )
    )
  }



############################################################
# 7. Optional: create a variable-label crosswalk
############################################################

# This is useful if you want to inspect how ACS variable names
# were translated into final wide dataframe column names.
acs_variable_label_crosswalk <- make_var_lookup_from_v22_metadata(
  variable_names = acs_variable_names,
  metadata_year = metadata_year,
  survey = acs_survey
) |>
  mutate(
    estimate_column = paste0(stable_name, "_estimate"),
    moe_column = paste0(stable_name, "_moe")
  )


############################################################
# 8. Pull data
############################################################

acs_house <- pull_acs_years_option_b(
  years = years_to_pull,
  geography = "congressional district",
  variable_names = acs_variable_names,
  survey = acs_survey,
  metadata_year = metadata_year,
  wide = TRUE
)

acs_senate <- pull_acs_years_option_b(
  years = years_to_pull,
  geography = "state",
  variable_names = acs_variable_names,
  survey = acs_survey,
  metadata_year = metadata_year,
  wide = TRUE
)


############################################################
# 9. Save output
############################################################

census <- full_join(acs_house, acs_senate)

census_variables <- acs_variable_label_crosswalk

save(census, census_variables, here::here("data", "census.rda"))

############################################################
# 10. Quick checks
############################################################

glimpse(census)

acs_variable_label_crosswalk
