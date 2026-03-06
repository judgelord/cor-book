library(googlesheets4)
crosswalk <- read_sheet("https://docs.google.com/spreadsheets/d/1lxKi3EsOREE-YoOjPE6nIxzwfyJJYbao2YUji__UR7c/edit?gid=0#gid=0") |>
  mutate(other_names = other_names |> str_remove("\\|.*") |> str_squish() )

crosswalk <- crosswalk |>
  mutate(agency = coalesce(agency, agency_short, regulationsdotgov_agency, ACUS_agency, other_names, department)) |>
  select(department, agency, agency_short, department_agency_acronym, department_acronym, other_acronyms, other_names, regulationsdotgov_acronym, regulationsdotgov_agency, ACUS_agency)


crosswalk|> drop_na(agency) |>  count(agency) |> filter(n>1)


save(crosswalk, file = here::here("data", "crosswalk.rda"))

