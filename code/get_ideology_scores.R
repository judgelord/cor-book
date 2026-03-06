load(here::here("data", "rcl_ideology_estimates.RData"))

x$Agency <- x$X.agency |> str_remove(" \\(.*")

load(here::here("data", "crosswalk.rda"))


x %<>% left_join(crosswalk |>
                   distinct(ACUS_agency, department_agency_acronym),
                 by= c("Agency" = "ACUS_agency"))

x %<>% left_join(crosswalk |>
                   distinct(agency, department_agency_acronym),
                 by= c("Agency" = "agency")) %>%
  mutate(department_agency_acronym = coalesce(department_agency_acronym.x, department_agency_acronym.y) ) %>%
  select(-department_agency_acronym.x, -department_agency_acronym.y)

x %<>% left_join(crosswalk |>
                        select(regulationsdotgov_agency, department_agency_acronym),
                      by= c("Agency" = "regulationsdotgov_agency"))%>%
  mutate(department_agency_acronym = coalesce(department_agency_acronym.x, department_agency_acronym.y) ) %>%
  select(-department_agency_acronym.x, -department_agency_acronym.y)

x %<>% left_join(crosswalk|>
                        select(agency_short, department_agency_acronym),
                      by= c("Agency" = "agency_short")) %>%
  mutate(department_agency_acronym = coalesce(department_agency_acronym.x, department_agency_acronym.y) ) %>%
  select(-department_agency_acronym.x, -department_agency_acronym.y)

x %<>% left_join(crosswalk|>
                        select(other_names, department_agency_acronym),
                      by= c("Agency" = "other_names")) %>%
  mutate(department_agency_acronym = coalesce(department_agency_acronym.x, department_agency_acronym.y) )%>%
  select(-department_agency_acronym.x, -department_agency_acronym.y)

#TODO
x |> filter(is.na(department_agency_acronym)) |> select(Agency)

rcl_ideology_estimates <- x

save(rcl_ideology_estimates, file = here::here("data", "rcl_ideology_estimates.rda"))
