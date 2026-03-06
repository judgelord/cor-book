library(tidyverse)
library(here)
library(googlesheets4)
library(magrittr)

master_crosswalk <- read_sheet("1lxKi3EsOREE-YoOjPE6nIxzwfyJJYbao2YUji__UR7c") |>
  drop_na(department_agency_acronym)


# correspondence data
load(here::here("data", "corr_counts.Rdata"))


#TODO fix this in other repo
# corrections (should not be necessary for next version)
corr_counts %<>% mutate(agency = agency |>
                          str_replace("DOC_SBA", "SBA") |>
                          str_replace("HUD_HQ", "HUD") |>
                          str_replace("DHS_HQ", "DHS") |>
                          str_replace("Navy", "NAVY")
)



corr_counts %<>%
  left_join(master_crosswalk |>
              distinct(department, department_acronym, department_agency_acronym),
            by = c("agency" = "department_agency_acronym") )

corr_counts %<>%
  mutate(main_agency_acronym = coalesce(department_acronym, agency)) %>%
  select(main_agency_acronym, everything() )

# PROBLEMS
corr_counts |> count(main_agency_acronym, sort = T) |> filter(str_detect(main_agency_acronym, "_"))

save(corr_counts, file = here::here("data", "corr_counts2.Rdata"))

