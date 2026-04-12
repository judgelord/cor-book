library(tidyverse)
library(magrittr)

# The minimal count data
load(here::here("data", "dcounts_min.Rdata"))
names(dcounts_min)

corr_counts <- dcounts_min %>% distinct(icpsr, chamber,
                                        agency, year,
                                        TYPE, per_icpsr_chamber_year_agency_type) #%>% filter(TYPE %in% c(1,2,3,4,5))

# agency name corrections
#TODO implement this upstream
corr_counts %<>% mutate(agency = agency |>
                          str_replace("DOC_SBA", "SBA") |>
                          str_replace("HUD_HQ", "HUD") |>
                          str_replace("DHS_HQ", "DHS") |>
                          str_replace("Navy", "NAVY")
)

save(corr_counts,
     file = here::here("data", "corr_counts.Rdata"))
