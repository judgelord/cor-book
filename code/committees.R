
library(tidyverse)

load(committees)


# Install the yaml package if it's not already installed
if (!require("yaml")) {
  install.packages("yaml")
}

# Load the yaml package
library("yaml")

# helper function 
extract <- function(x, name){
  d <- map_dfr(x, as_tibble)
  d$thomas_id <- name
  return(d)
}


y <- here::here("data", "committee-membership-2019-03-28.yaml") |>
  read_yaml()

names_y <- map(y, names) |> names()

membership2019 <- map2_dfr(y, names_y, extract) 


y <- here::here("data", "committee-membership-2020-12-07.yaml") |>
  read_yaml()

names_y <- map(y, names) |> names()

membership2020 <- map2_dfr(y, names_y, extract) 



membership <- full_join(membership2019, membership2020) |> 
  # match the other data from the same source 
  rename(bioguide_id = bioguide)


y <- here::here("data", "committees-current.yaml") |>
  read_yaml()

committees <- map_dfr(.x = y, .f =  as_tibble)

save(committees, file = here::here("data", "committees_current.rda"))

committee_membership_116th <- left_join(membership, 
                                  distinct(committees, thomas_id, committee = name, type) ) |> 
  # IMPORTANT: drop subcommittees
   drop_na(committee)

save(committee_membership_116th, 
     file = here::here("data", "committee_membership_116th.rda") )

# format committee data the same as the old stuff in the members data 
c <- committee_membership_116th |> 
  mutate(committee = committee |>
           str_remove_all(".*ommittee on the |.*ommittee on | and.*|,.*|' Affairs| Affairs| Committee|Joint ") |> 
           str_to_upper()
           )

c1 <- c$committee |> unique()


c2 <- members$committees |> str_split("\\|") |> unlist() |> unique()

c2[!c2 %in% c1]
c1[!c1 %in% c2]

members_committees <- c |> 
  group_by(bioguide_id) |> 
  mutate(#chamber = str_to_sentence(type),  #FIXME 
         committees = committee |> unique() |> paste(, collapse = "|") |> 
           str_remove_all("\\|NA\b*")|> 
           str_remove_all("\\|NA\b*")|> 
           str_remove_all("\\|NA\b*"),
         titles = unique(title) |> paste(collapse = ";") |> 
           str_remove_all(";NA\b*")|> 
           str_remove_all(";NA\b*")|> 
           str_remove_all("^NA;*")
         ) |> 
  distinct(committees, titles, name) |> 
  ungroup() |> 
  add_count(bioguide_id) |> 
  arrange(-n)

members_committees$titles |> unique()

members_committees$committees |> unique()


## ID FOR MERGING WITH MEMBERS DATA 

## THIS WOULD WORK IF LEGISLATORS WERE UPDATED 
# library(legislators)
# members_committees <- members_committees |> extractMemberName("name", congress = 116)

legislators_current <- read_csv(here::here("data", "legislators-current.csv"))
legislators_historical <- read_csv(here::here("data", "legislators-historical.csv"))

legislators <- full_join(legislators_current, legislators_historical)

missing_icpsr <- legislators |> filter(is.na(icpsr_id)) 

library(legislators)
fix <- missing_icpsr |> 
  extractMemberName("full_name", congress = 116) |> 
  drop_na(icpsr) |> 
  mutate(full_name = str_to_title(full_name))

legislators <- legislators |> 
  left_join(fix |> 
              distinct(icpsr, full_name)) |> 
  mutate(icpsr_id = coalesce(icpsr_id, icpsr)) |> 
  select(-icpsr)

members_committees_116th <- members_committees |> 
  # has corrected icpsrs 
  left_join(legislators)


# look for missing 
members_committees |> filter(is.na(icpsr_id)) |> 
  distinct(name, type, state, district)

# correct missing icpsrs 
members_committees <- members_committees |> 
  mutate(icpsr_id = ifelse(
    name == "Kelly Loeffler",  41904, icpsr_id
  ))

save(members_committees_116th, file =  here::here("data", "members_committees_116th.rda"))




#TODO merge in committee data for the 106th-115th 
load(here::here("data", "committees_membership_106-115.rda"))
committees 

members_committees <- committees |> 
  group_by(icpsr, congress, chamber) |> 
  summarise(committees = committees|> unique() |> paste(collapse = ";"), 
            positions = position |> unique() |> paste(collapse = ";")) |> 
  ungroup()

save(members_committees, file = here::here("data", "members_committees_106-115th.rda"))


