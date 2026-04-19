###########################################

# merge in committee data for the 106th-115th
load(here::here("data", "members_committees_106-115th.rda"))

members_committees

members_committees$positions %>% str_split(";") %>% unlist() %>% unique()

member_data %<>%  left_join(members_committees)

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

# look at missingness
member_data |> ungroup() |>  count(congress, committees, sort = T)

# merge in new committee data for the 116th 2019-2020
load(here::here("data", "members_committees_116th.rda"))
members_committees_116th <- members_committees_116th

members_committees_116th$titles %>% str_split(";") %>% unlist() %>% unique()

members_committees_116th %<>%
  select(icpsr = icpsr_id,
         titles,
         committees2 = committees) %>%
  mutate(congress = 116)

member_data %<>%  left_join(members_committees_116th) %>%
  mutate(
    # because these come from two different datasets, need to replace NAs to avoide ifelse below yielding NA
    titles = replace_na(titles, ""),
    positions = replace_na(positions, ""),
    chair = ifelse( str_detect(titles, "^Chair|;Chair|;Chairman|;Cochairman") | str_detect(positions, "Chair"),
                    1, 0 ),
    ranking_minority = ifelse( str_detect(titles, "Ranking Member")  | str_detect(positions, "Ranking Minority"),
                               1, 0 )
  ) %>%
  mutate(committees = coalesce(committees, committees2)) %>%
  select(-committees2) %>%
  distinct()

# NO LONGER NEEDED
# hack <- tibble(
#   congress = c(116, 116, 116, 116),
#   majority2 = c(1,0, 0, 1),
#   #party = c("(D)", "(D)","(R)","(R)"),
#   presidents_party = c(1,1,0,0),
#   chamber = c("House", "Senate", "House", "Senate")
# )
#
# member_data <-member_data %>% left_join(hack)
#member_data %<>% mutate(majority = coalesce(majority, majority2))

# corrections to committee data
corrections <- read_csv(here::here("data", "committee_corrections_116th.csv"))
corrections

corrections %<>% select(-notes, -`...4`)

# add in committee data corrections
member_data <- member_data %>%
  filter(!icpsr %in% corrections$icpsr | congress != 116) %>%
  full_join(corrections)  %>%
  ungroup()

# look for missing committee data
missing <- member_data %>% filter(congress == 116, is.na(committees))

missing <- member_data %>% filter(bioname %in% missing$bioname, congress==116) %>% arrange(bioname)

missing %>%
  write_csv(here::here("data", "missing_committees_116th.csv"))



## OVERSIGHT

load(here::here("data", "oversight_committee_data.rda"))

d1 <- oversight_committee_data |> select(
  committees = `Reporting Committees`,
  department_agency_acronym) |>
  drop_na(committees ) |>
  mutate(committees = str_to_upper(committees))# |>
# mutate(committee = str_split(committee, ";") |>
#          str_to_upper() )
# |>  unnest(committee)
d1

member_data$committees %<>% str_replace_all("\\|", ";")

# CAUTION! THIS WAS WRITTEN WITH "|" rather than ";", causeing errors, I am changing it but it may cause other errors if we are loading thorugh data with "|"
member_data$committees  <- member_data$committees |>
  str_replace_all("TURAL RESOURCES", ";NATURAL RESOURCES") |>
  str_replace_all(";NA;", ";" )  |>
  str_remove_all(";NA$|^NA;") |>
  str_replace_na()


m <-  member_data$committees |>
  str_split("\\;") |>
  unlist() |>
  str_squish() |>
  unique()

m <- m[!m %in% c("0", "NA", NA)]
m

#
# match <- function(x,y) {
#   z = ifelse(str_detect(x, y), y, NA)
# }
#
# match2 <- function(y){
#   map_chr(.x = m, )
# }
#
# map_chr(.x = d1$committee, .f = match2 )

mc <- m |> paste(sep = "|", collapse = "|")

d1$committees |> str_extract_all(mc)

crosswalk <- d1 |> mutate(
  committees = committees |> str_extract_all(mc)) |>
  unnest(committees) |>
  distinct()

members <- member_data |>
  mutate(committees = str_split(committees, ";|\\|")) |>
  unnest(committees) |>
  distinct() |>
  left_join(crosswalk) |>
  group_by(icpsr,congress, chamber) |>
  mutate(committees = str_c( unique(committees), collapse = ";")  |>
           str_remove_all("^NA;|;NA$|^NA$") |>
           str_replace(";NA;", ";"),
         #oversight = str_c( unique(department_agency_acronym), collapse = ";"),
         oversight = list(department_agency_acronym)  |>
           unlist() |> paste(collapse  = ";") |>
           str_remove_all("^NA;|;NA$|^NA$") |>
           str_replace(";NA;", ";")
  ) |>
  select(-department_agency_acronym) |>
  distinct()

members$committees

members$oversight

member_data <- members


## NOTES ON SOME OF THE MISSING COMMITTEE DATA THAT WAS MISSING
# AMASH switched parties

# Duffy resigned
# CUMMINGS, Elijah Eugene died

# ? CLYBURN, James Enos
# ? MCCARTHY, Kevin


# BISHOP, Dan came in after
# GARCIA, Mike - 2020 special election
# HILL, Katie - resigned
