## The data on members
## from voteview + committee data from Stewart and Wu + @unitedstates project
#TODO merge both sources of committee data first and then with voteview
##  also including population from the census

# VOTEVIEW HOUSE AND SENATE
members_raw <- read_csv(here::here("data", "HSall_members.csv"))

members <- members_raw %>%
  select(icpsr, bioname, congress, chamber, party_code, state_abbrev, district_code) %>%
  filter(congress > 100, congress < 117) %>%
  mutate(
    party = case_when(
      party_code == 100 ~ "(D)",
      party_code == 200 ~ "(R)",
      party_code == 328 ~ "(I)",
      F ~ NA
    ) )


members %>% filter(party =="(I)" ) %>% distinct(bioname)

# independents
# who caucus with Dems
ds <- c(        "JEFFORDS, James Merrill",
                "BARKLEY, Dean",
                "SANDERS, Bernard",
                "KING, Angus Stanley, Jr.")

# who caucus with GOP
rs <- c("GOODE, Virgil H., Jr.")

# as far as I can tell, Amash stopped caucusing with GOP, but sort of moot
is <-  c("AMASH, Justin")

members %<>%
  mutate(party_caucus =
           case_when(
             party == "(I)" & bioname %in% ds ~ "(D)",
             party == "(I)" & bioname %in% rs ~ "(R)",
             party == "(I)" & bioname %in% is ~ "(I)",
             TRUE ~ party ))

presidents <- members %>% filter(chamber == "President") %>%
  select(congress, party_of_president = party) %>% arrange(-congress)

members %<>%
  left_join(presidents) %>%
  mutate(presidents_party = as.numeric(party_caucus == party_of_president ) ) %>%
  filter(chamber != "President")

# Party size data
# From voteview.com https://voteview.com/articles/data_help_parties
parties <- read_csv(here::here("data", "HSall_parties.csv"))

party_size <- parties %>% distinct(party_code, n_members, chamber, congress) %>% arrange(-congress) %>%
  filter(chamber != "President") %>%
  group_by(congress, chamber) %>%
  mutate(chamber_size = sum(n_members))

# fix error (this is the only one that will affect our variables)
party_size %<>%
  mutate(
    n_members = case_when(
      chamber == "Senate" & congress == 110 & party_code == 100 ~ 51,
      chamber == "Senate" & congress == 110 & party_code == 200 ~ 49,
      T ~ n_members
    ),
    party_caucus = case_when(
      party_code == 100 ~ "(D)",
      party_code == 200 ~ "(R)",
      party_code == 328 ~ "(I)",
      T ~ NA
    )
  ) %>% select(-party_code)

members %<>%
  left_join(party_size) %>%
  mutate(
    party_size = n_members,
    majority =
      case_when(
        presidents_party == 1 & chamber == "Senate" & party_size == 50 ~ 1,
        chamber == "Senate" & party_size > 50 ~ 1,
        chamber == "House" & party_size > 217 ~ 1,
        T ~ 0
      ))

members %>% distinct(congress, party, chamber, majority) %>% filter(party !="(I)")

completeness <- members %>%
  filter(chamber != "President") %>%
  count(party, party_caucus, party_size, chamber, majority, presidents_party, party_of_president, congress) %>% arrange(-congress)

# check chamber size implied by voteview party data
completeness %>%
  filter(!party == "(I)") %>%
  group_by(congress, chamber) %>%
  summarise(sum(party_size)) %>% arrange(-congress)


member_data <- members %>%
  ungroup() %>%
  distinct( congress, chamber,
            bioname, #last_name,
            icpsr, #cqlabel,
            district_code,
            state_abbrev, #state,
            district_code, #pop2010,
            # committees, chair, ranking_minority,
            majority, presidents_party, party#, yearelected
  ) %>%
  group_by(bioname) %>%
  # first year
  mutate(first_cong = min(congress),
         first_year = 1787 + 2*first_cong) %>%
  # subset to 2007-2020 where we have reliable correspondence count data
  # filter(congress > 109, congress < 117) %>%
  ungroup()




# merge in district population
states <- read_csv(here::here("data", "states.csv")) %>% select(state, pop2010)

stateFromLower <-function(x) {
  #read 52 state codes into local variable [includes DC (Washington D.C. and PR (Puerto Rico)]
  st.codes<-data.frame(
    state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
                      "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME",
                      "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM",
                      "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN",
                      "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")),
    full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado",
                     "connecticut","district of columbia","delaware","florida","georgia",
                     "hawaii","iowa","idaho","illinois","indiana","kansas","kentucky",
                     "louisiana","massachusetts","maryland","maine","michigan","minnesota",
                     "missouri","mississippi","montana","north carolina","north dakota",
                     "nebraska","new hampshire","new jersey","new mexico","nevada",
                     "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico",
                     "rhode island","south carolina","south dakota","tennessee","texas",
                     "utah","virginia","vermont","washington","wisconsin",
                     "west virginia","wyoming"))
  )
  #create an nx1 data.frame of state codes from source column
  st.x<-data.frame(state=x)
  #match source codes with codes from 'st.codes' local variable and use to return the full state name
  refac.x<-st.codes$full[match(st.x$state,st.codes$state)]
  #return the full state names in the same order in which they appeared in the original source
  return(refac.x)
}

member_data %<>% mutate(state =stateFromLower(state_abbrev)) %<>%
  left_join(states)

# SAME PARTY
member_data %<>%
  group_by(icpsr, congress, chamber) %>%
  mutate(years = congress %>%
           congress_years() %>%
           paste(collapse = ""),
         new_member = str_detect(years, as.character(first_year)) )

member_data |>
  distinct(congress, years, first_year, new_member) |>
  arrange(-first_year)

member_data$new_member |> sum()


# 2010 redistricting cycle for 2012 election of the 113th congreess (APPROX)
#FIXME
member_data %<>%
  # just for post 2000 data for now
  #filter(congress > 106) %>%
  mutate(decade = case_when(
    congress < 113  ~ '0',
    congress > 112 ~ '1'))


member_data %<>% mutate(state_dist = case_when(
  chamber=='Senate'~ paste(state,district_code,  sep='_' ),
  chamber =='House'~ paste(paste(state, district_code, sep='_'), decade, sep='_')
))

member_data |> filter(is.na(state_dist))


# make a variable for the prior seat holder party
same_party_crosswalk <- member_data  %>%
  ungroup() %>%
  group_by(state_dist, chamber, congress) %>%
  # combine ICPSR IDS for senators to get the senate delegation to know if it changed
  arrange(icpsr) %>%
  # Make sure they are in the same order
  mutate(icpsrs = unique(icpsr) %>% # collapse senate ICPSR
           paste(collapse = ";"),
         parties = unique(party) %>% # collapse senat party
           paste(collapse = ";")
  ) %>%
  ungroup() %>%
  distinct(state_dist, congress, icpsrs, parties) %>%
  group_by(state_dist) %>%
  arrange(state_dist, congress) %>%
  mutate(lag_icpsrs = dplyr::lag(icpsrs),
         lag_parties = dplyr::lag(parties),
         # create lag party var and fill it in for that member's tenure
         new_member2 = icpsrs != lag_icpsrs,
         new_party = parties != lag_parties)

same_party_crosswalk %<>%
  full_join(member_data) %>%
  #drop_na(district_code)
  mutate(
    same_party =
      case_when(# correction for senators who are not the new member
        new_member & party == lag_parties ~ T,
        new_member & !str_detect(parties, party) ~ F,
        new_member & !new_party ~ T ,
                new_member & new_party ~ F
                )
  ) %>% select(congress, state_dist, icpsr, new_member, new_party, same_party, party, parties, lag_parties) %>%
  group_by(state_dist, icpsr) %>%
  arrange(state_dist, icpsr, congress) %>%
tidyr::fill(same_party, .direction = "down")  %>%
  ungroup() %>%
  arrange(state_dist, congress)

member_data %<>%
  left_join(same_party_crosswalk %>%
              distinct(state_dist, congress, icpsr, same_party))


###########################################

# merge in committee data for the 106th-115th
load(here::here("data", "members_committees_106-115th.rda"))

members_committees

members_committees$positions %>% str_split(";") %>% unlist() %>% unique()

member_data %<>%  left_join(members_committees)

member_data |> ungroup() |>  count(congress, committees, sort = T)

# merge in new committee data for the 116th 2019-2020
load(here::here("data", "members_committees_116th.rda"))
members_committees_116th <- members_committees_116th

members_committees_116th$titles %>% str_split(";") %>% unlist() %>% unique()

members_committees_116th %<>%
  select(name,
         icpsr = icpsr_id,
         titles,
         committees2 = committees) %>%
  mutate(congress = 116) %>%
  group_by(name) %>%
  fill(icpsr, .direction = "updown") %>%
  ungroup()

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


# MORE VARIABLES FROM RAW VOTEVIEW DATA
members <- members_raw %>%
  distinct(icpsr, bioname, congress, chamber, nominate_dim1, nominate_dim2)

member_data %<>% left_join(members)

save(member_data,
     file = here::here("data", "member_data.Rdata"))

skimr::skim(member_data)
