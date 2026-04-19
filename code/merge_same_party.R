# congress year converters
congress_years<- function(congress){
  years<- c(congress*2 + 1787, congress*2 + 1788 )
  return(years)
}
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
  distinct(state_dist, congress, icpsrs, parties, chamber) %>%
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
  ) %>%
  select(chamber, congress, state_dist, icpsr, new_member, new_party, same_party, party, parties, lag_parties) %>%
  group_by(state_dist, icpsr) %>%
  arrange(state_dist, icpsr, congress) %>%
  tidyr::fill(same_party, .direction = "down")  %>%
  ungroup() %>%
  arrange(state_dist, congress)

member_data %<>%
  left_join(same_party_crosswalk %>%
              # note: important to include chamber to avoide dupicates for chamber switchers
              distinct(state_dist, congress, chamber, icpsr, same_party))

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

