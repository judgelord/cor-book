
library(haven)

earlymoneydata <- read_dta(here::here("data", "earlymoneydata_primary.dta"))

earlymoneydata %>%
  mutate(party = ifelse(rep == 1,
                        "Repubican",
                        "Democratic")
  ) %>%
  ggplot() +
  aes(x = year,
      y = paste(state, district),
      color = party, label = district) +
  geom_text(size = 0.5, alpha = .5) +
  geom_point(size = 0.01, alpha = .5) +
  facet_wrap("party") +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 1))

emd <- distinct(earlymoneydata,
                year_of_prior_election = year, state_abbrev = state, district_code = district, rep,
                special,
                #blanket, safe, competitive, hopeless, # OTHER THINGS WE MGHT WANT
                PRVYEreceipts_toptwo20, PRVYEcompetitivereceipts_575, dpres)

# RENAME TO MATCH VOTEVIEW MEMBERS DATA
# BY SETTING CONGRESS TO THE YEAR OF THE PRIOR ELECTION, WE ARE LOOKING AT MEMBER BEHAVIOR RELATIVE TO WHETER THE PRIOR ELECTION (THAT THEY WON) WAS COMPEITIVE
# WE SHOULD ALSO MAKE A MEASURE FOR FUTURE ELECTIONS
emd %<>% mutate(congress = as.numeric(round((year_of_prior_election - 2001.1)/2)) + 107,
                congress = congress + 1, # lag congress
                chamber = "House",
                party_name = ifelse(rep == 1,
                                    "Repubican Party",
                                    "Democratic Party")
)

# LOOK FOR DUPLICATES
emd %<>% add_count(chamber, congress, state_abbrev, district_code, party_name, dpres,
                   #PRVYEreceipts_toptwo20, PRVYEcompetitivereceipts_575,
                   sort = T) %>%  select(n, everything())

early_money_duplicates <- emd |> filter(n>1)

save(early_money_duplicates, file =  here::here("data", "early_money_duplicates.rda"))
# of these, most of them are not related to people who won, but tx-28 is
# TX 28 in 2020 had two dem primaries? Or was one a third party?
emd |> filter(congress == 110, state_abbrev == "TX", district_code == 28)
earlymoneydata |> filter(year == 2006, state == "TX", district == 28)


# DROP SPECIAL ELECTIONS WHERE THERE IS A NON-SPECIAL ELECTION IN THE SAME CONGRESS
emd %<>% filter(!(n > 1 & special == 1))

# LOOK AGAIN FOR DUPLICATES
emd %>% count(chamber, congress, state_abbrev, district_code, dpres, party_name,
              #PRVYEreceipts_toptwo20, PRVYEcompetitivereceipts_575,
              sort = T) |> select(n, everything())

# WITHOUT PARTY, THERE ARE DUPLICATES, I GUEST BECAUSE CANDIDATES RUN IN BOTH PRIMARIES?
# THUS IT IS IMPORTANT TO INCLUDE PARTY NAME IN members_data
# LOOK AGAIN FOR DUPLICATES
emd %>% count(chamber, congress, state_abbrev, district_code, dpres,
              #PRVYEreceipts_toptwo20, PRVYEcompetitivereceipts_575,
              sort = T) |> select(n, everything())

emd %<>% mutate(party_name = str_replace(party_name, "Repubican Party", "Republican Party"))

member_data %<>%
  mutate(party_name = case_when(
    party == "(D)" ~ "Democratic Party",
    party == "(R)" ~ "Republican Party",
    party == "(I)" ~ "Independent"
  ))

# confirm no duplicates in member_data prior to merge
member_data <- distinct(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

# check for multiple people in a district in a congress
member_data |> count(chamber, congress, state_abbrev, district_code, party, sort = T) |> filter(n > 1)


# test merge
member_data %>%
  left_join(emd |>
              distinct(chamber,
                       congress,
                       state_abbrev,
                       district_code,
                       party_name, # important
                       dpres) ) |>
  count(icpsr, chamber, congress, party,
        dpres, #PRVYEreceipts_toptwo20, PRVYEcompetitivereceipts_575,
        sort = T)

# actual merge
member_data %<>%
  left_join(emd |>
              distinct(chamber,
                       congress,
                       state_abbrev,
                       district_code,
                       party_name, # important
                       PRVYEcompetitivereceipts_575,
                       year_of_prior_election,
                       dpres) )

# ELECTION competitive
member_data %<>% mutate(
  competitive = ifelse(dpres > 45 & dpres < 55,
                       1,
                       0),
  competitive_primary = PRVYEcompetitivereceipts_575
)

member_data %>% count(is.na(year_of_prior_election))
count(member_data, is.na(dpres), congress, chamber, party) |> kable()

