source("setup.R")


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
             sort = T) |> select(n, everything())

emd |> filter(n>1)

# of these, most of them are not related to people who won, but tx-28 is
# TX 28 in 2020 had two dem primaries? Or was one a third party? 
emd |> filter(congress == 110, state_abbrev == "TX", district_code == 28)
earlymoneydata |> filter(year == 2006, state == "TX", district == 28)


# DROP SPECIAL ELECTIONS WHERE THERE IS A NON-SPECIAL ELECTION IN THE SAME CONGRESS 
emd %<>% filter(!(n > 1 & special == 1))

# LOOK AGAIN FOR DUPLICATES 
emd %>% count(chamber, congress, state_abbrev, district_code, party_name, dpres, 
                   #PRVYEreceipts_toptwo20, PRVYEcompetitivereceipts_575, 
                   sort = T) |> select(n, everything())



load(here::here("data", "members.Rdata"))

# confirm no duplicates in members prior to merge 
members |> count(icpsr, chamber, congress, sort = T)

# check for multiple people in a district in a congress
members |> count(chamber, congress, state_abbrev, district_code, party_name, sort = T) |> filter(n > 1)


# test merge 
members %>% left_join(emd) |> count(icpsr, chamber, congress, 
                                    dpres, 
                                    #PRVYEreceipts_toptwo20, PRVYEcompetitivereceipts_575,
                                    sort = T) 


# actual merge 
members %<>% 
  left_join(emd) %>% 
  #filter(chamber == "House") %>% 
  select(year_of_prior_election, state_abbrev, district_code, everything())



members %>% count(is.na(year_of_prior_election))



# save(members, file = here::here("data", "members.Rdata"))
