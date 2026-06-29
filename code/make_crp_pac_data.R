library(tidyverse)
library(here)
library(legislators)
library(knitr)

# CONGRESS VAR
year_congress<- function(year){
  return(floor((year - 1787)/2))
}


list.files(here("data", "pac"))


#############################################################################################
# OPTION 1 from CPR BULK IN 2021
# 3.2 m observations, divided into energy and other (need to make a combined var)
# it does not have ICPSR MERGED IN, SO REQUIRES EXTRACT MEMBER NAMES
load(here("data" , "pac", "pac_contributions-from-cr_bulk.Rdata"))

pac_contributions$Date |> head() |> as.Date("%m/%d/%Y")

d <- pac_contributions %<>%
  mutate(
    date = as.Date(Date, "%m/%d/%Y"),
    year = str_sub(date, 1,4) |> as.numeric(),
         congress = year_congress(year))

d |> filter(is.na(date)) #111 missing

d |> count(congress) |> arrange(congress) |> kable()

# looks like these data are good from the 103rd to 115th
d %<>% filter(congress > 102, congress < 116)

# get icpsr from name using legislators package
d %<>% extractMemberName(col_name = "FirstLastP",
                         congress = "congress")

# duplicates
d %>% count(icpsr, bioname, FECRecNo) |> filter(n > 1)

# missing
d %>% filter(is.na(icpsr)) |>
  count(FirstLastP, congress, sort = T)

pac_contributions_crp_energy_clean <- d

save(pac_contributions_crp_energy_clean, file = here("data" , "pac", "pac_contributions_crp_energy_clean.Rdata"))

################################################################################
# OPTION 2 from CRP IN 2020
# 4.2 m observations - not divided into sector (perhaps an earlier version of the above?)
# it also does not have ICPSR MERGED IN, SO REQUIRES EXTRACT MEMBER NAMES
load(here("data" , "pac", "pac_money-from-CRP-Data.Rdata"))

d <- pac_money %<>%
  mutate(
    date = as.Date(Date, "%m/%d/%Y"),
    year = str_sub(date, 1,4) |> as.numeric(),
    congress = year_congress(year))

d |> filter(is.na(date)) #163 missing

d |> count(congress) |> arrange(congress) |> kable()

# looks like these data are also good from the 103rd to 115th, but with more observations, oddly
d %<>% filter(congress > 102, congress < 116)

# get icpsr from name using legislators package
d %<>% extractMemberName(col_name = "FirstLastP",
                         congress = "congress")

# duplicates
d %>% count(icpsr, bioname, FECRecNo) |> filter(n > 1)

# missing
d %>% filter(is.na(icpsr)) |>
  count(FirstLastP, congress, sort = T)

pac_contributions_crp_clean <- d

save(pac_contributions_crp_clean, file = here("data" , "pac", "pac_contributions_crp_clean.Rdata"))

###########################################################################################################
# OPTION 3 from correspondence - linstat IN 2020 (FERC PAPER? Possibly from Bonica )
# 1.17 m observations - divided into sector - FOR FERC PAPER
# 105th - 115th (contribuutions in 106th - 116th) and fewer observations for each (like 90k vs 388k  for the 115th compared to above) ??????

# it does have icpsr  and bonica IDs
# it also has ICPSR_corrrect, which makes me nervious - this seems mostly NA, and where it is not, it matches icpsr, so corrections must have already been made
# - upon checking, bonica's ICPSRs are mostly correct, see https://github.com/judgelord/legislators-data/issues/12
# it has no dates, just congress after the cycle

# IMPORTANT, "congress" is the congress CONGRESS AFTER THE CYCLE OF THE DONATION IN THIS ONE

load(here("data" , "pac", "pac_contributions-from-judgelord-correspondence-data.Rdata"))

d <- pac_contributions

d |> count(congress) |> arrange(congress) |> kable()

# drop 105, which is clearly missing data
d %<>% filter (congress > 105)

d$contributor_type |> unique()
d$icpsr_correct |> unique()
d$industry |> unique()

look <- d |> filter(is.na(industry)) |> drop_na(icpsr, amount)

d |> drop_na(icpsr_correct) |> distinct(congress, recipient_name, recipient_icpsr, icpsr_correct, icpsr)


d |> filter(is.na(recipient_icpsr)) |> distinct(recipient_name)

# Investigate icpsr mismatch
d |> filter(icpsr != icpsr_correct)
d |> filter(icpsr != recipient_icpsr)

if(F){ # TODO after fixing issue https://github.com/judgelord/legislators-data/issues/12
d %<>%
  mutate(congress_after = congress,
         congress = congress - 1) %>%
  extractMemberName("recipient_name", congress = "congress")

# Investigate icpsr mismatch
d |> filter(icpsr != icpsr_correct)
d |> filter(icpsr != recipient_icpsr)  |> select(congress, contains("icpsr"), recipient_name) |> distinct() |> mutate(icpsr_correct = recipient_icpsr) |> kable()
}
# it looks like recipiant_icpsr is correct

pac_contributions_bonica_clean <- d

save(pac_contributions_bonica_clean, file = here("data" , "pac", "pac_contributions_bonica_clean.Rdata"))


total <- d %>%
  group_by(icpsr, congress) |>
  summarise(PACamount = sum(amount))

energy <- d %>%
  filter(industry == "energy") |>
  group_by(icpsr, congress) |>
  summarise(PACamount_energy = sum(amount))

# THIS CAN BE CALCULTED LATER BY SUBTRACTION
# nonenergy <- d %>%
#   filter(industry == "other") |>
#   group_by(icpsr, congress) |>
#   summarise(PACamount_nonenergy = sum(amount))

pac_contributions_bonica_totals <- full_join(total, energy) |>
  mutate(PACamount_energy = replace_na(PACamount_energy, 0) )

save(pac_contributions_bonica_totals, file = here("data" , "pac", "pac_contributions_bonica_totals.Rdata"))



###########################################################################################################
# OPTION 4 from Correspondence dropbox in 2019 - this looks like raw BONICA - 1980-2012 - NOT USING THIS ONE
# 8m observations - NOT divided into sector
# BUT IT COULD BE GOOD FOR TESTING THE LEGISLATORS PACKAGE https://github.com/judgelord/legislators-data/issues/12
# it does have icpsr  and bonica IDs
# it also has ICPSR_corrrect, which makes me nervious
# it has no dates

# IMPORTANT, CONGRESS IS THE CONGRESS AFTER THE CYCLE OF THE DONATION IN THIS ONE

load(here("data" , "pac", "pac_contributions-from-Correspondence.Rdata"))

d <- pac_contributions

names(pac_contributions)


# 106-116
d |> count(cycle) |> arrange(cycle) |> kable()

d$contributor_type |> unique()

d |> filter(is.na(recipient_icpsr)) |> distinct(recipient_name)

# Investigate icpsr mismatch
d |> filter(icpsr != icpsr_correct)
d |> filter(icpsr != recipient_icpsr)

d %<>%
  mutate(congress_after = congress,
         congress = congress - 1) %>%
  extractMemberName("recipient_name", congress = "congress")

