## The data on members
## from voteview + committee data from Stewart and Wu + @unitedstates project
#TODO merge both sources of committee data first and then with voteview
##  also including population from the census

# VOTEVIEW HOUSE AND SENATE
members_raw <- read_csv(here::here("data", "HSall_members.csv"))

members_raw <- read_csv("https://voteview.com/static/data/out/members/HSall_members.csv")

member_data <- members_raw %>%
  select(icpsr, bioname, congress, chamber, party_code, state_abbrev, district_code) %>%
  #FIXME when we add non-voting memember data
  mutate(
    party = case_when(
      party_code == 100 ~ "(D)",
      party_code == 200 ~ "(R)",
      party_code == 328 ~ "(I)",
      F ~ NA
    ) )

member_data %<>%
  ungroup() %>%
  group_by(bioname) %>%
  # first year
  mutate(first_cong = min(congress),
         first_year = 1787 + 2*first_cong) %>%
  # subset to 2007-2020 where we have reliable correspondence count data
  # filter(congress > 109, congress < 117) %>%
  ungroup()


# NOTE SUBSETTING TO 102th for simplicity
member_data %<>% filter(congress > 102)


# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)


# make presidents data
presidents_party <- member_data %>% filter(chamber == "President") %>%
  filter(!(bioname == "TRUMP, Donald John" & congress == 117)) %>%
  select(congress, party_of_president = party) %>% arrange(-congress) %>%
  ungroup()

# and then delete so they don't pop up in de-bugging
member_data %<>% filter(chamber != "President")

#################
#### COMMITTEES MUST GO FIRST BECAUSE OF CORRECTIONS
here::here("code", "merge_committees.R") |> source()

look <- member_data |> count(reporting_agencies, committees) |> head(100)

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

#################
#### PRESTIGE (MUST HAPPEN AFTER COMMITTEES)
here::here("code", "merge_prestige.R") |> source()

# INSPECT PRESTIGE
member_data |> drop_na(prestige) |> count(prestige) |> kable()

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)


# independents
member_data %>%
  filter(party =="(I)",
         congress > 104) %>%
  distinct(bioname)

# who caucus with Dems
ds <- c(        "JEFFORDS, James Merrill",
                "BARKLEY, Dean",
                "SANDERS, Bernard",
                "KING, Angus Stanley, Jr.",
                "SINEMA, Kyrsten",
                "MANCHIN, Joe, III")

# who caucus with GOP
rs <- c("GOODE, Virgil H., Jr.",
        "MITCHELL, Paul",
        "KILEY, Kevin")

# as far as I can tell, Amash stopped caucusing with GOP, but sort of moot
is <-  c("AMASH, Justin")

member_data %<>%
  mutate(party_caucus =
           case_when(
             party == "(I)" & bioname %in% ds ~ "(D)",
             party == "(I)" & bioname %in% rs ~ "(R)",
             party == "(I)" & bioname %in% is ~ "(I)",
             TRUE ~ party ))

member_data|> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

member_data %<>%
  ungroup() %>%
  left_join(presidents_party) %>%
  mutate(presidents_party = as.numeric(party_caucus == party_of_president ) )

filter(member_data, is.na(presidents_party))

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

# Party size data
# From voteview.com https://voteview.com/articles/data_help_parties
#TODO Read from URL instead to keep up with new data
parties <- read_csv(here::here("data", "HSall_parties.csv"))

party_size <- parties %>% distinct(party_code, n_members, chamber, congress) %>% arrange(-congress) %>%
  filter(chamber != "President") %>%
  group_by(congress, chamber) %>%
  mutate(chamber_size = sum(n_members)) %>%
  ungroup()


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

member_data %<>%
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

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)


member_data %>% count(congress, party, chamber, majority) %>% filter(party !="(I)")

completeness <- member_data %>%
  filter(chamber != "President") %>%
  count(party, party_caucus, party_size, chamber, majority, presidents_party, party_of_president, congress) %>% arrange(-congress)

# check chamber size implied by voteview party data
completeness %>%
  filter(!party == "(I)") %>%
  group_by(congress, chamber) %>%
  summarise(sum(party_size)) %>% arrange(chamber, -congress) %>%
  kable()


member_data |> count(state_abbrev) |> kable()

################
# STATE DATA
here::here("code", "merge_state_data.R") |> source()

#FIXME WHEN WE GET NON-VOTING MEMBERS MERGED IN, FOR NOW THIS IS JUST MAKING IT HARD TO DEBUG
member_data %<>%
  drop_na(state) %>%
  filter(!state %in% c("puerto rico", "district of columbia") )

member_data |> count(state) |> kable()


# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

################
# CRP PAC DATA

load(here("data" , "pac", "pac_contributions_bonica_totals.Rdata"))

# only have data 106-115 merged with ICPSR, but we have 103-115 in the data folder, see make_crp_pac_data.R
member_data <- member_data |> left_join(
  pac_contributions_bonica_totals) |>
  mutate(PACamount = ifelse(congress %in% 106:115 & is.na(PACamount), 0, PACamount),
         PACamount_energy = ifelse(congress %in% 106:115 & is.na(PACamount_energy), 0, PACamount_energy)) |>
  mutate(PACamount_m = PACamount/1000000,
         PACamount_energy_m = PACamount_energy/1000000)


# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

###############################################################################################
# LES
################################################################
load(here::here("data", "les", "les.rda"))

names(les)

# This causes an error in merge
les %<>% select(-year)

member_data %<>% left_join(les |> rename(chair_les=chair)) # |> distinct(icpsr, congress, lesclassic))

# INSPECT
if(F){
  # missing from LES
data_member |>
    filter(is.na(lesclassic), congress < 119) |>
    distinct(congress, chamber, icpsr, bioname, votepct, state_leg, lesclassic) |>
    # write_csv(here::here("data", "missing-from-les.csv"))
    kable()

member_data |> count(is.na(lesclassic), congress) |> arrange(congress) |>  kable()
member_data |> filter(is.na(lesclassic), congress < 119) |> count(bioname, state, sort = T) |>  kable()

# which chair data is more complete
member_data |>count(is.na(chair))
member_data |>count(is.na(chair_les))

# differences between my chair data from committees repo and LEP chair
member_data |> filter(chair != chair_les) |> select(congress, chamber, bioname, chair, chair_les, titles, positions, subchr) |> kable()


member_data |>count(is.na(state_leg))
member_data |>count(is.na(votepct))

member_data |>count(is.na(ranking_minority))
member_data |>count(is.na(presidents_party))
member_data |>count(is.na(prestige))
member_data$reporting_agencies |>count(is.na(reporting_agencies))


}

#TODO fix missing data  https://github.com/judgelord/committees/issues/4
# FIXME https://github.com/judgelord/committees/issues/7
# for now fill in missing from one to the other
member_data %<>%
  mutate(chair = coalesce(chair, chair_les) )%>%
  # missing < 1%
  group_by(bioname, chamber, majority) %>%
  arrange(congress) %>%
  # for small number of missing, assume they stayed sub-chair if they stayed in the majority
  fill(subchr, .direction = "down" ) %>%
  fill(chair, .direction = "down" ) %>%
  fill(ranking_minority, .direction = "down" ) %>%
  fill(prestige, .direction = "down" ) %>%
  # otherwise, assume not subchair (most of these are non-voting members who will be dropped, so it sort of does not matter)
  mutate(subchr = replace_na(subchr, 0),
         chair = replace_na(chair, 0),
         ranking_minority = replace_na(ranking_minority, 0),
         prestige = replace_na(prestige, 0),
         # "" is the value for missing given in generating the committees data
         # these should be replaced with 0 if they had no committee assignments in the committees repo
         reporting_agencies = replace_na(reporting_agencies, "") )  # %>%   filter(is.na(chair)) %>% select(chair, chair_les)

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)


################
# Same Party lookup table
here::here("code", "merge_same_party.R") |> source()

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)


################
# ELECTIONS DATA for house
here::here("code", "merge_electoral.R") |> source()

# Secondary measure for senate based on vote share alone
member_data %<>%
  mutate(competitive2 = votepct < 57.5,
         competitive = coalesce(competitive, competitive2))


# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)


# MORE VARIABLES FROM RAW VOTEVIEW DATA
member_data %<>%
  left_join(
    members_raw %>%
      distinct(icpsr, bioname, congress, chamber, nominate_dim1, nominate_dim2)
    )

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

member_data %<>% ungroup()

# TRANSFORMATIONS (NOT NEEDED FOR MINIMAL REPLICATION DATA)
# Make clean name
member_data <- member_data |>
  mutate(
    member = bioname |>
      str_remove(", .*") |>
      str_to_title() |>
      str_replace("cc", "cC"),
    member_state = paste(member, state_abbrev, sep = " (") |>
      paste0(")"),
    cqlabel = paste0("(",
                     state_abbrev,
                     "-",
                     district_code,
                     ")") |>
      str_remove("-0")
  )


count(member_data, congress)


##################################################
# STAFF DATA
########################
# Crosson et al. (2021). - from staff.R in correspondence_data repo
load(here::here("data", "Staff", "staff.rds"))

staff %<>% select(-votepct_sq)
# STAFF DATA SEEM TO HAVE TWO DUPLICATES FOR THIS VARIABLE, SO I'M DROPPING IT

member_data <- left_join(member_data, staff)

# transform spending to be spending in millions
member_data %<>% mutate(est_staff_spending = est_total_spending/1000000,
                        est_legis_spending = est_total_legis_spending/1000000,
                          est_pol_spending = est_total_pol_spending/1000000,
                          est_comm_spending = est_total_comm_spending/1000000,
                          est_off_spending = est_total_off_spending/1000000,
                          est_constit_spending = est_total_constit_spending/1000000)

# STAFF DATA SEEM TO HAVE TWO DUPLICATES FOR THIS VARIABLE, SO I'M DROPPING IT
# icpsr chamber congress
# 1 20343 House        108
# 2 20349 House        110

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)


save(member_data,
     file = here::here("data", "member_data.Rdata"))

head(member_data)

count(member_data, committees, reporting_agencies, chair, sort = T)
count(member_data, presidents_party, sort = T)

member_data |> filter(is.na(presidents_party))
