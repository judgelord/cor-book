## The data on members
## from voteview + committee data from Stewart and Wu + @unitedstates project
#TODO merge both sources of committee data first and then with voteview
##  also including population from the census

# VOTEVIEW HOUSE AND SENATE
members_raw <- read_csv(here::here("data", "HSall_members.csv"))

members_raw <- read_csv("https://voteview.com/static/data/out/members/HSall_members.csv")

member_data <- members_raw %>%
  select(icpsr, bioname, congress, chamber, party_code, state_abbrev, district_code) %>%
  filter(congress > 102) %>% # NOTE SUBSETTING TO 102th
  mutate(
    party = case_when(
      party_code == 100 ~ "(D)",
      party_code == 200 ~ "(R)",
      party_code == 328 ~ "(I)",
      F ~ NA
    ) )

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

look <- member_data |> count(oversight, committees) |> head(100)

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

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

# Party size data
# From voteview.com https://voteview.com/articles/data_help_parties
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

member_data %<>%
  ungroup() %>%
  group_by(bioname) %>%
  # first year
  mutate(first_cong = min(congress),
         first_year = 1787 + 2*first_cong) %>%
  # subset to 2007-2020 where we have reliable correspondence count data
  # filter(congress > 109, congress < 117) %>%
  ungroup()

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)

member_data |> count(state_abbrev) |> kable()

################
# STATE DATA
here::here("code", "merge_state_data.R") |> source()

member_data |> count(state) |> kable()


# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)



################
# ELECTIONS DATSA
here::here("code", "merge_same_party.R") |> source()

# confirm no duplicates in member_data post merge
member_data <- distinct(member_data)
dim(member_data)
member_data |> count(icpsr, chamber, congress, sort = T) |> filter(n>1)


################
# ELECTIONS DATSA
here::here("code", "merge_electoral.R") |> source()

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

load(here::here("data", "Staff", "staff.rds"))

member_data <- left_join(member_data, staff)

# transform spending to be spending in millions
member_data %<>% mutate(est_legis_spending = est_total_legis_spending/1000000,
                          est_pol_spending = est_total_pol_spending/1000000,
                          est_comm_spending = est_total_comm_spending/1000000,
                          est_off_spending = est_total_off_spending/1000000,
                          est_constit_spending = est_total_constit_spending/1000000)

member_data %<>% select(-votepct_sq)
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

count(member_data, committees, oversight, chair, sort = T)
count(member_data, presidents_party, sort = T)

