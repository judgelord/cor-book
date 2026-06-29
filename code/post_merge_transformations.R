load( here::here("data", "rcl_ideology_estimates.rda") )

rcl <- rcl_ideology_estimates |> transmute(
  # RENAME TO MATCH
  # (THIS REALLY SHOULD HAVE BEEN STANDARDIZED IN THE SCRIPT THAT CREATED THESE DATA)
  agency = department_agency_acronym |>
    str_replace("HHS_", "DHHS_") |>
    str_replace("DHSICE", "DHS_ICE"),
  # CREATE DEPT IDEOLOGY SCORE
  department = str_remove(agency, "_.*"),
  agency_ideo = X.ideo_rating.,
  department_ideo = X.ideo_rating.
)

if(F){
rcl |> full_join( distinct(d, agency) )
}


# CENSUS DATA
load( here::here("data", "census", "census.rda") )

cen <- census |>
  select(c(
    "year","state_abbrev", "chamber", "district_code",
    "population_total",
    contains("percent"),
    contains("workers_"),
    contains("bachelor_"),
    contains("snap_"),
    -ends_with("_moe"))
    )

names(cen)

add <- cen |> filter(year == 2010)

add2000 <- add |> mutate(year = 2000)
add2001 <- add |> mutate(year = 2001)
add2002 <- add |> mutate(year = 2002)
add2003 <- add |> mutate(year = 2003)
add2004 <- add |> mutate(year = 2004)
add2005 <- add |> mutate(year = 2005)
add2006 <- add |> mutate(year = 2006)
add2007 <- add |> mutate(year = 2007)
add2008 <- add |> mutate(year = 2008)
add2009 <- add |> mutate(year = 2009)

cen<- cen |>
  full_join(add2000) |>
  full_join(add2001) |>
  full_join(add2002) |>
  full_join(add2003) |>
  full_join(add2004) |>
  full_join(add2005) |>
  full_join(add2006) |>
  full_join(add2007) |>
  full_join(add2008) |>
  full_join(add2009)

save(cen, file = here::here("data", "cen.rds"))

# d <- left_join(d, member_data)

post_merge_transformations <- function(d){

  # merge in census data on year, state, district, chamber
  d <- left_join(d, cen)

  nrow(d)
# tenure in office for experience tests
d <- d |>
  group_by(icpsr) |>
  mutate(
    tenure = year - first_year,
    first = ifelse(tenure==0, 1, 0),
    second = ifelse(tenure==1, 1, 0),
    third = ifelse(tenure==2, 1, 0),
    fourth = ifelse(tenure==3, 1, 0),
    fifth = ifelse(tenure==4, 1, 0),
    sixth = ifelse(tenure==5, 1, 0),
    first_two = tenure < 2,
    max_year = max(tenure)
  )

head(d$max_year)
nrow(d)

# indicator for whether they survived their first election
d <- d |>
  mutate(survive = ifelse(
    chamber =='House' & max_year>1 | chamber=='Senate' & max_year>5,
    1, 0)
  )

# indicator for whether they survived their first election
d <- d |>
  mutate(survive = ifelse(
    chamber =='House' & max_year>1 | chamber=='Senate' & max_year>5,
    1, 0)
  )

# indicator for reporting_agencies to committee
# d$reporting_agencies |> unique()
# d$agency |> unique()


#TODO THIS IS LOOKING FOR THE AGENCY IN THE LIST OF COMMITTEES' JURISTICTIONS; WE COULD DO THE REVERSE, but we would need to be careful about NAs and blanks (which are not in agency)
# UPDATE I THINK THIS IS THE WAY WE SHOULD BE DOING IT. SEE FOIAagenices in ACUS, we should have 100% coverage
#TODO confirm that department acronyms are not in sub-agency acronyms - yielding false positives
d <- d |>
  mutate(
    reporting_agencies = str_replace_all(reporting_agencies, ";","|") |>
      replace_na("404") |>
      str_replace("^NA$", "404"),
    oversight = str_detect(reporting_agencies,
                           # department - level --- this is generally good but could be more precise - It will over match when there is split-jurisdiction
                           #TODO we need a crosswalk with our agencies and lewis and selin using the agency crosswalk
                           agency |> str_remove("_.*")
                           ) |>
      as.numeric(),
    oversight = ifelse(is.na(committees), NA, oversight)
  )

nrow(d)

# confirm that NAs propegated - these should be the same
d$committees |> is.na() |> sum()
d$oversight |> is.na() |> sum()

# inspect
count(d, oversight, reporting_agencies, agency)

# inspect matches
count(d, oversight, reporting_agencies, agency) |> filter(oversight ==1)

ACUSagencies <- d$reporting_agencies |> str_split("\\|") |> unlist() |> unique()
FOIAagencies <- d$agency |> unique()

# FOIAagencies agencies in ACUS
FOIAagencies[str_detect(FOIAagencies, ACUSagencies |> str_c(collapse = "|")) ]

# FOIAagencies AGENCIES missing from ACUS
FOIAagencies[!str_detect(FOIAagencies, ACUSagencies |> str_c(collapse = "|")) ]

nrow(d)
#############################
# IDEOLOGY

d <- d |>
# by agency
  left_join(rcl |>
              distinct(agency, agency_ideo)
            ) |>
  # by department average
  mutate(  department = str_remove(agency, "_.*") ) |>
  left_join(rcl |>
              group_by(department) |>
              summarise(department_ideo = mean(department_ideo) ) ) |>
  # complete missing agency with department
  mutate(agency_ideo = coalesce(agency_ideo, department_ideo) |>
           #FIXME https://github.com/judgelord/cor-book/issues/7
           replace_na(0)
         )
nrow(d)

return(d)
}

# INSPECT
if(F){
  d |> filter(is.na(agency_ideo) ) |>
    count(agency)

  # MISSING 6 extremely low salience agencies from RCL
  # 1 ABMC    1651
  # 2 CSOSA   8191
  # 3 NCPC    3302
  # 4 NWTRB   1106
  # 5 PRC     6038
  # 6 RRB     6038
}

