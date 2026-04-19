load( here::here("data", "rcl_ideology_estimates.rda") )

rcl <- rcl_ideology_estimates |> transmute(
  agency = department_agency_acronym,
  agency_ideo = X.ideo_rating.
)

# d <- left_join(d, member_data)

post_merge_transformations <- function(d){
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

# indicator for oversight committee
# d$oversight_agencies |> unique()
# d$agency |> unique()

d <- d |>
  mutate(
    oversight_agencies = str_replace_all(oversight, ";","|") |>
      replace_na("404"),
    oversight = str_detect(agency, oversight_agencies) |> as.numeric()
  )

d <- d |> left_join(rcl) # |> drop_na(agency_ideo) |>  count(agency, agency_ideo)


}

