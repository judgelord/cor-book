

here::here("data", "members_committees_oversight.rda") |>
  str_replace("cor-book", "committees") |>
  load()


members_committees_oversight %<>%
  #FIXME in committees repo - this is a bad name
  rename(reporting_agencies = oversight) %>%
   # remove vars with NAs from merge of historical committee data in https://github.com/judgelord/committees/issues/1
  distinct(icpsr, congress, committees, reporting_agencies, chair, ranking_minority, titles, positions)


member_data <- member_data |>
  left_join(members_committees_oversight)

#TODO fix missing data  https://github.com/judgelord/committees/issues/4

if(FALSE){ # TESTING / LOOOKING FOR ERRORS
member_data |>
  count(is.na(committees))

  member_data |>
    filter(is.na(committees),
           !state %in% c("district of columbia", "puerto rico")) |>
    drop_na(state) |>
    count(icpsr, bioname, congress, chamber,
            state, district_code,
          committees,
              # chair,  ranking_minority,
             sort = T) |> arrange(district_code, bioname) |> kable()
          #congress)

  member_data |>
    count(is.na(reporting_agencies))

member_data |> count(positions, chair, ranking_minority)

member_data |> count(titles, chair, ranking_minority, sort = T) |>
  kable(format = "markdown")


# this missingness is noted in https://github.com/judgelord/committees/issues/4
look <- member_data |>
  filter(is.na(committees),
  #TODO does stewart data not cover the 105th?
  congress > 105) |>
  arrange(bioname)

look |>
  distinct(icpsr, bioname, congress, committees, positions) |>
  write_csv(file = here::here("data", "to correct by hand", "committee_corrections.csv"))
}


