

here::here("data", "members_committees_oversight.rda") |>
  str_replace("cor-book", "committees") |>
  load()


members_committees_oversight %<>%
  # remove vars with NAs from merge of historical committee data in https://github.com/judgelord/committees/issues/1
  select(icpsr, congress, committees, oversight, chair, ranking_minority, titles, positions)


member_data <- member_data |>
  left_join(members_committees_oversight)


if(FALSE){ # TESTING / LOOOKING FOR ERRORS
member_data |>
  count(is.na(committees))

  member_data |>
    count(is.na(oversight))

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
