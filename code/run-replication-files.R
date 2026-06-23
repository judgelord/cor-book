# get data from correspondence-data repo
here::here("code", "make_corr_counts_from_dcounts_min.R") |> source()
here::here("code", "make_member_data.R") |> source()

# make total and type 1-5 datasets
here::here("code", "replication-data.qmd") |> quarto::quarto_render(cache_refresh = TRUE)

# run all 6 replication files
quarto::quarto_render(
  input = here::here(),
  profile = "replication",
  cache_refresh = cache
)
