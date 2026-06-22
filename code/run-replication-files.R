# get data from correspondence-data repo
here::here("code", "make_corr_counts_from_dcounts_min.R") |> source()
here::here("code", "make_member_data.R") |> source()

library(quarto)

# Directory --- change to "docs" when we want to publish
dir <- "replication_html"
cach = TRUE

# make total and type 1-5 datasets
here::here("code", "replication-data.qmd") |> quarto_render(cache_refresh = cache)

# run replication files
here::here("replication-1.qmd") |> quarto_render(cache_refresh = cache, output_file = here::here(dir, "replication-1.html"))
here::here("replication-2.qmd") |> quarto_render(cache_refresh = cache, output_file = here::here(dir, "replication-2.html"))
here::here("replication-3.qmd") |> quarto_render(cache_refresh = cache, output_file = here::here(dir, "replication-3.html"))
here::here("replication-4.qmd") |> quarto_render(cache_refresh = cache, output_file = here::here(dir, "replication-4.html"))
here::here("replication-5.qmd") |> quarto_render(cache_refresh = cache, output_file = here::here(dir, "replication-5.html"))
here::here("replication-total.qmd") |> quarto_render(cache_refresh = cache, output_file = here::here(dir, "replication-total.html"))
