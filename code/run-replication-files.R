# get data from correspondence-data repo
here::here("code", "make_corr_counts_from_dcounts_min.R") |> source()

library(quarto)

# make total and type 1-5 datasets
here::here("code", "replication-data.qmd") |> quarto_render(cache_refresh = TRUE)

# run replication files
here::here("replication-1.qmd") |> quarto_render(cache_refresh = TRUE)
here::here("replication-2.qmd") |> quarto_render(cache_refresh = TRUE)
here::here("replication-3.qmd") |> quarto_render(cache_refresh = TRUE)
here::here("replication-4.qmd") |> quarto_render(cache_refresh = TRUE)
here::here("replication-5.qmd") |> quarto_render(cache_refresh = TRUE)
here::here("replication-total.qmd") |> quarto_render(cache_refresh = TRUE)
