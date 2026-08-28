if(F){
  # get data from correspondence-data repo
  here::here("code", "make_corr_counts_from_dcounts_min.R") |> source()
  # make member covariate data
  here::here("code", "make_member_data.R") |> source()
  # make total and type 1-5 datasets
  here::here("code", "replication-data.qmd") |> quarto::quarto_render(cache_refresh = TRUE)
  # run one replication file at a time
  refresh = T # refresh if data or models used in manuscript tables changed, don't if just adding new things
  job = T

  here::here("replication-figs.qmd") |> quarto::quarto_render(cache_refresh = refresh, as_job = job)

  here::here("replication-total.qmd") |> quarto::quarto_render(cache_refresh = refresh, as_job = job)
  here::here("replication-1.qmd") |> quarto::quarto_render(cache_refresh = refresh, as_job = job)
  here::here("replication-2.qmd") |> quarto::quarto_render(cache_refresh = refresh, as_job = job)
  here::here("replication-3.qmd") |> quarto::quarto_render(cache_refresh = refresh, as_job = job)
  here::here("replication-4.qmd") |> quarto::quarto_render(cache_refresh = refresh, as_job = job)
  here::here("replication-5.qmd") |> quarto::quarto_render(cache_refresh = refresh, as_job = job)

  # run all 6 replication files
  quarto::quarto_render(input = here::here(), profile = "replication", cache_refresh = refresh)

  # refresh CSVs from google drive
  source("code/download_tables_from_google_sheets.R")
}

# if you want, preview the book in HTML (the pdf is in the /docs/ folder)
quarto::quarto_preview()

# to stop the preview (required to run other code)
quarto::quarto_preview_stop()

# render the book, refreshing cached tables and figures
quarto::quarto_render(as_job = T, cache_refresh = T)

# send to git (just Devin for now)
if(F){
  system(command = "git add docs")
  system(command = "git restore --staged docs/interbranch.pdf")
  system(command = "git restore --staged docs/slides/interbranch.html")
  system(command = "git restore --staged docs/Unequal-Invisible-Representation.pdf") # only sometimes push pdf
  system(command = 'git commit -m "Publish site to docs/"')
  system(command = "git push")
}
