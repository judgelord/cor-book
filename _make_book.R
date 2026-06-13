if(F){
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
}

# if you want, preview the book in HTML (the pdf is in the /docs/ folder)
quarto::quarto_preview()

# to stop the preview (required to run other code)
quarto::quarto_preview_stop()

# send to git (just Devin for now)
if(F){
  system(command = "git add docs")
  system(command = "git restore --staged docs/interbranch.pdf")
  system(command = "git restore --staged docs/slides/interbranch.html")
  system(command = "git restore --staged docs/Unequal-Invisible-Representation.pdf") # only sometimes push pdf
  system(command = 'git commit -m "Publish site to docs/"')
  system(command = "git push")
}
