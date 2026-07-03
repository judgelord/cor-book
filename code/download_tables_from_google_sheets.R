
library(tidyverse)
library(googlesheets4)

gs4_auth(email = "correspondenceresearch@gmail.com")

save_sheet_csv <- function(sheet_url, name, folder = "tables") {
  dir_create(folder)

  if (!str_ends(name, "\\.csv")) {
    name <- str_c(name, ".csv")
  }

  output_path <- path(folder, name)

  sheet_data <- read_sheet(sheet_url)

  write_csv(sheet_data, output_path)

  output_path
}

  save_sheet_csv(
    sheet_url = "https://docs.google.com/spreadsheets/d/1sMpIj7TmRv9q1f3PXgoHEpvdnJ7NaU2rJ0IB47_XHig/edit?gid=1742403102#gid=1742403102",
    name = "strategiccombined"
  )
  save_sheet_csv(
    sheet_url = "https://docs.google.com/spreadsheets/d/18yOnc9l0aYkMbqHTgA-Q2Sw5NTFxgCyXCPiQu72yMhQ/edit?gid=1407239356#gid=1407239356",
    name = "capacity"
  )
  save_sheet_csv(
    sheet_url = "https://docs.google.com/spreadsheets/d/1a_F6OkpuVxTk0eWoXHJAbkjmN5ZC-TIdIGKRDUyQ8UI/edit?gid=1970651904#gid=1970651904",
    name = "demand"
  )
  save_sheet_csv(
    sheet_url = "https://docs.google.com/spreadsheets/d/1fs_yTpe1S2mgOT0GkzZ1TmuOTn4KpflUoYjtNOcG9hM/edit?gid=1009153319#gid=1009153319",
    name = "findings"
  )

  save_sheet_csv(
    sheet_url = "https://docs.google.com/spreadsheets/d/1stZTAPyp7I9GDaUbBwLrDqsOTEOlstD1tcjazv79bAU/edit?gid=0#gid=0",
    name = "results"
  )

