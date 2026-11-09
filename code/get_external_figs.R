library(here)
library(stringr)

from_dir <- here("figs") |>
  str_replace("cor-book", "congressional_staff")

to_dir <- here("figs")

files_to_copy <- list.files(
  path = from_dir,
  full.names = TRUE
)

file.copy(
  from = files_to_copy,
  to = to_dir,
  overwrite = TRUE
)

