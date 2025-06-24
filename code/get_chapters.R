# This script downloads the chapters as markdown files to be included in the book rendered with docs/book.qmd
library(tidyverse)

# "Correspondence Book" folder URL / id
folder = "https://drive.google.com/drive/folders/1glyNPbSjURqFy1IWCGrw9YoLxE15_v_b"

# list files in folder
drive_files = drive_ls(drive_get(as_id(folder)), type = "document")

# download each chapter as .txt, clean up, and rename as to .md
get_chapter <- function(drive_file){

file_name = here::here(drive_file$name)

drive_download(drive_file,
               type = "txt",
               path = file_name,
               overwrite = T)

# clean up text
readLines( paste0(file_name, ".txt") ) |>
  # remove comments
  str_remove_all("\\[.\\]") |>
  # remove empty space at beginning of lines
  str_squish() |>
  # fix headers
  str_replace_all("#([A-Z])", "## \\1") |>
  # save as .md
  write_lines(paste0(file_name, ".md") |> str_replace(": ", " - ") )

# clean up folder, deleting txt files, keeping only md files
file.remove( paste0(file_name, ".txt") )

} # END FUNCTION


# APPLY FUNCTION TO DOWNLOAD GOOGLE DRIVE FILES
for(i in 1:nrow(drive_files)){
  get_chapter(drive_files[i,])
}

# list files to paste into quarto.yml (only required when we change google doc names)
paste0("   - ", list.files(here::here())) |> knitr::kable(format = "simple")

# download bib file
drive_download(as_id("15O-L5AcEl7P2S82aQ7le3FJFoSEXYHGV"),
               path = here::here("assets", "congress.bib"),
               overwrite = T)

# render book
system(command = "quarto render")           # render all formats

# system(command = "quarto preview")           # preview

# system(command = "quarto render --to epub")  # render PDF format only
