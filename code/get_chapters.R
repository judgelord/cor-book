# This script downloads the chapters as markdown files to be included in the book rendered with docs/book.qmd

# you need to provide and email to authorize google drive
email = "devin.jl@gmail.com"

library(tidyverse)
library(googledrive)
drive_auth(email = email)

folder_id <- drive_find(n_max = 10,
                        type = "folder",
                        pattern = paste0("Correspondence Book"))$id
folder_id


#folder link to id
folder = "https://drive.google.com/drive/folders/1glyNPbSjURqFy1IWCGrw9YoLxE15_v_b"
folder_id = drive_get(as_id(folder))

# list files in folder
files = drive_ls(folder_id, type = "document")

# download each as .txt and rename as to .md
get_chapter <- function(drive_file){

file = here::here("docs", drive_file$name)

drive_download(drive_file,
               type = "txt",
               path = file,
               overwrite = T)

# clean up text
readLines( paste0(file, ".txt") ) |>
  # remove comments
  str_remove_all("\\[.\\]") |>
  # remove empty space at beginning of lines
  str_squish() |>
  # fix headers
  str_replace_all("#([A-Z])", "## \\1") |>
  # save as .md
  write_lines(paste0(file, ".md") |> str_replace(": ", " - ") )

# clean up folder, deleting txt files, keeping only md files
file.remove( paste0(file, ".txt") )

# file.rename(paste0(file, ".txt"),
#             paste0(file, ".md") |> str_replace(": ", " - ") )
}

for(i in 1:nrow(files)){
  get_chapter(files[i,])
}


# list files to paste into quarto.yml (only required when we change google doc names)
chapters <- list.files(here::here("docs"))
paste0("   - docs/", chapters) |> knitr::kable(format = "simple")


# download current bib file
drive_download(as_id("15O-L5AcEl7P2S82aQ7le3FJFoSEXYHGV"),
               path = here::here("assets", "congress.bib"),
               overwrite = T)

# move index to root
file.rename(here::here("docs", "index.md"),
            here::here("index.md") )


# render book
system(command = "quarto render")           # render all formats

# system(command = "quarto preview")           # preview

# system(command = "quarto render --to pdf")  # render PDF format only
