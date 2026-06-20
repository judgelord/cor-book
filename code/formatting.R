library(scales)
library(here)
library(tinytable)

source(here::here("code", "modelsummary_formatting.R"))

# cach files in cache folder
input_file <- knitr::current_input()

doc_name <- tools::file_path_sans_ext(basename(input_file))

input_file <- knitr::current_input()

doc_name <- tools::file_path_sans_ext(basename(input_file))

knitr::opts_chunk$set(
  # cache = FALSE, # this is in _quarto.yml
  cache.path = file.path("cache", doc_name, ""),
  warning = F,
  message = F)

# inline numbers round to 2, comma at thousands
inline <- function(x) {
  if (is.na(as.numeric(x))) {
    return (x)
  } else
    return (as.numeric(x) |>
              round(2) |>
              format(big.mark=",")
    )
}

knitr::knit_hooks$set(inline = inline)

