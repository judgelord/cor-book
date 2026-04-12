
# directory to store model objects
if (!dir.exists(here::here("models"))) {dir.create(here::here("models"))}

# directory to store figures
if (!dir.exists(here::here("figs"))) {dir.create(here::here("figs"))}

options(knitr.duplicate.label = 'allow') # https://stackoverflow.com/questions/36868287/purl-within-knit-duplicate-label-error

start_time <- Sys.time()

# DUE TO BREAKING CHANGES, THIS CODE REQUIRES AN PRIOR VERSION OF MARGINALEFFECTS
if(packageVersion("marginaleffects") != "0.25.1"){
  remove.packages("marginaleffects")
  packageurl <- "http://cran.r-project.org/src/contrib/Archive/marginaleffects/marginaleffects_0.25.1.tar.gz"
  install.packages(packageurl, repos=NULL, type="source")
}
# as described in the warning below, the uncertainty intervals around predictions do not include uncertainty in the estimates of the fixed effects. Since we are just using predictions to illustrate the model results, not make any claims about particular years, legislators, or agencies (the fixed effects), the uncertainty in the fixed effect estimates is not relevant for our use case (illustrating main effects, for which we do have estimates of uncertainty). However, we wish to alert users of these code that they should not use the predictions to make such claims.

requires <- c("tidyverse", "magrittr", # data wrangling
              "scales", # plot scales
              "ggrepel",# plot labels
              "here", # file paths
              "knitr", # document formatting
              "kableExtra", # for table formatting
              "modelsummary", # for regression tables
              "marginaleffects", # for predictions
              "fixest", # statistical package for fixed effects estimation
              "ineq", # to calculate GINI coefficients
              # MAY BE REQUIRED FOR QMD RENDERING
              "base64enc",
              "digest",
              "evaluate",
              "glue",
              "highr",
              "htmltools",
              "jsonlite",
              "markdown",
              "mime",
              "rmarkdown",
              "stringi",
              "stringr",
              "xfun",
              "yaml")

to_install <- c(requires %in% rownames(installed.packages()) == FALSE)

if(sum(to_install>0)){
  install.packages( requires[to_install],
                    repos = "https://cloud.r-project.org/" )
}

library(modelsummary)
library(marginaleffects)
library(fixest)
library(tidyverse)
library(magrittr)
library(ineq)
library(kableExtra)
library(ggrepel)
library(scales)

knitr::opts_chunk$set(echo = T, # code is folded
                      cache = T, # CACHE
                      fig.width = 4.5,
                      fig.height = 3.5,
                      split = T,
                      fig.align = 'center',
                      fig.path='figs/',
                      fig.retina = 6,
                      out.width = "100%",
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

# plot defaults
library(ggplot2); theme_set(
  theme_minimal() +
    theme(# FOR AJPS
      panel.grid = element_blank(),
      legend.position = "bottom",
      # make text black, not grey
      axis.text = element_text(color="black"),
      axis.ticks = element_line(color = "black"),
      # add space between labels and text
      axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
      axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
      plot.title = element_text(vjust = 1,
                                lineheight = 0,
                                margin = margin(0, 0, 0, 0)), # Margins (t, r, b, l)
      # END FOR AJPS
      panel.border  = element_blank(),
      panel.grid.major.x = element_blank())
)
options(
  ggplot2.continuous.color = "cividis",
  ggplot2.continuous.fill = "cividis"
)
scale_color_discrete <- function(...)
  scale_color_viridis_d(..., direction = -1)
scale_fill_discrete <- function(...)
  scale_fill_viridis_d(..., direction = -1)


# html table formatting
kablebox <- . %>%
  head(100) %>%
  knitr::kable() %>%
  kableExtra::kable_styling() %>%
  kableExtra::scroll_box(height = "200px")

kablebox_long <- . %>%
  head(100) %>%
  knitr::kable() %>%
  kableExtra::kable_styling() %>%
  kableExtra::scroll_box(height = "500px")
