source(here::here("code", "formatting.R"))

# directory to store model objects
if (!dir.exists(here::here("models"))) {dir.create(here::here("models"))}

# directory to store figures
if (!dir.exists(here::here("figs"))) {dir.create(here::here("figs"))}

knitr::opts_chunk$set(
  echo = T, # code is folded
  fig.width = 7.5,
  fig.height = 3.5,
  split = T,
  fig.align = 'center',
  fig.path='figs/',
  fig.retina = 6,
  out.width = "100%",
  warning = F,
  message = F)


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
library(here)
library(fs)



betas <- . %>% .$coefficients %>%
  round(3) %>%
  as_tibble(rownames = "beta") %>%
  pivot_wider(names_from = beta)


standard_errors <- . %>% .$se %>%
  round(3) %>% as_tibble(rownames = "se") %>%
  pivot_wider(names_from = se)


# plot defaults
library(ggplot2); theme_set(
  theme_minimal() +
    theme(
 )
)

library(ggplot2)
library(scales)

options(
  ggplot2.continuous.fill = NULL,
  ggplot2.continuous.colour = NULL,
  ggplot2.continuous.color = NULL
)

theme_set(
  theme_minimal() +
    theme(
      palette.colour.continuous = scales::pal_viridis(option = "cividis"),
      palette.fill.continuous   = scales::pal_viridis(option = "cividis"),
      palette.colour.discrete   = scales::pal_viridis(option = "cividis", direction = -1),
      palette.fill.discrete     = scales::pal_viridis(option = "cividis", direction = -1),
      # # FOR AJPS
      # panel.grid = element_blank(),
      # legend.position = "bottom",
      # # make text black, not grey
      # axis.text = element_text(color="black"),
      # axis.ticks = element_line(color = "black"),
      # # add space between labels and text
      # axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
      # axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm")),
      # plot.title = element_text(vjust = 1,
      #                           lineheight = 0,
      #                           margin = margin(0, 0, 0, 0)), # Margins (t, r, b, l)
      # # END FOR AJPS
      panel.border  = element_blank()
    )
)

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


options(knitr.duplicate.label = 'allow') # https://stackoverflow.com/questions/36868287/purl-within-knit-duplicate-label-error


