library(tidyverse)
library(foreign)
library(haven)
library(knitr)

CELSenate93to118Reduced <- read_dta("data/les/CELSenate93to118Reduced.dta")
CELHouse93to118Reduced <- read_dta("data/les/CELHouse93to118Reduced.dta")

lep <- full_join(CELSenate93to118Reduced |>
                   mutate(chamber = "Senate"),
                 CELHouse93to118Reduced|>
                   mutate(chamber = "House")
)

names(lep)

les <- lep |> select(chamber, icpsr, year, congress, female, afam, latino,
              votepct, chair, subchr, seniority, state_leg, freshman, deleg_size, born,
              # les2, expectation2, # confused about 99% missingness post merge, since these don't seem missing here
              lesclassic)

count(lep, !is.na(les2), congress) |> kable()



count(les, !is.na(lesclassic))

save(les, file = here::here("data", "les", "les.rda"))

