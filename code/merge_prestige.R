# THE BETTER DEFINITION WE USE IN THE FINAL PAPER
prestige_house <- c(
  "APPROPRIATIONS", #
  # "ARMED SERVICES", #
  #"BUDGET", #
  "COMMERCE", #
  "ENERGY", #
  "FINANCIAL SERVICES", #
  "BANKING", #
  #"FOREIGN"
  #"INTERNATIONAL RELATIONS", #
  # "JUDICIARY", #
  "FINANCIAL SERVICES", #
  "RULES", #
  "WAYS"
) %>% paste(collapse  = "|")#

prestige_senate <- c(
  #"AGRICULTURE",
  "APPROPRIATIONS", #
  "ARMED SERVICES", #
  #  "BANKING", #
  #"BUDGET", #
  #"COMMERCE", #
  #"ENERGY", #
  #  "NATURAL RESOURCES", #
  #  "ENVIRONMENT", #
  "FINANCE", #
  "FOREIGN RELATIONS") %>% paste(collapse  = "|")
#"HOMELAND SECURITY", #
#"JUDICIARY", #
# "INTELLIGENCE",
#"RULES") #

prestige_house |> str_split("\\|") |> kable(col.names = "House `Exclusive` Prestige Committees (CRS 2022, pg 3)")
prestige_senate |> str_split("\\|") |> kable(col.names = "Senate `Super A` Prestige Committees  (CRS 2022, pg 3)")

member_data %<>% ungroup() %>%
  mutate(prestige = ifelse(
    (chamber == "House" & str_detect(committees, prestige_house) ) | (chamber == "Senate" & str_detect(committees, prestige_senate)), 1, 0)
  )

member_data |> drop_na(prestige) |> count(prestige) |> kable()
