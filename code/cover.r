# make a placeholder cover image just for fun

# devtools::install_github("zmeers/ggparliament")
library(ggparliament)
# install.packages("tidyverse")
library(tidyverse)

#filter the election data for the most recent US House of Representatives
us_house <- election_data %>%
  filter(country == "USA" &
    year == 2016 &
    house == "Representatives")

us_house <- parliament_data(election_data = us_house,
  type = "semicircle",
  parl_rows = 10,
  party_seats = us_house$seats)


us_house$size <- rbinom(435, 1, .1)

#p <-
  ggplot(us_house) +
  aes(x, y, colour = party_short ) +
  geom_parliament_seats() +
  theme_void() +
  theme(legend.position = "none") +
  geom_emphasize_parliamentarians(size == 1)

ggsave(filename = here::here("cover.png"),
       width = 8.5,
       height = 5)
