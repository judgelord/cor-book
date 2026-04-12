# helper functions to plot predicted values
ggcompetitive <- function(predicted = predicted) {

  predicted$competitive %<>% str_replace("0", "Not competitive") %>% str_replace("1", "Competitive general")

  predicted$competitive_primary %<>% str_replace("0", "Not competitive primary") %>% str_replace("1", "Competitive primary")

  predicted$presidents_party %<>% str_replace("0", "Not President's Party") %>% str_replace("1", "President's Party")

  predicted %<>% drop_na(competitive_primary)

  predicted %>%
    ggplot() +
    aes(x = "",
        y = estimate, # predicted,
        ymin = conf.low,# predicted - 1.96*std.error,
        ymax = conf.high, #predicted + 1.96*std.error,
        fill = competitive,
        shape = competitive,
        color = competitive
    ) +
    geom_pointrange(position =  position_dodge(width = -.5)  )  +
    scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
    scale_color_viridis_d(begin = 0, end = .6, option = "cividis") +
    theme_minimal() +
    theme(panel.border  = element_blank(),
          panel.grid.major.x = element_blank()) + facet_grid(. ~ competitive_primary)
}


ggtenure <- function(predicted = predicted) {

  predicted %<>%
    # drop estimates from impossible values
    group_by(rowid,
             #predicted, std.error,
             estimate, conf.low, conf.high,
             competitive,
             competitive_primary) %>%
    mutate(sum = sum(first, second, third, fourth, fifth, sixth),
           more = ifelse(sum == 0, 1,0)) %>%
    filter(sum < 2) %>%
    pivot_longer(cols = c("first", "second", "third", "fourth", "fifth", "sixth", "more")) %>%
    select(name, value) %>%
    filter(value == 1) %>%
    # clean up for presentation
    mutate(year_in_congress = name %>%
             str_replace("more", "7\nor more") %>%
             str_replace("sixth", "6") %>%
             str_replace("fifth", "5") %>%
             str_replace("fourth", "4") %>%
             str_replace("third", "3") %>%
             str_replace("second", "2") %>%
             str_replace("first", "1")
    ) %>%
    ungroup()

  # Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values
  # predicted %<>% full_join(dcounts_tenure2 %>% mutate(predicted = NA))


  predicted$competitive %<>% str_replace("0", "Not competitive") %>% str_replace("1", "Competitive general")

  predicted$competitive_primary %<>% str_replace("0", "Not competitive primary") %>% str_replace("1", "Competitive primary")


  predicted %>%
    ungroup() %>%
    ggplot() +
    aes(x = year_in_congress,
        y = estimate, # predicted,
        ymin = conf.low,# predicted - 1.96*std.error,
        ymax = conf.high, #predicted + 1.96*std.error,
        shape = competitive_primary,
        color = competitive) +
    geom_pointrange(position =  position_dodge(width = -.3)  )  +
    scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
    scale_color_viridis_d(begin = 0, end = .6, option = "cividis") +
    theme_minimal() +
    theme(panel.border  = element_blank(),
          panel.grid.major.x = element_blank())

}

ggtenurep <- function(predicted = predicted) {

  predicted %<>%
    # drop estimates from impossible values
    group_by(rowid,
             #predicted, std.error,
             estimate, conf.low, conf.high,
             competitive_primary) %>%
    mutate(sum = sum(first, second, third, fourth, fifth, sixth),
           more = ifelse(sum == 0, 1,0)) %>%
    filter(sum < 2) %>%
    pivot_longer(cols = c("first", "second", "third", "fourth", "fifth", "sixth", "more")) %>%
    select(name, value) %>%
    filter(value == 1) %>%
    # clean up for presentation
    mutate(year_in_congress = name %>%
             str_replace("more", "7\nor more") %>%
             str_replace("sixth", "6") %>%
             str_replace("fifth", "5") %>%
             str_replace("fourth", "4") %>%
             str_replace("third", "3") %>%
             str_replace("second", "2") %>%
             str_replace("first", "1")
    ) %>%
    ungroup()

  predicted$competitive_primary %<>% str_replace("0", "Not competitive primary") %>% str_replace("1", "Competitive primary")



  # Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values
  # predicted %<>% full_join(dcounts_tenure2 %>% mutate(predicted = NA))


  predicted$competitive %<>% str_replace("0", "Not competitive") %>% str_replace("1", "Competitive general")


  predicted %>%
    ungroup() %>%
    ggplot() +
    aes(x = year_in_congress,
        y = estimate, # predicted,
        ymin = conf.low,# predicted - 1.96*std.error,
        ymax = conf.high, #predicted + 1.96*std.error,
        shape = competitive_primary,
        color = competitive_primary) +
    geom_pointrange(position =  position_dodge(width = -.3)  )  +
    scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
    scale_color_viridis_d(begin = 0, end = .6, option = "cividis") +
    theme_minimal() +
    theme(panel.border  = element_blank(),
          panel.grid.major.x = element_blank())

}
