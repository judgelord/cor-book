ggtenure <- function(predicted = predicted) {

  predicted %<>%
    # drop estimates from impossible values
    group_by(rowid,
             #predicted, std.error,
             estimate, conf.low, conf.high,
             chair) %>%
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

  predicted %<>% filter(chair == 0 | name %in% c("sixth", "more"))

  predicted$chair %<>% str_replace("0", " Not\nchair") %>% str_replace("1", "Chair")


  predicted %>%
    ungroup() %>%
    ggplot() +
    aes(x = year_in_congress,
        y = estimate, # predicted,
        ymin = conf.low,# predicted - 1.96*std.error,
        ymax = conf.high, #predicted + 1.96*std.error,
        shape = chair,
        color = chair,
        label = ifelse(year_in_congress == "6", chair, NA) ) +
    geom_text_repel(direction = "y", size = 3,
                    min.segment.length = Inf,
                    hjust= -.15,
                    check_overlap = T) +
    geom_pointrange(position =  position_dodge(width = -.3)  )  +
    scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
    scale_color_viridis_d(begin = 0, end = .6, option = "cividis")  +
    theme(legend.position = "none")
}

ggtenure <- function(predicted = predicted) {

  predicted %<>% ungroup()

  # Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values
  # predicted %<>% full_join(dcounts_tenure2 %>% mutate(predicted = NA))

  predicted$chair %<>% str_replace("0", " Not\nchair") %>% str_replace("1", "Chair")

  predicted$experience %<>% str_replace("1", "3+") %>% str_replace("0", "1-2")


  predicted %>%
    ungroup() %>%
    ggplot() +
    aes(x = experience,
        y = estimate, # predicted,
        ymin = conf.low,# predicted - 1.96*std.error,
        ymax = conf.high, #predicted + 1.96*std.error,
        shape = chair,
        color = chair,
        label = chair ) +
    geom_text_repel(direction = "y", size = 3,
                    min.segment.length = Inf,
                    hjust= -.15,
                    check_overlap = T) +
    geom_pointrange(position =  position_dodge(width = -.3)  )  +
    scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
    scale_color_viridis_d(begin = 0, end = .6, option = "cividis")  +
    theme(legend.position = "none")
}

