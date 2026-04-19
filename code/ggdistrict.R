
ggdistrict <- function(predicted = predicted, party = FALSE) {

predicted %<>%
  # drop estimates from impossible values
      group_by(rowid,
               #predicted, std.error,
               estimate, conf.low, conf.high,
               same_party) %>%
  mutate(sum = sum(new_member, second, third, fourth, fifth, sixth),
         more = ifelse(sum == 0, 1,0)) %>%
    filter(sum < 2) %>%
    pivot_longer(cols = c("new_member", "second", "third", "fourth", "fifth", "sixth", "more")) %>%
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
             str_replace("new_member", " New\nmember")
             ) %>%
    ungroup()

# Ideally, we could plot against data, but there is so much variation that you can no longer distinguish differences in predicted values
# predicted %<>% full_join(dcounts_total2 %>% mutate(predicted = NA))




p <- predicted %>%
  ungroup() %>%
  ggplot() +
  aes(x = year_in_congress,
            y = estimate, # predicted,
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high) +
  geom_pointrange(position =  position_dodge(width = -.3)  )  +
  # geom_line() + # requires numeric...look to see how I did this on the other ones
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis")

if(party){
p <- predicted %>%
  mutate(same_party = as.logical(same_party)) %>%
  ungroup() %>%
  ggplot() +
  aes(x = year_in_congress,
            y = estimate, # predicted,
      ymin = conf.low,# predicted - 1.96*std.error,
      ymax = conf.high, #predicted + 1.96*std.error,
      shape = same_party,
      color = same_party) +
  geom_pointrange(position =  position_dodge(width = -.3)  )  +
  scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
  scale_color_viridis_d(begin = 0, end = .6, option = "cividis")
}

return(p)

}

