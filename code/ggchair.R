# helper functions to plot predicted values
ggchair <- function(predicted = predicted) {

  predicted$chair %<>% str_replace("0", " Not\nchair") %>% str_replace("1", "Chair")

  predicted$presidents_party %<>% str_replace("0", "\nNot president's\nparty") %>% str_replace("1", "\nPresident's\nparty")

  predicted %>%
    ggplot() +
    aes(x = presidents_party,
        y = estimate, # predicted,
        ymin = conf.low,# predicted - 1.96*std.error,
        ymax = conf.high, #predicted + 1.96*std.error,
        fill = chair,
        shape = chair,
        color = chair,
        label = chair,
    ) +
    geom_text_repel(direction = "y", size = 3,
                    hjust= -.25,
                    min.segment.length = Inf,
                    force = 2,
                    vjust = 0,
                    check_overlap = T) +
    geom_pointrange(position =  position_dodge(width = -.1)  )  +
    scale_fill_viridis_d(begin = 0, end = .6, option = "cividis") +
    scale_color_viridis_d(begin = 0, end = .6, option = "cividis") +
    theme(legend.position = "none",
          axis.ticks.x = element_blank())
}
