plot_data <- function(d) {
  d1 <- d %>%
    ungroup() %>%
    group_by(member_state, year) %>%
    summarise(n = sum(perYear, na.rm = T) ) %>%
    ungroup() %>%
    mutate(gini = Gini(n) ) %>%
    dplyr::group_by(member_state) %>%
    dplyr::summarise(mean = mean(n),
                     gini = mean(gini),
                     total = sum(n)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Percentile = dplyr::ntile(mean, 100),
                  rank = dplyr::min_rank(-mean)) %>%
    dplyr::select(member_state, Percentile, mean, rank, gini, total) %>%
    dplyr::distinct() %>%
    arrange(member_state)

d1$rank |> range()
  gini <- unique(d1$gini) |> round(3)


d1 |>
    ggplot() +
    geom_col(aes(x = Percentile,
                 y = mean),
             color = "grey",
             width = .000001,
             fill = "black",
             position = "dodge") +
    geom_point(aes(x = Percentile, y = mean), alpha = .5,color = "light blue" ) +
    annotate(x = 30, y =  max(d1$mean, na.rm = T)-.1*max(d1$mean, na.rm = T),
             geom = "text",
             size = 3.5,
             label = paste("Gini coefficient =", gini)
             ) +
  geom_text(aes(x = Percentile, y = mean,
                label = ifelse(Percentile > 80,
                               member_state,
                               "")),
            check_overlap = T,
            size = 3,
            hjust = "inward") +
    labs(
      x = "Percentile in Chamber",
      y = "Predicted average requests per year") +
    scale_color_viridis_d(option = "cividis", begin = .3, end = .7) +
    theme(legend.position = c(.8, .10),
          legend.title = element_blank(),
          #         legend.box.background = element_rect(),
          legend.box.margin = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, 'cm'))  +
    ## important additional element
    guides(fill = guide_legend(byrow = TRUE))

}


