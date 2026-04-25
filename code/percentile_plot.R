plot_data <- function(d) {
  d1 <- d %>%
    ungroup() %>%
    group_by(member_state, year, party) %>%
    summarise(n = sum(perYear, na.rm = T) ) %>%
    ungroup() %>%
    mutate(gini = Gini(n) ) %>%
    dplyr::group_by(member_state,party) %>%
    dplyr::summarise(mean = mean(n),
                     gini = mean(gini),
                     total = sum(n)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Percentile = dplyr::ntile(mean, 100),
                  rank = dplyr::min_rank(-mean)) %>%
    dplyr::select(member_state, Percentile, mean, rank, gini, total, party) %>%
    dplyr::distinct() %>%
    arrange(member_state)

d1$rank |> range()
  gini <- unique(d1$gini) |> round(3)


d1 |>
    ggplot() +
    geom_col(aes(x = Percentile,
                 y = mean),
             alpha = .1,
             fill = "grey",
             width = .000001,
             position = "dodge") +
    geom_jitter(aes(x = Percentile, y = mean, color= party), alpha = 1 ) +
    annotate(x = 30, y =  max(d1$mean, na.rm = T)-.1*max(d1$mean, na.rm = T),
             geom = "text",
             size = 3.5,
             label = paste("Gini coefficient\n=", gini)
             ) +
  geom_text_repel(aes(x = Percentile, y = mean,
                label = ifelse(Percentile > 80,
                               member_state,
                               "")),
                xlim = c(50,90),
                max.overlaps = 7,
            check_overlap = T,
            size = 3,
            hjust = "inward") +
    labs(
      x = "Percentile in Chamber",
      y = "Average requests per year") +
    scale_color_viridis_d(option = 1, begin = .3, end = .7) +
    theme(legend.position = c(.90, .12),
          legend.title = element_blank(),
          #         legend.box.background = element_rect(),
          legend.box.margin = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, 'cm'))  +
    ## important additional element
    guides(fill = guide_legend(byrow = TRUE))

}


