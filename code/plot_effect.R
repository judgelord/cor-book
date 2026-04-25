percentiles <- function(a){
  a %>% #filter(is.na(estimate))
    #drop_na(estimate) %>%
    ungroup() %>%
    group_by(member_state, year) %>%
    summarise(n = sum(estimate, na.rm = T) ) %>%
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
}


plot_effect <- function(m = model, d = data, newdata, effect){

  a <- m |>
    marginaleffects::predictions(newdata = newdata) |>
    percentiles() |>
    mutate(effect = "Without effect")

  b <- m |>
    predictions(newdata = d )  |>
    percentiles()|>
    mutate(effect = "With effect")

gini_a <- unique(a$gini) |> round(2)
gini_b <- unique(b$gini) |> round(2)
total_a <- sum(a$mean)
total_b <- sum(b$mean)

# align member rankings by the unmodified data predictions
  a$Percentile <- b$Percentile

  # testing
  if(F){
a$member_state == b$member_state

a[which(a$mean > b$mean),]

  a <- a |> select(-Percentile) |>
    left_join(
      select(b, Percentile, member_state)
    )
  }

  x <- full_join(a, b)

  x |> filter(mean == 0)

  p <- x |>
    #FIXME why are some 0
    filter(mean>0) |>
    ggplot() +
    # geom_col(aes(x = Percentile,
    #              y = mean),
    #          color = "grey",
    #          width = .000001,
    #          fill = "black",
    #          position = "dodge") +
    geom_line(aes(group = member_state, x = Percentile,
                  y = mean), alpha = .5) +
    geom_point(aes(x = Percentile, y = mean,
                   color = effect), alpha = .5 ) +
    annotate(x = 50, y =  max(x$mean)-.1*max(x$mean),
             geom = "text",
             size = 3.5,
             label = paste(effect, "effect:\n",
                           ifelse(total_b-total_a > 0, "+", ""), round(total_b-total_a) |> format(big.mark=",")  , "requests per year\n",
                           ifelse(gini_b-gini_a > 0, "+", ""), round(gini_b-gini_a, 2), "change in Gini coefficient"
                           #", from", gini_a, "to", gini_b,
                           # paste0(min(d$year), "-", max(d$year))
                           ),
             check_overlap = T) +
    labs(
      x = "Percentile",
      y = "Predicted average requests per year") +
    scale_color_viridis_d(option = "cividis", begin = .3, end = .7) +
    theme(legend.position = c(.8, .10),
          legend.title = element_blank(),
          #         legend.box.background = element_rect(),
          legend.box.margin = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, 'cm'))  +
            ## important additional element
            guides(fill = guide_legend(byrow = TRUE))
  p
  return(p)

}

