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

  # NAs in predictions from observed values
  NAs <- m |>
    marginaleffects::predictions(newdata = d )  |>
    group_by(member_state, year) |>
    filter(is.na(estimate)) |>
    ungroup() |>
    distinct(member_state, agency,
             year)

  # counterfactual
  a <- m |>
    marginaleffects::predictions(newdata = newdata)  |>
    # drop NAs
    anti_join(NAs) |>
    percentiles() |>
    mutate(effect = "Counterfactual\n without effect")


  b <- m |>
    marginaleffects::predictions(newdata = d )  |>
    # drop NAs
    anti_join(NAs) |>
    percentiles()|>
    mutate(effect = "Predictions\n with effect")

gini_a <- unique(a$gini) |> round(2)
gini_b <- unique(b$gini) |> round(2)
total_a <- sum(a$mean)
total_b <- sum(b$mean)

# align member rankings by the unmodified data predictions
# this allows
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

  x <- full_join(a, b) |>
    group_by(member_state) |>
    mutate(diff = mean(mean) - mean ) |>
    arrange(member_state) |>
    ungroup()

  x |> filter(diff != 0)

  max_diff <- x |> slice_max(order_by = diff, n = 1, with_ties = F) # |> pull(member_state)
  max_diff

  x |> filter(mean == 0)

  p <- x |>
    #FIXME why are some 0
    #filter(mean>0) |>
    ggplot() +
    # we can only have a line by member satae if we align member rankings by the unmodified data predictions above
    geom_line(aes(group = member_state, x = Percentile,
                  y = mean), alpha = .5) +
    geom_point(aes(x = Percentile, y = mean,
                   color = effect), alpha = .5 ) +
    annotate(x = 0,
             y =  max(x$mean),
             hjust = "inward",
             vjust = "inward",
             geom = "text",
             size = 3.5,
             label = paste(effect, "effect:\n",
                           ifelse(total_b-total_a > 0, "+", ""), round(total_b-total_a) |> format(big.mark=",")  , "requests per year\n",
                           ifelse(gini_b-gini_a > 0, "+", ""), round(gini_b-gini_a, 2), "change in Gini coefficient"
                           #", from", gini_a, "to", gini_b,
                           # paste0(min(d$year), "-", max(d$year))
                           ),
             check_overlap = T) +
    # note the max diff member
    # annotate(x = max_diff$Percentile, y = max_diff$mean, label = max_diff$member_state, geom = "text_repel", size = 3.5, max.overlaps = 2, ylim = c(15,20), xlim = c(50, 100) ) +
    labs(
      x = "Percentile",
      y = "Predicted average requests per year") +
    scale_color_viridis_d(option = "cividis", begin = .3, end = .7) +
    theme(legend.position = c(.2, .6),
          legend.title = element_blank(),
          #         legend.box.background = element_rect(),
          legend.box.margin = unit(c(0,0,0,0), "mm"),
          legend.spacing.y = unit(0, 'cm'))  +
            ## important additional element
            guides(fill = guide_legend(byrow = TRUE))
  p
  return(p)

}

