# combines files saved in replication_styles.qmd called by replication.qmd

here::here("models", "total",  "m_style_cross.rda") |> load()
m_style_cross_total <- m_style_cross

here::here("models", 1,  "m_style_cross.rda") |> load()
m_style_cross_1 <- m_style_cross

here::here("models", 2,  "m_style_cross.rda") |> load()
m_style_cross_2 <- m_style_cross

here::here("models", 3,  "m_style_cross.rda") |> load()
m_style_cross_3 <- m_style_cross

here::here("models", 4,  "m_style_cross.rda") |> load()
m_style_cross_4 <- m_style_cross

here::here("models", 5,  "m_style_cross.rda") |> load()
m_style_cross_5 <- m_style_cross

models <- list(
  "(1)" = m_style_cross_1,
  "(2)" = m_style_cross_2,
  "(3)" = m_style_cross_3,
  "(4)" = m_style_cross_4,
  "(5)" = m_style_cross_5,
  "(6)" = m_style_cross_total)

rows <- tibble(
  term = c("Dependent variable"),
  `(1)` = c("Const."),
  `(2)` =c("Corp"),
  `(3)` = c("NGO/Govt"),
  `(4)` = c("Policy"),
  `(5)` = c("Corp Pol"),
  `(6)` = c("Total")

)

# call coefs
attr(rows, 'position') <- c(0)


modelsummary(models, add_rows = rows, gm, cm)

save(models, rows, gm, cm,
     file = here::here("models", "m_style_cross_combined.rda"))

