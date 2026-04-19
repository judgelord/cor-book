library(magrittr)

# Regression table formatting for AJPS
modelsummary_AJPS <- function(models, notes = "", center_rows = 1, ...){
  modelsummary::modelsummary(models,
                             # Custom sig stars for AJPS
                             stars = c('†' = .1, '*' = .05, '**' = .01),
                             # Align coefficients by decimal for AJPS
                             #align = paste0("l", paste0(rep("d", length(models)), collapse = "")),
                             add_rows = rows,
                             coef_map = cm,
                             gof_map = gm,
                             output = "tinytable",
                             notes = notes) |>
    # bold header, hline bottom, aligned center
    tinytable::style_tt(i = 0:1, bold = T, line = "b",  align = "c") |>
    # stats aligned center
    tinytable::style_tt(i = center_rows, align = "c") |>
    # row labels left
    tinytable::style_tt(j = 1, align = "l") |>
    tinytable::style_tt(fontsize = .7)
}

modelsummary <- modelsummary_AJPS


betas <- . %>% .$coefficients %>%
  round(3) %>%
  as_tibble(rownames = "beta") %>%
  pivot_wider(names_from = beta)


standard_errors <- . %>% .$se %>%
  round(3) %>% as_tibble(rownames = "se") %>%
  pivot_wider(names_from = se)
