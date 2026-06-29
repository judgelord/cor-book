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
                             notes = notes)  |>
    # bold header, hline bottom, aligned center
    tinytable::style_tt(j = 1, align = "l") |>
    tinytable::style_tt(i = 0:1, bold = T, line = "b",  align = "c") |>
  # stats aligned center
  #  tinytable::style_tt(i = center_rows, align = "c") |>
  # row labels left
   tinytable::style_tt(j = 1, align = "l") # |>  tinytable::style_tt(fontsize = .7)
}

# overwrite modelsummary::modelsummary with custom version above
modelsummary <- modelsummary_AJPS

modelsummary::config_modelsummary(
  factory_default = "tinytable",
  factory_html = "tinytable",
  factory_latex = "tinytable",
  factory_markdown = "tinytable"
)


#FIXME Why are the tables not formatting well?
if(F){
  load(here::here("models", "total", "models.Rdata"))

  test <-modelsummary:: modelsummary(models)
  test
  class(test)

  test <- modelsummary(models)
  class(test)
  test
}
