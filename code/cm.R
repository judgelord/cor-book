# Coef Map
cm = c("pop2010m"= "State Population (Millions)",
       "competitive" = "Competitive General",
       "competitive_primary" = "Competitive Primary",
       "chair" = "Committee Chair",
       "ranking_minority" = "Ranking Member",
       "prestigeR2" = "Prestige Committee (R2)",
       "prestigeOLD" = "Prestige Committee (Old)",
       "prestige" = "Prestige Committee",
       "oversight" = "Oversight Committee",
       "lesclassic" = "LES (classic)",
       "alignmentTRUE" = "Aligned with Agency",
       "distance" = "Ideological Distance",
       "distance:presidents_party" = "Ideological Distance x President's party",
       "new_memberTRUE" = "New Member",
       "new_member" = "New Member",
       "new_senator" = " New Senator in Delegation",
       "new_one" = " New Member in Delegation",
       "new_proportion" = " New Proportion in Delegation",
       "new_member:same_party" = " New Member x Same Party",
       "first_two" = "First Two Years",
       "experience" = "Three+ Years in Office",
       "first" = "First Year",
       "second" = "Second Year",
       "third" = "Third Year",
       "fourth" = "Fourth Year",
       "fifth" = "Fifth Year",
       "sixth" = "Sixth Year",
       "new_memberTRUE:same_party" = "New Member x Same Party",
       "same_party:second" = "Second Year x Same Party",
       "same_party:third" = "Third Year x Same Party",
       "same_party:fourth" = "Fourth Year x Same Party",
       "same_party:fifth" = "Fifth Year x Same Party",
       "same_party:sixth" = "Sixth Year x Same Party",
       "same_party" = "Same Party",
       "majority" = "Majority",
       "presidents_party" = "President's party",
       "Legislator" = "Legislator",
       "Agency" = "Agency",
       "Num.Obs." = "Observations"
)

# FORMATTING FOR AJPS
cmAJPS <- cm |> str_to_sentence()
names(cmAJPS) <- names(cm)
cm <- cmAJPS
# END FORMATTING FOR AJPS

# set fixed effects mapping
setFixest_dict(cm)

format_n <- function(x) format(round(x, 3), big.mark=",") # this works
f <- function(x) stringr::str_replace(x, "[A-z]", "✓") #FIXME not sure why this is not working

gm <- list(
  list("raw" = "nobs", "clean" = "Observations", "fmt" = format_n),
  list("raw" = "FE: agency_year", "clean" = "Year x agency fixed effects", "fmt" = f),
  list("raw" = "FE: Year_x_Agency", "clean" = "Year x agency fixed effects", "fmt" = f),
  list("raw" = "FE: Legislator_x_Agency", "clean" = "Legislator x agency fixed effects", "fmt" = f),
  list("raw" = "FE: icpsr_agency", "clean" = "Legislator x agency fixed effects", "fmt" = f),
  list("raw" = "FE: icpsr_agency", "clean" = "Legislator-agency fixed effects", "fmt" = f),
  list("raw" = "FE: District", "clean" = "District fixed effects", "fmt" = f),
  list("raw" = "FE: Year", "clean" = "Year fixed effects", "fmt" = f),
  list("raw" = "FE: Legislator.*x.*Agency", "clean" = "Legislator x agency fixed effects", "fmt" = f),
  list("raw" = "FE: Year.*x.*Agency", "clean" = "Year x agency fixed effects", "fmt" = f),
  list("raw" = "FE: Legislator", "clean" = "Legislator fixed effects", "fmt" = f)
)

coef_omit = "(Intercept)|majority|presidents_party"
coef_omit = "none"

setFixest_dict(cm)

gof_omit = "R.*|AIC|BIC|Log.*|Std.*"

# table formatting to match stata (sort of)
format_table <- . %>%
  # OLD modelsummary based on kableExtra package (broken): https://stackoverflow.com/questions/78422275/argument-is-of-length-zero-from-kableextra-after-updating-rstudio
  #kableExtra::row_spec(row = 1, bold = T, hline_after = TRUE) %>%
  #kableExtra::kable_styling(font_size = 11) %>%
  #full_width = TRUE,
  #latex_options = c("repeat_header")) %>%
  # NEW modelsummary based on tinytable package: https://vincentarelbundock.github.io/tinytable/vignettes/format.html
  as.character() %>%
  str_replace("Num.Obs.", "Observations") %>%
  str_replace("Std.Errors", "\footnotesize Robust/Clustered Std. Errors") %>%
  str_replace("FE: Legislator.*x.*Agency", "Legislator x Agency FE") %>%
  str_replace("FE: Year.*x.*Agency", "Year x Agency FE") %>%
  str_replace("FE: Year", "Year Fixed Effects") %>%
  str_replace("FE: Legislator", "Legislator Fixed Effects") %>%
  str_replace_all("X|✓", "\\\\\\checkmark") %>%
  # a random midrule appeared in the wrong place
  #str_remove("\\midrule") %>%
  #  extract just the table, no caption etc
  str_remove_all("\\\\begin\\{table\\}|\\\\centering|\\\\end\\{table\\}")

