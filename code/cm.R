library(stringr)

# Coef Map
cm = c(
  # $
  "PACamount_m" = "Corporate Campaign $ (Millions)",
  "PACamount_energy_m" = "Energy Corp. Campaign $ (Millions)",
  "PACamount_nonenergy_m" = "Non-energy Corp. Campaign $ (Millions)",
  # census (old)
  "pop2010m"= "State Population (Millions)",
  # census (new)
       "population_total" = "Population",
       "population_m" = "Population (Millions)",
  "workers_federal" = "Federal employed (Millions)" ,
  "workers_total" = "workers_total (Millions)" ,
  "workers_private_sector" = "Private sector (Millions)" ,
  "workers_self_employed" = "Self employed (Millions)" ,
  "workers_nonprofit" = "Nonprofit employed (Millions)" ,
  "workers_government" = "Government employed (Millions)" ,
  # "workers_nonprofit_2" = "workers_nonprofit_2 (Millions)" ,
  "education_bachelor_or_higher" = "College Grads (Millions)" ,
  "public_assistance_or_snap_households" = "Receiving public assistance (Millions)" ,
  # Percents from census
  "percent_workers_federal" = "% Federal Employees",  #FIXME this one is from a different section, not sure what to use for the denominator
       # workers section of census
  "percent_workers_nonprofit" = "% Nonprofit Sector" ,
  "percent_workers_self_employed"  = "% Self Employed",
  "percent_workers_private_sector"  = "% Private Sector",
  "percent_workers_government"  = "% Government Sector",
       # other sections
  "percent_education_bachelor_or_higher" = "% College Grad",
  "percent_public_assistance_or_snap_households" = "% Public Assistance",
  # STYLES FROM CROSSON AND KAVLOFSKI
  "styleDistrict Advocate" = "District Advocate",
  "styleParty Builder" = "Party Builder",
  "styleParty Soldier" = "Party Soldier",
  "stylePolicy Specialist" = "Policy Specialist",
  # ELECTORAL
       "competitive" = "Competitive General",
       "competitive_primary" = "Competitive Primary",
  # staff
  "pct_MRA_spending" = "% MRA Spent",
  "pct_MRA_legis_spending"   = "% MRA Legislative",
  "pct_MRA_pol_spending"   = "% MRA Political",
  "pct_MRA_comm_spending"  = "% MRA Comms.",
  "pct_MRA_off_spending"   = "% MRA Office",
  "pct_MRA_constit_spending" = "% MRA Constituent",
  "prop_legis_spending"  = "% Legislative Spending",
  "prop_pol_spending" = "% Political Spending",
  "prop_comm_spending" = "% Comms. Spending",
  "prop_off_spending" = "% Office Spending",
  "prop_constit_spending" = "% Const. Spending",
  "est_avg_office_size" = "Office Size",
  "est_legis_staff_size" = "Legislative Staff Size",
  "est_pol_staff_size"     = "Political Staff Size",
  "est_comm_staff_size" = "Comms. Staff Size",
  "est_off_staff_size" = "Office Staff Size",
  "est_constit_staff_size" = "Const. Staff Size",
  "est_total_spending" = "Total Spending",
  "est_total_legis_spending" = "Legislative Spending",
  "est_total_pol_spending" = "Political Spending",
  "est_total_comm_spending" = "Comms. Spending",
  "est_total_off_spending" = "Office Spending",
  "est_total_constit_spending" = "Const. Spending",
  "est_staff_spending" = "Total Staff Spending",
  "est_legis_spending" = "Legislative Spending",
  "est_pol_spending" = "Political Spending",
  "est_comm_spending" = "Comms. Spending",
  "est_off_spending" = "Office Spending",
  "est_constit_spending" = "Const. Spending",
  # VOTEVIEW
"majority" = "Majority",
"presidents_party" = "President's party",
       "abs_nominate_dim1" = "abs(NOMINATE)",
  "abs(nominate_dim1)" = "abs(NOMINATE)",
       "abs(agency_ideo)" = "Agency Percieved as Ideological",
       "abs_nominate_dim1:presidents_party" = "abs(NOMINATE) x President's party",
  "party(R)" = "Republican",
  "party(I)" = "Independent",
       # RCL + VOTEVIEW
       "alignmentTRUE" = "Aligned with Agency",
       "distance" = "Ideological Distance",
       "distance:presidents_party" = "Ideological Distance x President's party",
       "presidents_party:distance" = "Ideological Distance x President's party",
       # EXPERIENCE
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
  # institutional position
"oversight" = "Oversight Committee",
  "chair" = "Committee Chair",
  "ranking_minority" = "Ranking Member",
  "prestigeR2" = "Prestige Committee (R2)",
  "prestigeOLD" = "Prestige Committee (Old)",
  "prestige" = "Prestige Committee",
# CEL (LES)
"state_leg" = "Served in State Leg.",
"subchr" = "Subcommittee Chiar",
"lesclassic" = "LES (classic)",
"seniority" = "Seniority",
"freshman" = "Freshman",
"female"  = "Female",
"afam"= "African American" ,
"latino" = "Latino",
"votepct" = "Vote Percent",
#FE
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
  list("raw" = "FE: Legislator", "clean" = "Legislator fixed effects", "fmt" = f),
  list("raw" = "FE: year", "clean" = "Year fixed effects", "fmt" = f)
)

coef_omit = "(Intercept)|majority|presidents_party"
coef_omit = "none"


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

