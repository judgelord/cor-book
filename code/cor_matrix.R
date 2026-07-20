# THE FIRST PART HERE IS COPPIED FROM replication.qmd to get the data ready.
if(F){
  type = 1
# load r packages and qmd render defaults
here::here("code", "qmd_global_options.R") |> source()
testing = F

# note these dcounts already have member_data merged in
load(here::here("data", type, "dcounts.rds"))

data_member <- dcounts

rename_vars <- function(data){
  data <- data |>
    mutate(same_party = as.numeric(same_party),
           # old census variable
           pop2010m = pop2010/1000000,
           # from new census data
           population_m = population_total/1000000,
           # note I am transforming these without making "_m" versions
           workers_federal=workers_federal/1000000 ,
           workers_private_sector=workers_private_sector/1000000 ,
           workers_self_employed=workers_self_employed/1000000 ,
           workers_nonprofit=workers_nonprofit/1000000 ,
           workers_government=workers_government/1000000 ,
           education_bachelor_or_higher=education_bachelor_or_higher/1000000,
           public_assistance_or_snap_households=public_assistance_or_snap_households/1000000,
           # $
           PACamount_nonenergy_m = PACamount_m - PACamount_energy_m,
           Legislator = icpsr,
           District = state_dist,
           Year = year,
           first_two = first, second,
           experience = ifelse(first_two == 1, 0, 1),
           distance = abs(nominate_dim1 - agency_ideo),
           # staff
           # fixed effects
           icpsr_agency = paste(agency, icpsr, sep='_'),
           agency_year = paste(agency, year, sep='_'),
           Year_x_Agency = agency_year,
           Legislator_x_Agency = icpsr_agency)

  return(data)

}
data_member <- rename_vars(data_member)


}


# THIS PART IS RUN ON SOURCE
data_member <- data_member %>%
  group_by(icpsr, chamber, congress) %>%
  mutate(perYear_sum = sum(perYear) ) %>%
  ungroup() %>%
  mutate(
    perYear_decile = ntile(perYear_sum, 10),
    decile_1  = if_else(perYear_decile == 1, 1, 0),
    decile_2  = if_else(perYear_decile == 2, 1, 0),
    decile_3  = if_else(perYear_decile == 3, 1, 0),
    decile_4  = if_else(perYear_decile == 4, 1, 0),
    decile_5  = if_else(perYear_decile == 5, 1, 0),
    decile_6  = if_else(perYear_decile == 6, 1, 0),
    decile_7  = if_else(perYear_decile == 7, 1, 0),
    decile_8  = if_else(perYear_decile == 8, 1, 0),
    decile_9  = if_else(perYear_decile == 9, 1, 0),
    decile_10 = if_else(perYear_decile == 10, 1, 0)
  )

data_member

vars <- c(
"population_m",
  "chamber",
  #"workers_self_employed",
  # "workers_private_sector",
  # "workers_nonprofit",
  "percent_workers_government",
  "percent_education_bachelor_or_higher",
  "percent_public_assistance_or_snap_households",
   "percent_workers_self_employed",
   #"percent_workers_private_sector",
   "percent_workers_nonprofit",
  # electoral
  "competitive",
  "PACamount_m",
  # institution
  "chair",
  "subchr",
  "ranking_minority",
  "prestige",
  # member level
  "state_leg",
  "experience",
"oversight",
"distance",
  "abs_nominate_dim1",
"republican",
#"nominate_dim1",
  "presidents_party",
  "majority",
"senate",
"lesclassic",
"decile_1",
"decile_2",
"decile_3",
"decile_4",
"decile_5",
"decile_6",
"decile_7",
"decile_8",
"decile_9",
"decile_10"
)

# Correlation matrix for all numeric variables in a tibble
cor_matrix <- data_member %>%
  mutate(abs_nominate_dim1 = abs(nominate_dim1),
         republican = as.numeric(party == "(R)" ),
         senate = as.numeric(chamber == "Senate" )) %>%
  select(where(is.numeric)) %>%
  select(any_of(vars)) %>%
  cor(use = "pairwise.complete.obs")



#   library(corrplot)
# plot.new()
# corrplot(cor_matrix,
#          method = "color",
#          diag = F,
#          pch.cex = 1.3,
#          tl.col="black", #tl.cex=1.3,
#          tl.srt = 45,
#          pch.col="red",
#          sig.level = c(.05),
#          insig = "label_sig",
#          type = "upper",
#          tl.offset=0.2,
#          cl.pos="r",
#          cl.cex = 1.3)


 # GGPLOT

 #install.packages("reshape2")
 library(reshape2)

 cor_matrix[lower.tri(cor_matrix)] <- NA


  melted <- melt(cor_matrix, na.rm = T )



 # A Full Correlation Plot Using ggplot2

 ## Version One: Correlation Plot using ggplot2:
  p1 <- melted |>
   filter(Var1!=Var2,
          !str_detect(Var1, "decile")) |>
 ggplot(aes(x = Var1, y = Var2, fill = value)) +
   geom_tile(color = "black") +
   scale_fill_gradient2(midpoint = 0, mid ="white") +
   labs(#title = "Correlation Matrix",
        x = "",
        y = "",
        fill = "Correlation") +
   theme(axis.text.x = element_text(angle = 90, hjust=0, vjust = .02) ,
         panel.grid = element_blank(),
         legend.position =  c(.8,.3))  +
   scale_x_discrete(position = "top")
p1

 ## decile only Correlation Plot using ggplot2:
 p2 <- melted |>
   filter(Var1!=Var2,
          !str_detect(Var1, "decile"),
          str_detect(Var2, "decile")) |>
   ggplot(aes(x = Var1, y = Var2, fill = value)) +
   geom_tile() +
   scale_fill_gradient2(midpoint = 0, mid ="white") +
   labs(#title = "Correlation Matrix",
     x = "",
     y = "",
     fill = "Correlation") +
   theme(axis.text.x = element_text(angle = 90, hjust=0, vjust = .02) ,
         panel.grid = element_blank() )  +
   scale_x_discrete(position = "top")
p2
