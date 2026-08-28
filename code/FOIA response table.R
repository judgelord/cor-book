here::here("data_list.R") |> str_replace("cor-book", "correspondence_data") |> source()
data_list

data_list %<>%
  filter(agency != "DHS_USCIS_2016") %>%
  mutate(agency =
           str_remove(agency, "_HQ") |>
           str_replace("DHHS", "HHS")|>
           str_replace("DOD_Navy", "DOD_NAVY"))

data_list %>% filter(agency == "DHHS_FDA")
data_list %>% filter(agency == "HHS_FDA")


library(googlesheets4)
library(googledrive)

# get FOIAed agency totals from FOIA List on drive
foiaList_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1NOJ-fw_HDA4iObsm-LpTgfIMBd4wjSTLiL3EmfKcjH8/edit?gid=0#gid=0")


foiaList <- foiaList_raw %<>%
  select(Department, agency_acronym, department_agency_acronym, Bureau, sample, data, on_drive) %>% filter(!is.na(sample)) %>%
  distinct() %>%
  mutate(agency = str_remove(department_agency_acronym, "_$"))

foiaList %>% filter(agency == "DHHS_FDA")
foiaList %>% filter(agency == "HHS_FDA")

d <- data_list |> full_join(foiaList)

# inspect for failed matches
d |> filter(is.na(Department))

d %<>% mutate(department = str_remove(agency, "_.*"))

df <- d

# corrections to foia list
df %<>% mutate(Department = ifelse(department == "DHS", "Department of Homeland Security", Department))
df %<>% mutate(Department = ifelse(department == "DOC", "Department of Commerce", Department))
df %<>% mutate(Department = ifelse(department == "DOD", "Department of Defense", Department))
df %<>% mutate(Department = ifelse(department == "DOT", "Department of Transportation", Department))
df %<>% mutate(Department = ifelse(department == "DOI", "Department of the Interior", Department))
df %<>% mutate(Department = ifelse(department == "DHHS", "Department of Health and Human Services", Department))
df %<>% mutate(Department = ifelse(department == "EOP", "Executive Office of the President", Department))
df %<>% mutate(Department = ifelse(department == "USDA", "Department of Agriculture", Department))
df %<>% mutate(Department = ifelse(department == "HUD", "Department of Housing and Urban Development", Department))
df %<>% mutate(Department = ifelse(department == "CSOSA", "Court Services and Offender Supervision Agency", Department))
df %<>% mutate(Department = ifelse(department == "NWTRB", "Nuclear Waste Technical Review Board | NWTRB", Department))


df %>% select(agency, department, Department) %>% distinct() %>% filter(is.na(Department))

df %>% select(agency, department, Department) %>% distinct()

data <- df

data$data <-gsub("yes.*", 1, data$data)
data$data <-gsub("no.*", 0, data$data)
data %<>% mutate(data = ifelse(!is.na(status) | on_drive == 1, 1, data))
data$data1 <- as.numeric(data$data)

data$on_drive <-gsub("yes.*", 1, data$on_drive)
data$on_drive <-gsub("no.*", 0, data$on_drive)
data %<>% mutate(on_drive = ifelse(!is.na(status), 1, on_drive))

data$on_drive %<>% as.numeric()
data$on_drive


data %<>% mutate(Bureau = ifelse(is.na(Bureau), Department, Bureau))

data %<>%
  mutate(Department = ifelse(!grepl("Department of", Department), "Independent Agencies", Department))

data %<>% group_by(Department) %>%
  mutate(Components = n(), Records = sum(data1), Coded = sum(on_drive)) %>%
  distinct()

data %<>% distinct(agency, Department, Components, Records, Coded)


data

# add total observations
here("data" ,"all_contacts.rda") |> str_replace("cor-book", "correspondence_data") |> load()


obs <- all_contacts |>
  mutate(agency =
           str_remove(agency, "_HQ|_2016") |>
           str_replace("DHHS", "HHS")|>
           str_replace("DOD_Navy", "DOD_NAVY")) |>
  left_join(data) # |>  filter(is.na(Department))

nrow(obs)


obs %<>% group_by(Department) %>% summarise(Observations =n()) %>% distinct()
#


data %<>% full_join(obs) %>%
  mutate(Observations  = replace_na(Observations, 0)) %>%
  select(-agency) %>%
  ungroup() %>%
  distinct()

total <- data |> select(-Department) |> summarise_all(sum) |> mutate(Department = "Total")

# totals
data %<>% ungroup() %>%
  bind_rows(total)

data %<>%
  rename(`Components FOIAed` = Components,
         `Records received` = Records)

write.csv(data, file = "data/_FOIA_response_table.csv")


data %>% kable()

n <- tail(data$Observations, 1) %>% kable()

write(n, file = "data/n")



