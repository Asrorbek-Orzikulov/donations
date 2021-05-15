library(tidyverse)
library(readxl)
library(lubridate)
rm(list = ls())
setwd("/Users/asrorbek/Desktop/Master 1/Semester 3/Hackathon")
contacts <- read_excel("Contacts.xlsx")
names(contacts)

# dropping the first column
contacts <- contacts %>%
  select(-1)
view(head(contacts, 50))

# parsing date columns
date_cols <- contacts %>%
  select(contains("date")) %>%
  names()

contacts[date_cols] <- map_dfc(contacts[date_cols], ymd)

# uncovering data errors
contacts %>%
  filter(!is.na(BIRTH_DATE),
         CREATION_DATE < BIRTH_DATE) %>%
  view()

# people who donated too late
contacts %>%
  filter(!is.na(BIRTH_DATE),
         CREATION_DATE > BIRTH_DATE) %>%
  mutate(donation_age = year(FIRST_DONATION_DATE) - year(BIRTH_DATE)) %>% 
  filter(donation_age > 100) %>%
  view()

# people who donated too early
contacts %>%
  filter(!is.na(BIRTH_DATE),
         CREATION_DATE > BIRTH_DATE) %>%
  mutate(donation_age = year(FIRST_DONATION_DATE) - year(BIRTH_DATE)) %>% 
  filter(donation_age < 10) %>%
  select(1, contains("date"), donation_age) %>%
  view()

# number of duplicated rows
sum(duplicated(contacts$ID_CTC))

# when donors start donating
contacts %>%
  filter(!is.na(BIRTH_DATE),
         CREATION_DATE > BIRTH_DATE) %>%
  mutate(donation_age = year(FIRST_DONATION_DATE) - year(BIRTH_DATE)) %>%
  ggplot(aes(donation_age)) +
  geom_histogram() +
  xlim(0, 100)

# row binding the donors
tables_to_read <- c("Dons_2014.xlsx", "Dons_2015.xlsx", "Dons_2016.xlsx",
                    "Dons_2017.xlsx", "Dons_2018.xlsx")
donors <- tibble()
for (table_name in tables_to_read) {
  table <- read_excel(table_name)
  print(nrow(table))
  donors <- bind_rows(donors, table)
}

print(107829 + 110170 + 150222 + 144844 + 152435 == nrow(donors))

view(head(donors, 20))
view(tail(donors, 50))

# dropping NAs
donors <- donors %>% 
  mutate(na_count = rowSums(is.na(.))) %>%
  filter(na_count == 0) %>%
  select(-na_count)

# adding the year column
donors <- donors %>%
  mutate(year_donation = year(`Receipt date`))

# last date of year and recency
donors <- donors %>%
  group_by(year_donation) %>%
  mutate(last_date = max(`Receipt date`),
         recency = difftime(last_date, `Receipt date`, units = "days"),
         recency = as.numeric(recency))

# some analysis
donors %>%
  filter(year_donation == 2018,
         `Amount of transaction` != `ventilation amount`) %>% 
  head(20) %>% 
  view()

donors %>% group_keys()

# maximum count of `Movement Id``
donors %>%
  ungroup() %>%
  group_by(`Movement Id`) %>%
  tally(sort = TRUE) %>%
  head()

# frequency column
donors <- donors %>%
  ungroup() %>%
  group_by(`contact ID`, year_donation) %>%
  add_tally() %>%
  rename(freq = n) %>%
  ungroup()

# monetary_value column
donors <- donors %>% 
  group_by(`contact ID`, year_donation) %>%
  mutate(monetary_value = sum(`ventilation amount`)) %>%
  ungroup()

# keeping the most recent donation
donors_csv <- donors %>% 
  group_by(`contact ID`, year_donation) %>%
  filter(recency == min(recency)) %>%
  select(`contact ID`, `cam code`, `oft code`, `payment method`,
         year_donation, recency:monetary_value)

# quick check of donors_csv
nrow(donors_csv) == 115108
view(head(donors_csv, 30))
view(tail(donors_csv, 30))

sum(donors_csv$recency < 0)
sum(donors_csv$freq < 0)
sum(donors_csv$monetary_value < 0)

donors %>%
  filter(`contact ID` == 759694,
         year_donation == 2018) %>%
  view()

# exporting the data
write_csv(donors_csv, "donors_new.csv")

# plotting
hist(donors_csv$recency)
hist(donors_csv$freq)
hist(donors_csv$monetary_value)



