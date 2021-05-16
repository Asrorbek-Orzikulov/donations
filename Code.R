library(tidyverse)
library(readxl)
library(lubridate)
library(e1071)

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
rm(table)

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
donors %>%
  filter(`ventilation amount` == 0) %>%
  head(30) %>%
  view()

donors <- donors %>%
  ungroup() %>%
  filter(`ventilation amount` > 0) %>%
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
  group_by(year_donation, `contact ID`) %>%
  filter(recency == min(recency)) %>%
  slice_max(order_by = `ventilation amount`, with_ties = FALSE) %>%
  select(`contact ID`, `cam code`, `oft code`, `payment method`, RF,
         year_donation, recency:monetary_value)

# quick check of donors_csv
nrow(donors_csv) == 111105
view(head(donors_csv, 30))
view(tail(donors_csv, 30))

sum(donors_csv$recency < 0)
sum(donors_csv$freq < 0)
sum(donors_csv$monetary_value < 0)

donors %>%
  filter(`contact ID` == 759694,
         year_donation == 2018) %>%
  view()

# should be 10458303
donors %>% ungroup() %>% summarise(sum = sum(`ventilation amount`))
donors_csv %>% ungroup() %>% summarise(sum = sum(monetary_value))
sum(donors[["ventilation amount"]], na.rm = TRUE) == sum(donors_csv$monetary_value)

# plotting and transformation: recency
hist(donors_csv$recency)
skewness(donors_csv$recency)
hist(donors_csv$recency^(1/3))  # best one
skewness(donors_csv$recency^(1/3))
hist(log(donors_csv$recency + 1))
skewness(log(donors_csv$recency + 1))

# plotting and transformation: freq
hist(donors_csv$freq)
skewness(donors_csv$freq)
hist(sqrt(donors_csv$freq))
skewness(sqrt(donors_csv$freq))
hist(log(donors_csv$freq))
skewness(log(donors_csv$freq))  # best one

# plotting and transformation: monetary_value
hist(donors_csv$monetary_value)
skewness(donors_csv$monetary_value)
hist(sqrt(donors_csv$monetary_value))
skewness(sqrt(donors_csv$monetary_value))
hist(log(donors_csv$monetary_value))
skewness(log(donors_csv$monetary_value)) # best one

# scaling and transforming variables
donors_csv <- donors_csv %>%
  group_by(year_donation) %>% 
  mutate(
    recency = recency^(1/3),
    recency = (recency - mean(recency)) / sd(recency),
    freq = sqrt(freq),
    freq = (freq - mean(freq)) / sd(freq),
    monetary_value = log(monetary_value),
    monetary_value = (monetary_value - mean(monetary_value)) / sd(monetary_value)
         )

donors_csv <- donors_csv %>%
  mutate(
    rf_new = case_when(
      RF == "Oui" | RF == "yes" ~ 1,
      RF == "Non" | RF == "no" ~ 0
    )
  ) %>%
  ungroup()

# exporting the data
write_csv(donors, "donors_united.csv")
write_csv(donors_csv, "donors_new.csv")

names(donors_csv)
donors_csv %>%
  select(`contact ID`:`payment method`, rf_new) %>%
  write_csv("descriptors.csv")

donors_csv %>%
  select(`contact ID`, recency:monetary_value) %>%
  write_csv("segmentation.csv")

# plotting donors
donors_long <- read_csv("plotting.csv")
view(head(donors_long))
donors_long %>%
  ggplot(aes(x = Attribute, y = Value, group = cluster)) +
  geom_line()
