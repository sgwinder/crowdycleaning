####
#### Parsing Crowdy Parking Lot Counts
####

library(tidyverse)
library(googlesheets4)

# Starting with the most recent pull
# Crowdy20191203_Data_Cleaned
crowdyid <- "1Nw79vxKjTTDp-0ots3wwZoq4NV5Y2wx4cNZk_9vW7KE"
crowdy_data <- read_sheet(crowdyid, col_types = "c", na = c("", "NA")) # slow

# Also pull in a joinkey to get phone numbers to trails
joinid <- "163uVp6DpL-QerVqftJpYs5CJDsZZjbv_N0-xDt-Tntc"
th_info <- read_sheet(joinid, sheet = "Crowdsource Parking Lot Counts TH")

# What I need is a "convo" id. But figuring out how to do that is tricky...
# I suppose a relatively good method is to group by "to", "from", and "date"
# Note that this led me to identify a bunch of issues that I fixed row by row in the googlesheet
## When a question was asked twice, I assigned the second value to be an NA
# spread data 

no_nas <- crowdy_data %>%
  filter(!is.na(bodyclean)) #

crowdy_wide <- no_nas %>%
  select(bodyclean, lastquestion, from, to, date) %>%
  #rowid_to_column() %>%
  spread(key = lastquestion, value = bodyclean)
crowdy_wide %>%
  filter(is.na(CntTime))
# At least some of these are because a converstaion stretched over multiple days

crowdy_cnts <- crowdy_wide %>%
  select(from, to, date, starts_with("Cnt"), starts_with("Vehic"))
crowdy_cnts

# Let's separate out first, second and third counts
first <- crowdy_cnts %>%
  select(to, CntDate, CntTime, VehicCount) %>%
  filter(!is.na(VehicCount))
first

second <- crowdy_cnts %>%
  filter(!is.na(VehicCount2)) %>%
  mutate(CntDate = if_else(!is.na(CntDate2), CntDate2, CntDate),
         CntTime = CntTime2,
         VehicCount = VehicCount2) %>%
  select(to, CntDate, CntTime, VehicCount)

third <- crowdy_cnts %>%
  filter(!is.na(VehicCount3)) %>%
  mutate(#CntDate = if_else(!is.na(CntDate3), CntDate3, CntDate),
         CntTime = CntTime3,
         VehicCount = VehicCount3) %>%
  select(to, CntDate, CntTime, VehicCount)

# bind em together
counts <- bind_rows(first, second, third)

## Now use the joinkey to convert phone numbers to trails
th_joinkey <- th_info %>%
  select(TrlNum = Number, cell = `Cell number`) %>%
  mutate(cell = paste0("+1", str_replace_all(cell, "-", "")))

counts_tr <- counts %>%
  left_join(th_joinkey, by = c("to" = "cell")) %>%
  select(TrlNum, everything(), -to) %>%
  mutate(source = "SMS Volunteer") %>%
  arrange(CntDate)

counts_tr
# write it out
#write_csv(counts_tr, "~/Documents/Crowdy/Parking_Counts_Crowdy20191203.csv")
