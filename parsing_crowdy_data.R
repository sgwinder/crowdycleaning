####
#### Parsing Crowdy Parking Lot Counts
####

library(tidyverse)
library(googlesheets4)
library(lubridate)

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
  filter(!is.na(bodyclean)) 

# note that this recreates NAs, but deleting them first allows for a neat spread
crowdy_wide <- no_nas %>%
  select(bodyclean, lastquestion, from, to, date) %>%
  #rowid_to_column() %>%
  spread(key = lastquestion, value = bodyclean)

#### What about convos that go over multiple days?
# can I combine some of them into single convos?
crowdy_wide %>%
  group_by(from, to)
# I have 1,134 rows, and 1,045 unique pairs of to and from

# can I find sequential dates for those duplicate sharers?

# let's select only rows which have duplicate from/to pairs
multi_dates <- crowdy_wide %>%
  group_by(from, to) %>%
  summarise(n = n(),
            mindate = min(ymd(CntDate), na.rm = T), 
            maxdate = max(ymd(CntDate), na.rm = T),
            nadates = sum(is.na(CntDate))) %>%
  filter(n > 1)
  
# and, selecting out those rows
multi_nums <- multi_dates %>% 
  ungroup() %>%
  mutate(tofrom = str_c(to, from)) %>%
  select(tofrom)

multi_rows <- crowdy_wide %>%
  mutate(tofrom = str_c(to, from)) %>%
  filter(tofrom %in% multi_nums$tofrom) %>%
  arrange(from, to, date)

crowdy_wide %>%
  filter(is.na(CntDate))
# At least some of these are because a converstaion stretched over multiple days

#### Meh. Let's leave it for now - the to, from, date grouping should be good enough

# write out crowdy_wide
#write_csv(crowdy_wide, "convos_20191203.csv")

#### Parse and clean counts
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

#### Pull out data on party people, party vehicles, and how long for parking model
trips <- crowdy_wide %>%
  select(from, to, date, CntDate, HowLong, PartyPeople, PartyVehics) %>%
  filter(!(is.na(HowLong) & is.na(PartyPeople) & is.na(PartyVehics))) 

# bind on to trails & remove phone numbers
trips_tr <- trips %>%
  left_join(th_joinkey, by = c(to = "cell")) %>%
  select(-from, -to)

# write it out
#write_csv(trips_tr, "Trips_info_crowdy20191203.csv")
