####
#### Munging Clean Crowdy Data
#### March, 2020
#### Use this script to convert manually cleaned dumb crowdy data into convos, parking lot counts, 
####   and trip data (how long, how many people/party and /veh) 
#### 
#### Inputs: CrowdyDDDD_Data_Cleaned from google drive. These are long form spreadsheets of data 
####           which have been parsed using `parse_sms_surveys.r`, then manually cleaned by a tech,
####           following the protocols in "Crowdy Cleaning Instructions and Formatting" on the team drive
#### Outputs:
####    Convos_deploystamp.csv - a wide form dataset of conversations (assuming that all convos happen on a single day)
####    Parking_counts_deploystamp.csv - a long form dataset of parking lot counts only
####    Trips_info_deploystamp.csv - Info on trip length, number of people in party, and number of vehicles in party

library(tidyverse)
library(googlesheets4)
library(lubridate)

setwd("~/Documents/Crowdy/")
gdriveuser <- "sgwinder@uw.edu"
sheets_auth(email = gdriveuser)

# change id and deploystamp depending on which set of data you are working from

# Starting with the most recent pull
# Crowdy20191203_Data_Cleaned
crowdyid <- "1Nw79vxKjTTDp-0ots3wwZoq4NV5Y2wx4cNZk_9vW7KE"
deploystamp <- "Crowdy20191203"

# deployed20180928
#crowdyid <- "1UeJjCead3qXnKQecQVoJ2vrP0kf0r6zoIph4LTDY5RU"
#deploystamp <- "20180928"

# deployed20180612
#crowdyid <- "1lChv7Tdgi5nBXIXiIlblK7dNF1SGxRsz9qdtFVzQSN4"
#deploystamp <- "20180612"

crowdy_data <- read_sheet(crowdyid, col_types = "c", na = c("", "NA")) # slow

# Also pull in a joinkey to get phone numbers to trails
joinid <- "163uVp6DpL-QerVqftJpYs5CJDsZZjbv_N0-xDt-Tntc"
th_info <- read_sheet(joinid, sheet = "Crowdsource Parking Lot Counts TH")

# What I need is a "convo" id. But figuring out how to do that is tricky...
# I suppose a relatively good method is to group by "to", "from", and "date"
# Note that this led me to identify a bunch of issues (by errors that keys were shared)
# that I fixed row by row in the googlesheet.
## When a question was asked twice, I assigned the second value to be an NA
# spread data 

no_nas <- crowdy_data %>%
  filter(!is.na(bodyclean)) 

# note that this recreates NAs, but deleting them first allows for a neat spread
crowdy_wide <- no_nas %>%
  select(bodyclean, lastquestion, from, to, date) %>%
  #rowid_to_column() %>%
  spread(key = lastquestion, value = bodyclean)
crowdy_wide
## TODO: Figure out how to combine rows for convos that went on for more than one day.
##  There are some (unsuccessful) attempts at this at the end of the script 

# write out crowdy_wide
#write_csv(crowdy_wide, paste0("data/Convos_", deploystamp, ".csv"))

#### Parse and clean counts
crowdy_cnts <- crowdy_wide %>%
  select(from, to, date, starts_with("Cnt"), starts_with("Vehic"))
crowdy_cnts

# Let's separate out first, second and third counts
first <- crowdy_cnts %>%
  select(to, CntDate, CntTime, VehicCount) %>%
  filter(!is.na(VehicCount))
first

# note that CntDate is commented out for the 20180928 Deployment, which had no CntDate2
second <- crowdy_cnts %>%
  filter(!is.na(VehicCount2)) %>%
  mutate(CntDate = if_else(!is.na(CntDate2), CntDate2, CntDate),
         CntTime = CntTime2,
         VehicCount = VehicCount2) %>%
  select(to, CntDate, CntTime, VehicCount)

# no third counts for 20180928 or 20180612 deployments
third <- crowdy_cnts %>%
  filter(!is.na(VehicCount3)) %>%
  mutate(CntDate = if_else(!is.na(CntDate3), CntDate3, CntDate),
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
#write_csv(counts_tr, paste0("data/Parking_Counts_", deploystamp, ".csv"))

#### Pull out data on party people, party vehicles, and how long for parking model
# (note that there is no "HowLong" question in the 20180612 deployment)
trips <- crowdy_wide %>%
  select(from, to, date, CntDate, HowLong, 
         PartyPeople, PartyVehics) %>%
  filter(!(is.na(HowLong) & is.na(PartyPeople) & is.na(PartyVehics))) 

# bind on to trails & remove phone numbers
trips_tr <- trips %>%
  left_join(th_joinkey, by = c(to = "cell")) %>%
  select(-from, -to)
trips_tr

# write it out
#write_csv(trips_tr, paste0("data/Trips_info_", deploystamp, ".csv"))


############ Old ###########
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