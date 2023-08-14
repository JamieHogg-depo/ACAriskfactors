#==========================================================================
# Format data for Atlas database
#
# Step 1:
#        Set the directories
#        Load concordance etc
#
# Step 2:
#        Set up the meta data
#
# Step 3:
#        Load data WITHOUT any exclusions (eg you may have excluded people with no area)
#        Load ERP data
#
# Step 4:
#        Aggregate counts by age and year
#        For screening, the current plan is to present percent of eligible pop who have participated in the last 2 years
#        For screening (and I believe risk factors) the percents won't be age-standardised
#        I've included code for age-standardisation (commented out) for your reference
#        For screening, the current plan is to present annual counts
#
# Step 5:
#        Load Tango's MEET
#        Currently, the code allows three levels of evidence from Tango's MEET
#        However, Viser's plans only allow a binary indicator for the Tango's MEET result
#        So we may edit this in the future
#
# Step 6:
#        Save as csv
#        
# Authors: Jess Cameron
# Created: 08/08/23
# Updated: 
#          
#==========================================================================
library(dplyr)
library(lubridate)
# library(openxlsx) # Currently need to save as xlsx to retain leading "0"s in codes

# File path for data
fp.dat <- "G:/Screening/BreastScreen/Data/BREAST_SCREEN_1920.csv"

# File path for population data
fp.pop <- "G:/Other Data 2016/aust_pop_7121.dta"

# File path for Tango's MEET results
fp.meet <- "G:/Screening/BreastScreen/Results/2016 ASGS/Tango/Tango_BreastScreen.csv"

# File path for saving the data
fp.out <- "G:/Screening/BreastScreen/Results/2016 ASGS/Aus_summary_BreastScreen.csv"

# Construct a data set with all the meta data for the different measures
# You may construct this using "expand.grid" and then
# filter out impossible combinations (eg female prostate cancer) and
# merge in the codes to match the strings
Ref <- data.frame(model_string = "Spatial",
                  model_code = 3,
                  measure_string = c("Relative", "Absolute", "Absolute"),
                  measure_code = c(1,2,2),
                  measure_level_string = c("Relative ratios", "Modelled number of participants", "Modelled number of non-participants"),
                  measure_level_code = c("01", "04", "06"),
                  indicator_string = "Screening",
                  indicator_code = "04",
                  sub_indicator_string = "Breast cancer",
                  sub_indicator_code = "033",
                  sub_sub_indicator_string = NA,
                  sub_sub_indicator_code = NA,
                  sex_string = "Females",
                  sex_code = 2,
                  yeargrp = "2019-2020")

# Load uncleaned URF data
dat <- read.csv(fp.dat)

# Remove duplicate participant IDs after sorting by attendance date
dat_by_age_year <- 
  dat %>%
  subset(age_screen %in% 50:74) %>% # Include only the eligible population
  mutate(date_attendance = dmy(date_attendance)) %>% # Format date
  arrange(s_id, desc(date_attendance)) %>% 
  mutate(session = 1) %>% # Define session so that we can identify how many times an individual has been screened
  group_by(s_id) %>% # s_id is participant ID number
  mutate(session = cumsum(session)) %>% # Number the sessions for each individual
  ungroup() %>% # head()
  subset(session == 1) %>% # Keep only the first screen for each individual
  mutate(year = year(date_attendance), # Obtain year the screen took place
         agegrp = round((age_screen - 47)/5), # Reformat age group
         agegrp = factor(agegrp,
                         labels = paste(seq(50,70,5), seq(54, 74, 5), sep = "-"))
  ) %>% 
  group_by(year, agegrp) %>%
  summarise(n = sum(session)) %>% # Count the number of screens per age group and year
  ungroup()


# Load estimated resident population data
ERP <- 
  readstata13::read.dta13(fp.pop) %>% # head()
  subset(state == 9 & year %in% 2019:2020 & agegrp %in% 15:19 & sex == 2) %>% # Keep Australia-wide data, for relevant years and age group
  mutate(agegrp = factor(agegrp - 14,
                         labels = paste(seq(50,70,5), seq(54, 74, 5), sep = "-")) # Reformat age group
  )

# There different numbers of rows by categorical variables
table(ERP$year)
table(ERP$sex)
table(ERP$agegrp)

# Check the numbers make sense
ERP %>%
  group_by(year) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

# Population seems to make sense
# A tibble: 2 x 2
# year     pop
# <dbl>   <dbl>
#   1  2019 3466666
# 2  2020 3560078

table(ERP$agegrp)

# Obtain ERP by age group and year
ERP_by_age_year <-
ERP %>%
  group_by(year, agegrp) %>%
  summarise(pop = sum(pop)) %>%
  ungroup()

# Join screening numbers with ERP data
dat_ERP_by_age_year <-
  full_join(dat_by_age_year,
            ERP_by_age_year,
            by = c("year", "agegrp"))

# # For age-standardised rates
# # Obtain standard age distribution (Australian standard 2001)
# stdage <- 
#   readstata13::read.dta13("G:/Other Data 2016/ast01pop.dta") %>%
#   subset(agegrp %in% 15:19) %>% # Keep only the eligible age groups
#   mutate(agegrp = factor(agegrp - 14,
#                          labels = paste(seq(50,70,5), seq(54, 74, 5), sep = "-"))
#   )
# 
# # Becuse we've removed some age groups, we standardise the weights to sum to 1
# stdage$std <- stdage$ast01wgt / sum(stdage$ast01wgt)
# 
# # Confirm it sums to 1
# sum(stdage$std)
# 
# 
# # Join standard population distribution to data
# dat_ERP_by_age_year <-
#   left_join(dat_ERP_by_age_year,
#             subset(stdage, select = c(agegrp, std)),
#             by = "agegrp")
# 
# # Does it look correct
# dat_ERP_by_age_year
# 
# # Do the annual data look OK?
# dat_ERP_by_age_year %>%
#   mutate(rate = n * std / pop) %>%
#   group_by(year) %>%
#   summarise(rate = sum(rate))

# Sum the annual rates / counts to obtain the 2-year estimates
# because the recommended screening interval is 2 years
asr <-
dat_ERP_by_age_year %>%
  mutate(rate = n / pop / 5) %>% # Average over the 5 age groups # For Age-standardised rates:   mutate(rate = n * std / pop) %>%
  summarise(rate = sum(rate),
            n = sum(n), # Number of participants who have had at least one screen/test in a defined period
            pop = sum(pop) / 2) # Average eligible population over the two-year period

# Participation rate (%)
Ref$asr = rep(round(100 * asr$rate, 1), nrow(Ref))
# Rate of non-participation (%) - just in case
Ref$asr[grepl("non-participant", Ref$measure_level_string)] = 100 - Ref$asr[grepl("non-participant", Ref$measure_level_string)]
# Number of people participants screened / tested in the period
Ref$avgct = rep(round(asr$n), nrow(Ref))
# Number of eligible people who haven't participated in screening / testing - just in case
Ref$avgct[grepl("non-participant", Ref$measure_level_string)] = round(asr$pop - asr$n)


# Tango's MEET
meet <- read.csv(fp.meet)

p.meet = average(meet[,-1])

Ref$meet <- rep(0 + as.numeric(p.meet < 0.1) + as.numeric(p.meet < 0.05) + as.numeric(p.meet < 0.01))

# Save estimates
write.csv(Ref,
          file = fp.out,
          row.names = F)
