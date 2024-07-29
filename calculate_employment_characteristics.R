####################################################################################
# This script uses raw data from the CSU, CDE, and NCES to estimate employment characteristics
# for CSU credential completers for the annual MSTI report
#
# S. Kolbe
####################################################################################

####################################################################################
# Data sources
####################################################################################

# Employment: use EdQ Center master employment data until Ed-Fi/TPDM ODS is available
#
# Poverty: CDE data available at https://www.cde.ca.gov/ds/sd/sd/filessp.asp
#
# Locale: NCES data available at https://nces.ed.gov/programs/edge/Geographic/SchoolLocations (may need to use a prior year if analysis year is unavailable)
# NCES-CDS crosswalk: https://www.cde.ca.gov/ds/si/ds/pubschls.asp as 'Public Schools and Districts'
#
# School credentials: CDE data available at https://www.cde.ca.gov/ds/ad/staffdemo.asp
#
# Achievement: go to https://www.cde.ca.gov/ta/ac/sa/, and from there find the downloadable SARC files for the analysis year,
# and download the camath file (or rewrite to work with the CA school dashboard files)

####################################################################################
# Set up
####################################################################################

library(dplyr)
library(magrittr)
library(data.table)
library(stringr)

options(stringsAsFactors = FALSE)
options(scipen = 999)

cohort <- "2018-2019"
pubschyear <- paste0(as.numeric(substr(cohort, 1, 4)) + 1, "-", as.numeric(substr(cohort, 6, 9)) + 1)

####################################################################################
# Read in employment data
####################################################################################

emp <- read.csv("source_data/master_employment_data_2021-04-29.csv") %>%
  filter(., compCohort %in% cohort) %>%
  filter(., emp_years_post_completion == 1) %>%
  filter(., contentDescription %in% c("Math", "Foundations of Math",
                                         "Biology", "Biology Specialized", "Chemistry", "Chemistry Specialized",
                                         "Foundational-Level General Science", "Geoscience", "Geoscience Specialized",
                                         "Physics", "Physics Specialized")) %>%
  mutate(., cds = str_pad(empl_cds_code, width = 14, pad = 0)) %>%
  select(., studyCode, ssn, cds, credentialCode, credentialDescription, contentCode, contentDescription, campusAbb, campusType, compCohort)

####################################################################################
# Read in completer and employment data
####################################################################################

frpm <- read.csv("source_data/calpads_upc_1415_1920_181120.csv") %>%
  filter(., Academic.Year == max(pubschyear))

pubschls <- read.csv("source_data/pubschls_1415_1920_181120.csv")

locale <- read.csv("source_data/EDGE_GEOCODE_PUBLICSCH_1920/EDGE_GEOCODE_PUBLICSCH_1920/EDGE_GEOCODE_PUBLICSCH_1920.csv")

# This is the most recent data available at time of analysis
staffcred <- read.csv("source_data/StaffCred18.csv")
staffschool <- read.csv("source_data/StaffSchoolFTE18.csv")

# Note that there was no assessment data for 2019-2020 due to covid, so a prior year of data is used here
camath <- read.csv("source_data/camath.csv")

####################################################################################
# Prep poverty data
# Select the useful fields, prep the CDS code, add frpm level indicators
####################################################################################

poverty <- select(frpm, 
                  countyID = County.Code, districtID = District.Code, schoolID = School.Code, 
                  n_frpm = Free..ReducedMealProgram, n_tot = Total.Enrollment) %>%
  mutate(., 
         n_frpm = as.numeric(gsub(",", "", n_frpm)),
         n_tot = as.numeric(gsub(",", "", n_tot)),
         perc_frpm = round(n_frpm/n_tot * 100, 1),
         countyID = str_pad(countyID, width = 2, pad = 0),
         districtID = str_pad(districtID, width = 5, pad = 0),
         schoolID = str_pad(schoolID, width = 5, pad = 0),
         cds = paste0(countyID, districtID, schoolID)
         ) %>%
  select(., cds, perc_frpm)

# Add indicators of 25% and 50% poverty
poverty$frpm_above_25 <- 0
poverty$frpm_above_50 <- 0
poverty$frpm_above_25[poverty$perc_frpm > 25] <- 1
poverty$frpm_above_50[poverty$perc_frpm > 50] <- 1

poverty %<>% select(., cds, frpm_above_25, frpm_above_50)

rm(frpm)

####################################################################################
# Add CDS code to locale data
####################################################################################

locale %<>% 
  select(., NCESSCH, STATE, LOCALE) %>%
  filter(., STATE == "CA")
locale$NCESSCH %<>% str_pad(., width = 12, pad = 0)

# Recode locales into bins. Documentation here:
# https://nces.ed.gov/ccd/pdf/sl051bgen.pdf
locale$locale_bin <- NA
locale$locale_bin[locale$LOCALE %in% c(11, 12, 13)] <- "City"
locale$locale_bin[locale$LOCALE %in% c(21, 22, 23)] <- "Suburb"
locale$locale_bin[locale$LOCALE %in% c(31, 32, 33)] <- "Town"
locale$locale_bin[locale$LOCALE %in% c(41, 42, 43)] <- "Rural"

nces_cds_crosswalk <- select(pubschls, cds = CDSCode, NCESDist, NCESSchool) %>%
  filter(., NCESSchool != "No Data" & NCESDist != "No Data") %>%
  mutate(., NCESDist = as.numeric(NCESDist),
         NCESSchool = as.numeric(NCESSchool)) %>%
  mutate(., NCESDist = str_pad(NCESDist, width = 7, pad = 0),
         NCESSchool = str_pad(NCESSchool, width = 5, pad = 0),
         NCESSCH = paste0(NCESDist, NCESSchool)) %>%
  select(., cds, NCESSCH)

locale <- merge(locale, nces_cds_crosswalk, by = "NCESSCH") %>%
  select(., cds, locale_bin)

rm(nces_cds_crosswalk, pubschls)

####################################################################################
# Prep teacher cred data
####################################################################################

# Explanation of the values in staffschool is available here:
# https://www.cde.ca.gov/ds/ad/fsstaffschoolfte.asp

# Explanation of the values in staffcred is available here:
# https://www.cde.ca.gov/ds/ad/fsstaffcred12.asp

teachers <- select(staffschool, RecID, DistrictCode, SchoolCode, StaffType, FTE) %>%
  mutate(., DistrictCode = str_pad(DistrictCode, width = 7, pad = 0),
         SchoolCode = str_pad(SchoolCode, width = 7, pad = 0),
         cds = paste0(DistrictCode, SchoolCode))

cred <- select(staffcred, RecID, CredentialType) %>%
  unique(.)

teachers <- merge(teachers, cred, by = "RecID") %>%
  select(., cds, RecID, StaffType, CredentialType) %>%
  filter(., StaffType == "T")

school_cred_rates <- teachers %>%
  group_by(cds) %>%
  summarize(
    n_teachers = n_distinct(RecID),
    n_full_cred = n_distinct(RecID[CredentialType == 10]),
    perc_full_cred = round((n_distinct(RecID[CredentialType == 10])/n_distinct(RecID))*100,0)
  ) %>%
  as.data.frame(.)

school_cred_rates$school_fully_cred <- 0
school_cred_rates$school_fully_cred[school_cred_rates$perc_full_cred == 100] <- 1

school_cred_rates %<>% select(., cds, school_fully_cred)

rm(staffcred, staffschool, teachers, cred)

####################################################################################
# Prep achievement data
####################################################################################

# MTES = total enrollment, all students group
# MNTS = number tested, all students group
# MPTS = percent tested, all students group
# MMES = percent met or exceeded, all students group

achievement <- select(camath, CDSCode, MTES, MNTS, MPTS, MMES)%>%
  rename(cds = CDSCode)
achievement$MMES[achievement$MMES=="--"] <- NA
achievement$MNTS[achievement$MNTS=="--"] <- NA
achievement$MMES %<>% as.numeric(.)
achievement$MNTS %<>% as.numeric(.)
achievement$MBE <- NA
achievement$MBE = 100 - achievement$MMES
# Get approximate count of number meeting or exceeding expectations bast on MNTS and MMES
achievement$MMES_n <- round(achievement$MNTS * (achievement$MMES/100),0)
# Get approximate count of number below expectations
achievement$MBE_n <- achievement$MNTS - achievement$MMES_n

achievement$cds %<>% str_pad(., width = 14, pad = 0)

achievement$below_exp_50 <- 0
achievement$below_exp_60 <- 0
achievement$below_exp_75 <- 0

achievement$below_exp_50[achievement$MBE > 50] <- 1
achievement$below_exp_60[achievement$MBE > 60] <- 1
achievement$below_exp_75[achievement$MBE > 75] <- 1

achievement %<>% select(., cds, below_exp_50, below_exp_60, below_exp_75, MNTS, MMES_n)

rm(camath)

####################################################################################
# Combine school characteristics with employment characteristics
####################################################################################

combined <- left_join(emp, poverty, by = "cds") %>%
  left_join(., locale, by = "cds") %>%
  left_join(., school_cred_rates, by = "cds") %>%
  Left_join(., achievement, by = "cds") %>%
  arrange(., studyCode)

####################################################################################
# Calculate summary stats for characteristics of schools where math and science
# completers are employed
####################################################################################

employment_desc_summary <- combined %>%
  summarize(
    n_math_sci_employed = n_distinct(studyCode),
    perc_in_schools_with_frpm_above_25 = n_distinct(studyCode[frpm_above_25 == 1 & !is.na(frpm_above_25)])/n_distinct(studyCode[!is.na(frpm_above_25)]),
    perc_in_schools_with_frpm_above_50 = n_distinct(studyCode[frpm_above_50 == 1 & !is.na(frpm_above_50)])/n_distinct(studyCode[!is.na(frpm_above_50)]),
    perc_in_city = n_distinct(studyCode[locale_bin == "City" & !is.na(locale_bin)])/n_distinct(studyCode[!is.na(locale_bin)]),
    perc_in_suburb = n_distinct(studyCode[locale_bin == "Suburb" & !is.na(locale_bin)])/n_distinct(studyCode[!is.na(locale_bin)]),
    perc_in_town = n_distinct(studyCode[locale_bin == "Town" & !is.na(locale_bin)])/n_distinct(studyCode[!is.na(locale_bin)]),
    perc_in_rural = n_distinct(studyCode[locale_bin == "Rural" & !is.na(locale_bin)])/n_distinct(studyCode[!is.na(locale_bin)]),
    perc_in_school_fully_cred = n_distinct(studyCode[school_fully_cred == 1 & !is.na(school_fully_cred)])/n_distinct(studyCode[!is.na(school_fully_cred)]),
    perc_in_schools_with_below_exp_50 = n_distinct(studyCode[below_exp_50 == 1 & !is.na(below_exp_50)])/n_distinct(studyCode[!is.na(below_exp_50)]),
    perc_in_schools_with_below_exp_60 = n_distinct(studyCode[below_exp_60 == 1 & !is.na(below_exp_60)])/n_distinct(studyCode[!is.na(below_exp_60)]),
    perc_in_schools_with_below_exp_75 = n_distinct(studyCode[below_exp_75 == 1 & !is.na(below_exp_75)])/n_distinct(studyCode[!is.na(below_exp_75)])
  ) %>%
  as.data.frame(.)

write.csv(employment_desc_summary, "output/employment_desc_summary.csv", row.names = FALSE)

# Another angle on achievement
achv_where_employed <- subset(achievement, cds %in% emp$cds)
sum(achv_where_employed$MMES_n, na.rm = TRUE)/sum(achv_where_employed$MNTS, na.rm=TRUE)

# Percent meeting or exceeding math expectations statewide
sum(achievement$MMES_n, na.rm = TRUE)/sum(achievement$MNTS, na.rm=TRUE)
