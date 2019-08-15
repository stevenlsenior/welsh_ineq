# Intro/Packages ----------------------------------------------------------

#This project is an exploratory data analysis of mortality statistics from Wales 2001-2017

#We begin by looking at mortality data from the most deprived 10% lower-super output areas (LSOAs) and
#least deprived 10% LSOAs by Welsh IMD 2014 scores, broken down by age, gender and
#International Classification of Disease-10 (ICD-10) category and
#exploring trends in mortality rates and life expectancy over this period

#Note: deaths have been categorised into group causes - see paper for full methodology

#Load packages
library(sjmisc)
library(tidyverse)
library(readxl)
library(PHEindicatormethods)
library(LifeTables)
library(Matrix)
library(lfe)
library(plm)
library(tidyr)
library(grid)
library(gridExtra)

# Data import/clean -------------------------------------------------------

#Load data
deaths_01_16 <- read_excel("data_1981-2016.xls", sheet = 6, skip = 14, col_names = TRUE)
pop_01_16 <- read_excel("data_1981-2016.xls", sheet = 8, col_names = TRUE, range = cell_rows(4:324))
deaths_17 <- read_excel("data_2017.xls", sheet = 4, skip = 12, col_names = TRUE)
pop_17 <- read_excel("data_2017.xls", sheet = 6, skip = 12, col_names = TRUE)

#Rename columns in dataframes
#also amend last column of deaths dataframe to ensure identical between datasets
colnames(deaths_01_16)[5:25] <- paste("deaths_", colnames(deaths_01_16)[5:25], sep = "")
colnames(pop_01_16)[4:24] <- paste("popn_", colnames(pop_01_16)[4:24], sep="")
colnames(deaths_17)[5:25] <- paste("deaths_", colnames(deaths_17)[5:25], sep="")
colnames(pop_17)[4:24] <- paste("popn_", colnames(pop_17)[4:24], sep="")
colnames(deaths_17)[25] <- "deaths_All Ages"

#Merge mortality dataframes
deaths <- rbind(deaths_01_16, deaths_17)

#Merge population dataframes
popns <- rbind(pop_01_16, pop_17)

#Alter column names in dataframes for deprivation and cause of death to make identical
colnames(deaths)[2] <- "deprivation_decile"
colnames(popns)[2] <- "deprivation_decile"
colnames(deaths)[4] <- "cause_death"

#Merge mortality and population dataframes 

death_popn <- inner_join(deaths, popns, by = c("Year", "deprivation_decile", "Sex"))

#Generate new dataset with aggregated mortality for all causes
agg_deaths <- deaths %>% select(-cause_death) %>% group_by(Year, deprivation_decile, Sex) %>% summarise_all(funs(sum))

#Convert datasets from wide to long format
names(agg_deaths) <- make.names(names(agg_deaths), unique = TRUE)
agg_deaths$Year <- as.factor(as.character(agg_deaths$Year))
agg_deaths$Sex <- as.factor(as.character(agg_deaths$Sex))
agg_deaths$deprivation_decile <- as.factor(as.character(agg_deaths$deprivation_decile))
agg_deaths_long <- gather(agg_deaths, age_grp, deaths, deaths_.1:deaths_All.Ages, factor_key = FALSE)

#Change data content for age group variable 
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_.1"] <- "0"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_01.04"] <- "1"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_05.09"] <- "5"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_10.14"] <- "10"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_15.19"] <- "15"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_20.24"] <- "20"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_25.29"] <- "25"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_30.34"] <- "30"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_35.39"] <- "35"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_40.44"] <- "40"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_45.49"] <- "45"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_50.54"] <- "50"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_55.59"] <- "55"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_60.64"] <- "60"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_65.69"] <- "65"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_70.74"] <- "70"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_75.79"] <- "75"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_80.84"] <- "80"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_85.89"] <- "85"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_90."] <- "90"
agg_deaths_long$age_grp[agg_deaths_long$age_grp == "deaths_All.Ages"] <- "All"

#Convert population dataset into long format
names(popns) <- make.names(names(popns), unique = TRUE)
popns$Year <- as.factor(as.character(popns$Year))
popns$Sex <- as.factor(as.character(popns$Sex))
popns$deprivation_decile <- as.factor(as.character(popns$deprivation_decile))
agg_popns_long <- gather(popns, age_grp, popn, popn_.1:popn_All.Ages, factor_key = FALSE)

#Change data content for age group variable 
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_.1"] <- "0"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_01.04"] <- "1"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_05.09"] <- "5"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_10.14"] <- "10"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_15.19"] <- "15"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_20.24"] <- "20"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_25.29"] <- "25"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_30.34"] <- "30"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_35.39"] <- "35"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_40.44"] <- "40"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_45.49"] <- "45"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_50.54"] <- "50"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_55.59"] <- "55"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_60.64"] <- "60"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_65.69"] <- "65"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_70.74"] <- "70"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_75.79"] <- "75"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_80.84"] <- "80"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_85.89"] <- "85"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_90."] <- "90"
agg_popns_long$age_grp[agg_popns_long$age_grp == "popn_All.Ages"] <- "All"

#Merge mortality and population dataframes after filtering mortality data before 2001
agg_deaths_long$Year <- as.numeric(as.character(agg_deaths_long$Year))
agg_deaths_long <- agg_deaths_long %>% filter(Year>2000)
agg_popns_long$Year <- as.numeric(as.character(agg_popns_long$Year))
deaths_popns <- left_join(agg_deaths_long, agg_popns_long, by = c("Year", "deprivation_decile", "Sex", "age_grp"))


# AS-mortality calculation ------------------------------------------------


#Create age-standardised mortality rates using European Standard Population (ESP2013)
#by gender and cause of death for top 10% and bottom 10% deciles of deprivation
#using dsr calculation from PHE Indicator Methods package

#First filter data to get top and bottom decile and remove aggregated deaths/populations for deciles
dec1_dec10 <- deaths_popns %>% filter(deprivation_decile == "1" | deprivation_decile == "10") %>% filter(age_grp != "All")

#Collapse data for age-bands 0 and 1 (0-1 and 1-4) into one age band to match ESP2013 standard population
under_five <- dec1_dec10 %>% filter(age_grp == "0" | age_grp == "1")
under_five <- under_five %>% select(-age_grp) %>% group_by(Year, deprivation_decile, Sex) %>%
  summarise_all(funs(sum))
nrow(under_five)
under_five$age_grp <- c(rep(0, 68))
dec1_dec10_5_plus <- dec1_dec10 %>% filter(!age_grp %in% c(0,1))
dec1_dec10_5_plus$age_grp <- as.numeric(dec1_dec10_5_plus$age_grp)
dec1_dec10_v2 <- rbind(dec1_dec10_5_plus, under_five)
dec1_dec10_v2 <- dec1_dec10_v2 %>% filter(age_grp != "NA")

#Arrange data by age-band and rename columns and filter into two datasets by gender
dec1_dec10_v2 <- dec1_dec10_v2 %>% rename(ageband = age_grp)
dec1_dec10_v2 <- dec1_dec10_v2 %>% arrange(ageband)
dec1_dec10_v2_m <- dec1_dec10_v2 %>% filter(Sex == "Male")
dec1_dec10_v2_f <- dec1_dec10_v2 %>% filter(Sex == "Female")

#Create graphs using phe_dsr from PHE indocator methods package
#and ggplot of trends in direct age-standardised mortality rates for deciles 1 and 10 for men and women
phe_dsr(dec1_dec10_v2_m, deaths, popn) %>% filter(deprivation_decile == "1" | deprivation_decile == "10") %>%
  ggplot(aes(x=Year, y=value)) + geom_line(aes(colour=deprivation_decile)) 
phe_dsr(dec1_dec10_v2_f, deaths, popn) %>% filter(deprivation_decile == "1" | deprivation_decile == "10") %>% 
  ggplot(aes(x=Year, y=value)) + geom_line(aes(colour=deprivation_decile))

#Now using this approach go back to original dataset earlier in analysis
#and generate cause-specific age-adjusted mortality rates for each decile 
#and gender during the period 2001-2017

#The dataset needs to be converted from wide to long format
#with aggregated deaths (for all causes combined)
#also generated to allow graphs of trends in cause-specific
#and all-cause mortality to be calculated
names(death_popn) <- make.names(names(death_popn), unique = TRUE)

death_popn_long <- death_popn %>% group_by(Year, deprivation_decile, Sex, cause_death) %>% 
  to_long(keys = "ageband", values = c("deaths", "popn"),
          c("deaths_.1", "deaths_01.04", "deaths_05.09", 
            "deaths_10.14", "deaths_15.19", "deaths_20.24", 
            "deaths_25.29", "deaths_30.34", "deaths_35.39", 
            "deaths_40.44", "deaths_45.49", "deaths_50.54", 
            "deaths_55.59", "deaths_60.64", "deaths_65.69", 
            "deaths_70.74", "deaths_75.79", "deaths_80.84", 
            "deaths_85.89", "deaths_90.", "deaths_All.Ages"), 
          c("popn_.1", "popn_01.04", "popn_05.09",
            "popn_10.14", "popn_15.19", "popn_20.24",
            "popn_25.29", "popn_30.34", "popn_35.39",
            "popn_40.44", "popn_45.49", "popn_50.54",
            "popn_55.59", "popn_60.64", "popn_65.69",
            "popn_70.74", "popn_75.79", "popn_80.84",
            "popn_85.89", "popn_90.", "popn_All.Ages"))

#Change data content for ageband variable 
death_popn_long$ageband[death_popn_long$ageband == "deaths_.1"] <- "0"
death_popn_long$ageband[death_popn_long$ageband == "deaths_01.04"] <- "1"
death_popn_long$ageband[death_popn_long$ageband == "deaths_05.09"] <- "5"
death_popn_long$ageband[death_popn_long$ageband == "deaths_10.14"] <- "10"
death_popn_long$ageband[death_popn_long$ageband == "deaths_15.19"] <- "15"
death_popn_long$ageband[death_popn_long$ageband == "deaths_20.24"] <- "20"
death_popn_long$ageband[death_popn_long$ageband == "deaths_25.29"] <- "25"
death_popn_long$ageband[death_popn_long$ageband == "deaths_30.34"] <- "30"
death_popn_long$ageband[death_popn_long$ageband == "deaths_35.39"] <- "35"
death_popn_long$ageband[death_popn_long$ageband == "deaths_40.44"] <- "40"
death_popn_long$ageband[death_popn_long$ageband == "deaths_45.49"] <- "45"
death_popn_long$ageband[death_popn_long$ageband == "deaths_50.54"] <- "50"
death_popn_long$ageband[death_popn_long$ageband == "deaths_55.59"] <- "55"
death_popn_long$ageband[death_popn_long$ageband == "deaths_60.64"] <- "60"
death_popn_long$ageband[death_popn_long$ageband == "deaths_65.69"] <- "65"
death_popn_long$ageband[death_popn_long$ageband == "deaths_70.74"] <- "70"
death_popn_long$ageband[death_popn_long$ageband == "deaths_75.79"] <- "75"
death_popn_long$ageband[death_popn_long$ageband == "deaths_80.84"] <- "80"
death_popn_long$ageband[death_popn_long$ageband == "deaths_85.89"] <- "85"
death_popn_long$ageband[death_popn_long$ageband == "deaths_90."] <- "90"
death_popn_long$ageband[death_popn_long$ageband == "deaths_All.Ages"] <- "All"

#Remove "All" deaths/population counts
#and aggregate deaths and populations in 0-1 and 1-4 age bands
death_popn_long <- death_popn_long %>% filter(ageband != "All")
death_popn_long$ageband <- as.numeric(death_popn_long$ageband)

#Create data for age-adjusted mortality rates by gender, cause, year and WIMD decile

#First separate dataframe into two distinct dataframes by gender
agg_data_m <- death_popn_long %>% 
  mutate(new_ageband = if_else(ageband %in% c("0", "1"), 0, ageband)) %>%
  group_by(Year, deprivation_decile, Sex, cause_death, new_ageband) %>%
  summarise(deaths = sum(deaths), popn = sum(popn)) %>%
  filter(Sex == "Male")

agg_data_f <- death_popn_long %>% 
  mutate(new_ageband = if_else(ageband %in% c("0", "1"), 0, ageband)) %>%
  group_by(Year, deprivation_decile, Sex, cause_death, new_ageband) %>%
  summarise(deaths = sum(deaths), popn = sum(popn)) %>%
  filter(Sex == "Female")

#Rename new_ageband and remove old ageband variables and arrange by ageband
agg_data_m <- agg_data_m %>%
  rename(ageband = new_ageband) %>%
  arrange(ageband)

agg_data_f <- agg_data_f %>%
  rename(ageband = new_ageband) %>%
  arrange(ageband)

#Create graphs using phe_dsr from PHE indicator methods package
#and ggplot
agg_data_m %>%
  group_by(Year, deprivation_decile, cause_death) %>%
phe_dsr(deaths, popn) %>%
  ggplot(aes(x=Year, y=value, color = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  facet_wrap(~cause_death)

agg_data_f %>%
  group_by(Year, deprivation_decile, cause_death) %>%
  phe_dsr(deaths, popn) %>%
  ggplot(aes(x=Year, y=value, color = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  facet_wrap(~cause_death)

#Recode cause of death categories for easier to read summaries on graphs

agg_data_m$cause_death[agg_data_m$cause_death == "01 Malignant neoplasm of liver and intrahepatic bile ducts"] <- "Hepatobiliary cancers"
agg_data_m$cause_death[agg_data_m$cause_death == "02 Malignant neoplasm of colon, rectosigmoid junction and rectum (i.e.colorectal)"] <- "Colorectal cancers"
agg_data_m$cause_death[agg_data_m$cause_death == "03 Malignant neoplasms of digestive organs except liver and intrahepatic bile ducts"] <- "Digestive cancers"              
agg_data_m$cause_death[agg_data_m$cause_death == "04 Malignant neoplasms of lymphoid, haematopoietic and related tissue"] <- "Haematological cancers"              
agg_data_m$cause_death[agg_data_m$cause_death == "05 Malignant neoplasm of trachea, bronchus and lung"] <- "Lung cancer"              
agg_data_m$cause_death[agg_data_m$cause_death == "06 Malignant neoplasm of prostate"] <- "Prostate cancer"              
agg_data_m$cause_death[agg_data_m$cause_death == "07 Malignant neoplasm of breast"] <- "Breast cancer"              
agg_data_m$cause_death[agg_data_m$cause_death == "09 All other cancers (C00-D48)"] <- "All other cancers"              
agg_data_m$cause_death[agg_data_m$cause_death == "10 Diseases of the respiratory system"] <- "Respiratory diseases"              
agg_data_m$cause_death[agg_data_m$cause_death == "11 Diabetes"] <- "Diabetes"              
agg_data_m$cause_death[agg_data_m$cause_death == "12 Ischaemic heart disease"] <- "Ischaemic heart disease"              
agg_data_m$cause_death[agg_data_m$cause_death == "13 Stroke (cerebrovascular)"] <- "Stroke"              
agg_data_m$cause_death[agg_data_m$cause_death == "14 Other circulatory" ] <- "Other circulatory"              
agg_data_m$cause_death[agg_data_m$cause_death == "15 Intentional injuries"] <- "Intentional injuries"              
agg_data_m$cause_death[agg_data_m$cause_death == "16 Unintentional injuries"] <- "Unintentional injuries"              
agg_data_m$cause_death[agg_data_m$cause_death == "17 Dementia and Alzheimers"] <- "Dementia and Alzheimers"              
agg_data_m$cause_death[agg_data_m$cause_death == "18 All other causes"] <- "All other causes"              
agg_data_m$cause_death[agg_data_m$cause_death == "19 Neonatal"] <- "Neonatal"              

agg_data_f$cause_death[agg_data_f$cause_death == "01 Malignant neoplasm of liver and intrahepatic bile ducts"] <- "Hepatobiliary cancers"
agg_data_f$cause_death[agg_data_f$cause_death == "02 Malignant neoplasm of colon, rectosigmoid junction and rectum (i.e.colorectal)"] <- "Colorectal cancers"
agg_data_f$cause_death[agg_data_f$cause_death == "03 Malignant neoplasms of digestive organs except liver and intrahepatic bile ducts"] <- "Digestive cancers"              
agg_data_f$cause_death[agg_data_f$cause_death == "04 Malignant neoplasms of lymphoid, haematopoietic and related tissue"] <- "Haematological cancers"              
agg_data_f$cause_death[agg_data_f$cause_death == "05 Malignant neoplasm of trachea, bronchus and lung"] <- "Lung cancer"              
agg_data_f$cause_death[agg_data_f$cause_death == "06 Malignant neoplasm of prostate"] <- "Prostate cancer"              
agg_data_f$cause_death[agg_data_f$cause_death == "07 Malignant neoplasm of breast"] <- "Breast cancer"
agg_data_f$cause_death[agg_data_f$cause_death == "08 Cervical Cancer"] <- "Cervical cancer"
agg_data_f$cause_death[agg_data_f$cause_death == "09 All other cancers (C00-D48)"] <- "All other cancers"              
agg_data_f$cause_death[agg_data_f$cause_death == "10 Diseases of the respiratory system"] <- "Respiratory diseases"              
agg_data_f$cause_death[agg_data_f$cause_death == "11 Diabetes"] <- "Diabetes"              
agg_data_f$cause_death[agg_data_f$cause_death == "12 Ischaemic heart disease"] <- "Ischaemic heart disease"              
agg_data_f$cause_death[agg_data_f$cause_death == "13 Stroke (cerebrovascular)"] <- "Stroke"              
agg_data_f$cause_death[agg_data_f$cause_death == "14 Other circulatory" ] <- "Other circulatory"              
agg_data_f$cause_death[agg_data_f$cause_death == "15 Intentional injuries"] <- "Intentional injuries"              
agg_data_f$cause_death[agg_data_f$cause_death == "16 Unintentional injuries"] <- "Unintentional injuries"              
agg_data_f$cause_death[agg_data_f$cause_death == "17 Dementia and Alzheimers"] <- "Dementia and Alzheimers"              
agg_data_f$cause_death[agg_data_f$cause_death == "18 All other causes"] <- "All other causes"              
agg_data_f$cause_death[agg_data_f$cause_death == "19 Neonatal"] <- "Neonatal" 

#Create final AS graphs
agg_data_m$deprivation_decile <- as.factor(as.character(agg_data_m$deprivation_decile))
agg_data_m %>%
  group_by(Year, deprivation_decile, cause_death) %>%
  phe_dsr(deaths, popn) %>%
  ggplot(aes(x=Year, y=value, colour = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  facet_wrap(~cause_death) + 
  labs(title = "Trends in age-standardised mortality rates by underlying cause of death and deprivation decile",
       subtitle = "Males",
       y = "Age-adjusted mortality rate per 100,000 people")

agg_data_f$deprivation_decile <- as.factor(as.character(agg_data_f$deprivation_decile))
agg_data_f %>%
  group_by(Year, deprivation_decile, cause_death) %>%
  phe_dsr(deaths, popn) %>%
  ggplot(aes(x=Year, y=value, colour = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  facet_wrap(~cause_death) + 
  labs(title = "Trends in age-standardised mortality rates by underlying cause of death and deprivation decile",
       subtitle = "Females",
       y = "Age-adjusted mortality rate per 100,000 people")

#Manually calculate age-adjusted mortality rates

#First obtain European Standard Population (2013)
ageband <- seq(0,90,5)
sp <- data.frame(cbind(ageband, esp2013))

#Merge ESP2013 data with existing dataframe
data_esp <- inner_join(death_popn_long, sp, by = "ageband")

#Rename cause_death data
data_esp$cause_death[data_esp$cause_death == "01 Malignant neoplasm of liver and intrahepatic bile ducts"] <- "Hepatobiliary cancers"
data_esp$cause_death[data_esp$cause_death == "02 Malignant neoplasm of colon, rectosigmoid junction and rectum (i.e.colorectal)"] <- "Colorectal cancers"
data_esp$cause_death[data_esp$cause_death == "03 Malignant neoplasms of digestive organs except liver and intrahepatic bile ducts"] <- "Digestive cancers"              
data_esp$cause_death[data_esp$cause_death == "04 Malignant neoplasms of lymphoid, haematopoietic and related tissue"] <- "Haematological cancers"              
data_esp$cause_death[data_esp$cause_death == "05 Malignant neoplasm of trachea, bronchus and lung"] <- "Lung cancer"              
data_esp$cause_death[data_esp$cause_death == "06 Malignant neoplasm of prostate"] <- "Prostate cancer"              
data_esp$cause_death[data_esp$cause_death == "07 Malignant neoplasm of breast"] <- "Breast cancer"
data_esp$cause_death[data_esp$cause_death == "08 Cervical Cancer"] <- "Cervical cancer"
data_esp$cause_death[data_esp$cause_death == "09 All other cancers (C00-D48)"] <- "All other cancers"              
data_esp$cause_death[data_esp$cause_death == "10 Diseases of the respiratory system"] <- "Respiratory diseases"              
data_esp$cause_death[data_esp$cause_death == "11 Diabetes"] <- "Diabetes"              
data_esp$cause_death[data_esp$cause_death == "12 Ischaemic heart disease"] <- "Ischaemic heart disease"              
data_esp$cause_death[data_esp$cause_death == "13 Stroke (cerebrovascular)"] <- "Stroke"              
data_esp$cause_death[data_esp$cause_death == "14 Other circulatory" ] <- "Other circulatory"              
data_esp$cause_death[data_esp$cause_death == "15 Intentional injuries"] <- "Intentional injuries"              
data_esp$cause_death[data_esp$cause_death == "16 Unintentional injuries"] <- "Unintentional injuries"              
data_esp$cause_death[data_esp$cause_death == "17 Dementia and Alzheimers"] <- "Dementia and Alzheimers"              
data_esp$cause_death[data_esp$cause_death == "18 All other causes"] <- "All other causes"              
data_esp$cause_death[data_esp$cause_death == "19 Neonatal"] <- "Neonatal"

#Calculate AS mortality rates using population weights
data_esp <- data_esp %>% group_by(Year, deprivation_decile, Sex, cause_death) %>% 
  mutate(total = sum(esp2013))
data_esp <- data_esp %>% mutate(weight=esp2013/total)
data_esp <- data_esp %>% mutate(crude = deaths/popn)
data_esp <- data_esp %>% mutate(dsr = (crude * weight)*100000)


# Visualise AS-mortality and trends in LE ---------------------------------


#Plot graphs
data_esp$deprivation_decile <- factor(as.character(data_esp$deprivation_decile),
                                         levels = c("1", "2", "3", "4", "5", "6", "7", 
                                                    "8", "9", "10"))

data_esp %>%
  filter(Sex == "Male") %>%
  group_by(Year, deprivation_decile, cause_death) %>%
  mutate(dsr=sum(dsr)) %>%
  ggplot(aes(x=Year, y=dsr, colour = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  facet_wrap(~cause_death) + 
  labs(title = "Trends in age-standardised mortality rates by underlying cause of death and deprivation decile",
       subtitle = "Males",
       y = "Age-adjusted mortality rate per 100,000 people")

data_esp %>%
  filter(Sex == "Female") %>%
  group_by(Year, deprivation_decile, cause_death) %>%
  mutate(dsr=sum(dsr)) %>%
  ggplot(aes(x=Year, y=dsr, colour = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  facet_wrap(~cause_death) + 
  labs(title = "Trends in age-standardised mortality rates by underlying cause of death and deprivation decile",
       subtitle = "Females",
       y = "Age-adjusted mortality rate per 100,000 people")

le_data1 %>%
  group_by(Year, Sex, deprivation_decile) %>%
  phe_life_expectancy(deaths = deaths, popn, startage = ageband) %>%
  filter(Sex == "Male" & ageband == "0") %>%
  ggplot(aes(x=Year, y=value, colour = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  labs(title = "Trends in life expectancy at birth by deprivation decile",
       subtitle = "Males",
       y = "Life expectancy at birth in years")

le_data1 %>%
  group_by(Year, Sex, deprivation_decile) %>%
  phe_life_expectancy(deaths = deaths, popn, startage = ageband) %>%
  filter(Sex == "Female" & ageband == "0") %>%
  ggplot(aes(x=Year, y=value, colour = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  labs(title = "Trends in life expectancy at birth by deprivation decile",
       subtitle = "Females",
       y = "Life expectancy at birth in years")

# Modelling ---------------------------------------------------------------


#Explore distributions of variables for modelling

#Distribution of male- and female- age-adjusted mortality rates

data_esp %>%
  filter(Sex == "Male") %>%
  mutate(dsr=sum(dsr)) %>%
  ggplot(aes(x=dsr, colour='darkred', fill='darkred')) + geom_density() +
  labs(x="Age-adjusted male life expectancy", y="Density", 
       title="Distribution of dependent variable / age-adjusted life expectancy in the dataset") + 
  theme(legend.position="none")

data_esp %>%
  filter(Sex == "Female") %>%
  mutate(dsr=sum(dsr)) %>%
  ggplot(aes(x=dsr)) + geom_density(fill='darkblue', colour='darkblue') +
  labs(x="Age-adjusted female life expectancy", y="Density", 
       title="Distribution of dependent variable / age-adjusted life expectancy in the dataset") + 
  theme(legend.position="none")

#Distribution of deaths by year, cause and deprivation decile for males and females

data_esp %>%
  filter(Sex == "Male") %>%
  mutate(dsr=sum(dsr)) %>%
  ggplot(aes(x=deaths)) + geom_density(fill='darkred', colour='darkred') + 
  facet_wrap(~Year) + 
  labs(x="Deaths", y="Density", 
       title="Distribution of numbers of deaths by year period") + 
  theme(legend.position="none")

data_esp %>%
  filter(Sex == "Male") %>%
  mutate(dsr=sum(dsr)) %>%
  ggplot(aes(x=deaths)) + geom_density(fill='darkred', colour='darkred') + 
  facet_wrap(~deprivation_decile) + 
  labs(x="Deaths", y="Density", 
       title="Distribution of numbers of deaths by deprivation decile") + 
  theme(legend.position="none")

data_esp %>%
  filter(Sex == "Male") %>%
  mutate(dsr=sum(dsr)) %>%
  ggplot(aes(x=deaths)) + geom_density(fill='darkred', colour='darkred') + 
  facet_wrap(~cause_death)  + 
  labs(x="Deaths", y="Density", 
       title="Distribution of numbers of deaths by cause of death") + 
  theme(legend.position="none")

#Run preliminary model

model_data_m <- data_esp %>%
  filter(Sex == "Male") %>%
  group_by(Year, deprivation_decile, cause_death) %>%
  mutate(dsr=sum(dsr))

model_data_f <- data_esp %>%
  filter(Sex == "Female") %>%
  group_by(Year, deprivation_decile, cause_death) %>%
  mutate(dsr=sum(dsr))

model_m <- felm(dsr ~ cause_death | deprivation_decile + Year, data = model_data_m)

model_m2 <- plm(dsr ~ cause_death, data = model_data_m, model = 'within', 
                effect='twoways', index = c('deprivation_decile', 'Year'))


# LT code ---------------------------------------------------------------

#Create life tables to calculate life expectancy and other variables
#needed for decomposition

#Methodology for life tables originally from Chiang at https://apps.who.int/iris/handle/10665/62916

#Amend aggregated death/population dataframe to remove 'all' age categories
#and create ordered deprivation deciles
#also order by age band

le_data1 <- deaths_popns %>%
  filter(age_grp !="All")
le_data1$deprivation_decile <- factor(as.character(le_data1$deprivation_decile),
                                        levels = c("1", "2", "3", "4", "5", "6", "7", 
                                                   "8", "9", "10"))
le_data1$age_grp <- as.numeric(as.character(le_data1$age_grp))
le_data1 <- le_data1 %>%
  arrange(age_grp)

#Create a function to generate life table using Chiang and Silcocks methodology
#First using Chiang II methodology from ONS
#See methodology at:
#https://webarchive.nationalarchives.gov.uk/20110824183840/http://www.statistics.gov.uk/statbase/ssdataset.asp?vlnk=6949

life_table <- le_data1 %>%
  group_by(Year, Sex, deprivation_decile) %>%
  mutate(ax #average fraction of interval lived
         = if_else(age_grp %in% c("0"), 0.1, 0.5),
         Mx #age-specific death rate
         = deaths/popn, 
         n #number of years between intervals
         = if_else(age_grp <90, dplyr::lead(age_grp)-age_grp, 2/Mx), 
         qx #proportion dying in interval
         = if_else(age_grp <90, (n*Mx)/(1+n*(1-ax)*Mx), 1),
         px #proportion surviving 
         = 1-qx,
         lx #number alive at age x
         = if_else(age_grp %in% c("0"), 100000, 1),
         px_lag = dplyr::lag(px, default = 1),
         lx = if_else(age_grp >0, cumprod(lx*px_lag), lx),
         dx #number dying in interval
         = lx - dplyr::lead(lx, default = 0),
         Lx #total years lived in interval
         = if_else(age_grp <90, n*(dplyr::lead(lx)+(ax*dx)), lx/Mx),
         Tx #total years lived beyond age x
         = rev(cumsum(rev(Lx))),
         ex #estimated life expectancy
         = Tx/lx)
         
#This gives LEs approximately equal to the PHEindicatormethods package
#next step to explore why differences

#Create CSV files:
#One with life table from this code
#One with LE calculations from PHE function
#Then one from amended PHE function code to generate 'PHE Life Table'

write.csv(life_table, "chiang.lt.csv")
write.csv(le_data1 %>% 
            group_by(Year, Sex, deprivation_decile) %>%
            phe_life_expectancy(deaths, popn, startage = age_grp, 
                                age_contents = c(0L, 1L, 5L, 10L, 15L,
                                                 20L, 25L, 30L, 35L, 40L,
                                                 45L, 50L, 55L, 60L, 65L,
                                                 70L, 75L, 80L, 85L, 90L)),
          "phe.le.csv")

#Generate life table drawing on PHE Indicator Methods methodology
#see https://rdrr.io/cran/PHEindicatormethods/src/R/LifeExpectancy.R

#Adapt PHE function from phe_life_expectancy function to create life table
#Credit to Sebastian Fox and PHEindicatormethods package for amended code

#Mortality data has to be arranged by ascending age category
#with 20 age bands to fit with the age_contents rule
#assumes there are no missing variables, minimum age is 0 and
#each population category has a population >5000

life_tab <- function(data, deaths, population, startage,
                        age_contents = c(0L, 1L, 5L, 10L, 15L,
                                         20L, 25L, 30L, 35L, 40L,
                                         45L, 50L, 55L, 60L, 65L,
                                         70L, 75L, 80L, 85L, 90L),
                        confidence = 0.95) {
  number_age_bands <- 20 #length(age_contents)
  # apply quotes
  deaths <- enquo(deaths)
  population <- enquo(population)
  startage <- enquo(startage)
  data <- data %>%
    mutate(startage_2b_removed = as.integer(sub("\\D*(\\d+).*", "\\1", !!startage)))
  if (confidence >= 90) {
    confidence <- confidence/100
  }
  z <- qnorm(confidence + (1 - confidence)/2)
  data$group_id_2b_removed <- data %>%
    group_indices()
  data <- data %>%
    mutate(id_2b_removed = row_number(),
           #age interval column 
           ni_2b_removed = as.numeric(dplyr::lead(startage_2b_removed) - startage_2b_removed),
           #fraction of year of life lived (ax)
           ai_2b_removed = case_when(
             startage_2b_removed == 0 ~ 0.1,
             TRUE ~ 0.5),
           #age-specific mortality rate (Mx)
           M_2b_removed = !!deaths / !!population,
           #fraction of year of life lived at extremes of age  (I think)
           ni_2b_removed = case_when(
             is.na(ni_2b_removed) ~ 2 / M_2b_removed,
             TRUE ~ ni_2b_removed),
           #Proportion dying during age interval (qx)
           qi_2b_removed = case_when(
             id_2b_removed < number_age_bands ~ M_2b_removed * ni_2b_removed / (1 + M_2b_removed * ni_2b_removed * (1 - ai_2b_removed)),
             TRUE ~ 1),
           #Proportion surviving up to age interval (px)
           p_2b_removed = 1 - qi_2b_removed,
           #Number of hypothetical population of 10,000 alive at age interval (lx)
           l_2b_removed = case_when(
             id_2b_removed == 1 ~ 1e5,
             TRUE ~ 1),
           p1_2b_removed = lag(p_2b_removed,
                               default = 1),
           l_2b_removed = case_when(
             id_2b_removed != 1 ~ cumprod(l_2b_removed * p1_2b_removed),
             TRUE ~ l_2b_removed),
           #number dying during age interval (dx)
           di_2b_removed = l_2b_removed - dplyr::lead(l_2b_removed, default = 0),
           #number of years lived by total cohort in age interval (Lx)
           Li_2b_removed = case_when(
             id_2b_removed < number_age_bands ~ ni_2b_removed * (dplyr::lead(l_2b_removed) + (ai_2b_removed * di_2b_removed)),
             TRUE ~ l_2b_removed / M_2b_removed),
           #Total number of years lived beyond age x (Tx)
           Ti_2b_removed = rev(cumsum(rev(Li_2b_removed))),
           #Life expectancy at age x (ex)
           ei = case_when(
             l_2b_removed == 0 ~ 0,
             TRUE ~ Ti_2b_removed / l_2b_removed),
           spi_2b_removed = case_when(
             di_2b_removed == 0 ~ 0,
             TRUE ~ (qi_2b_removed ^ 2) * (1 - qi_2b_removed) / !!deaths),
           spi_2b_removed = case_when(
             id_2b_removed == number_age_bands ~ 4 / (!!deaths * (M_2b_removed ^ 2)),
             TRUE ~ spi_2b_removed),
           W_spi_2b_removed = case_when(
             id_2b_removed < number_age_bands ~ spi_2b_removed * (l_2b_removed ^ 2) * (((1 - ai_2b_removed) * ni_2b_removed + dplyr::lead(ei)) ^ 2),
             TRUE ~ ((l_2b_removed / 2) ^ 2) * spi_2b_removed),
           STi_2b_removed = rev(cumsum(rev(W_spi_2b_removed))),
           SeSE_2b_removed = sqrt(STi_2b_removed / (l_2b_removed ^ 2)),
           lowercl = ei - z * SeSE_2b_removed,
           uppercl = ei + z * SeSE_2b_removed)
}

  
phe_lt <- le_data1 %>%
  group_by(Year, Sex, deprivation_decile) %>%
  life_tab(deaths, popn, age_grp)

#Export

write.csv(phe_lt, "phe.lt.csv")

#Chiang life table gives estimated LE identical to the PHE LE function
#However the amended PHE code generates a LE significantly lower

#For simplicity and given idential LEs suggest using Chiang LT code
#to generate life tables from now on
  
#Attempt to use James' code for generating life tables

#Start with extract of mortality/population data

m_dec1_01 <- le_data1 %>%
  filter(Sex == "Male", 
         deprivation_decile == "1", 
         Year == "2001") 

#Aggregrate data for ages 0-5 and 85+

m_dec1_01 <- m_dec1_01 %>%
  #collapse 85-90 and 90+ into 85+
  mutate(new_age_grp = if_else(age_grp %in% c("85", "90"), 85, age_grp)) %>%
  group_by(Year, Sex, deprivation_decile, new_age_grp) %>%
  summarise(deaths=sum(deaths), popn=sum(popn)) %>%
  rename(age_grp = new_age_grp) %>%
  #collapse 0-1 and 1-5 into 0-5
  mutate(new_age_grp = if_else(age_grp %in% c("0", "1"), 0, age_grp)) %>%
  group_by(Year, Sex, deprivation_decile, new_age_grp) %>%
  summarise(deaths=sum(deaths), popn=sum(popn)) %>%
  rename(age_grp = new_age_grp)

#Generate Mx variable

m_dec1_01 <- m_dec1_01 %>%
  mutate(mx=deaths/popn)

#Import code for Kannisto-Thatcher code from James
  
.KTExtension <- function(lx70) {
  # lx70: lx for 70 and older (should have 4 rows - one for each age group 
  # 70-74, 75-79, 80-84, 85+ and as many columns as years)
  # This function calculates the average number of years lived by 
  # those who die in each age group 70-74, 75-79, 80-84, 85+.
  # Returns a matrix of same dimensions as lx70.
  
  # For age groups >= 70, calculate hazard rate using the approximation
  # mu(x+1/2) ~ -log(1 - q) = -log(p) where p is the probability of 
  # survival to the next age group and equals l(x+5) / lx	
  mux <- (log(lx70[-nrow(lx70), , drop = FALSE]) - log(lx70)[-1, , drop = FALSE]) / 5
  
  # Calculate lx for 1-year age groups from 70 to 85. For 70, 75, 80, 85 
  # use known values. For the rest use interpolation l71 = l70 * exp(-mu70), 
  # l72 = l70 * exp(-2 * mu70),..., l84 = l80 * exp(-4 * mu80)
  lx70 <- rbind(
    lx70[rep(seq(3), each = 5), , drop = FALSE] * 
      exp(-seq(0, 4) * mux[rep(seq(3), each = 5), , drop = FALSE]), 
    lx70[4, , drop = FALSE]
  )
  
  # Calculate dx and qx for 1-year age groups using lx, for ages >= 70
  dx70 <- rbind(lx70[-nrow(lx70), , drop = FALSE] - lx70[-1, , drop = FALSE], 
                lx70[nrow(lx70), , drop = FALSE]
  )
  qx70 <- dx70 / lx70
  
  # Run regression on logit of probability of dying
  logitqx70 <- log(qx70 / (1 - qx70)) 
  logitqx70[nrow(logitqx70), ] <- NA # Not defined for 85+
  y <- as.vector(logitqx70)
  x <- rep(70:85 + .5, length.out = length(y))
  num.yr <- length(y) / 16
  yr <- as.factor(rep(seq_len(num.yr), each = 16))
  w <- as.vector(dx70)
  if (nlevels(yr) == 1) {
    mod <- lm(y ~ x, weights = x)
    logA <- mod$coefficients[1] # intercept
    B <- mod$coefficients[2] # slope
  } else {
    mod <- lm(y ~ 0 + yr + yr:x, weights = w)
    logA <- mod$coefficients[paste0("yr", seq(num.yr))] # intercepts
    B <- mod$coefficients[paste0("yr", seq(num.yr), ":x")] # slopes
  }
  
  # Calculate qx for age x >= 85
  logitqx85 <- t(logA + outer(B, (85:129 + .5))) # predict logit qx for age >= 85
  qx85 <- exp(logitqx85) / (1 + exp(logitqx85)) # invert logit transform
  
  # Calculate lx values for age x >= 85
  lx85 <- matrix(nrow = nrow(logitqx85), ncol = ncol(logitqx85)) 
  lx85[1, ] <- lx70[nrow(lx70), ] # last entry of vector holding lx70-85 
  for (k in seq(2, nrow(lx85))) 
    lx85[k, ] <- lx85[k - 1, ] * (1 - qx85[k - 1, ])
  
  # Calculate dx for age x >= 85
  dx85 <- lx85 * qx85
  
  # Join lx70 (holding lx for 1-year age groups from 70 to 85) with 
  # lx85 (holding lx for 1-year age groups from 85 to 129)
  # For the intersecting point, corresponding to age 85, we keep
  # the value in lx85, estimated via the method above
  lx70 <- rbind(lx70[-nrow(lx70), , drop = FALSE], lx85)  
  dx70 <- rbind(dx70[-nrow(dx70), , drop = FALSE], dx85)
  
  # Collapse back to 5-year age groups 70-74, 75-79, 80-84 and 
  # 85+, calculating average number of years lived by those who
  # die in each age group 
  # We assume that deaths occur at the midpoint of each 1-year 
  # age group so the number of years lived in the age group
  # 70-74 by someone who dies at age 73 is 3.5; similarly, the 
  # number of years lived in the age group 85+ by someone who dies
  # at age 100 is 15.5, etc.
  # yl is the years lived in the (current) age group at the time of death
  yl <- 70:129 - c(rep(c(70, 75, 80), each = 5), rep(85, length(85:129))) + 0.5
  
  # 5 year age group that each individual age from 70 to 129 belongs to
  x5y <- seq(70, 85, 5)[findInterval(70:129, seq(70, 85, 5))]
  
  ax70 <- as.vector(t(sapply(split(seq(nrow(dx70)), x5y),
                             function(v) colSums(dx70[v, , drop = FALSE] * yl[v]) / 
                               colSums(dx70[v, , drop = FALSE]))
  ))
  ax70
}

#Borrow code for period life table from James

PeriodLifeTable <- function(age, mx, ax, check.conv = FALSE, full.table = FALSE) {
  # Generate a period life table for the supplied inputs
  # age: vector of age groups, 0, 5, 10, ..., 80, 85 (may be repeated
  #     if more than 1 years of data are available)
  # mx: mortality rates corresponding to ages in "age"
  # ax: average number of years lived by those who die in each age group
  #     NA values are replaced by 2.5. The ax for the open-ended age group
  #     85+ is calculated using the Kannisto-Thatcher method [Thatcher et al, 
  #     The survivor ratio method for estimating numbers at high ages, 
  #     Demographic Research (6) 2002]. For age groups 5-9 to 80-84 the ax 
  #     are calibrated using an iterative procedure described on p.47 of 
  #     Preston et al, Demography: measuring and modeling population processes, 
  #     2001. 
  # check.conv: If TRUE, it will test that the iterative procedure to estimate
  #     ax (see above) has converged
  if (length(age) != length(mx) | length(age) != length(ax)) 
    stop("All input vectors must be of same length")
  
  ax[is.na(ax)] <- 2.5
  
  # Remove NA rates (this is useful when eg a given year has no mortality data; 
  # the removed entries are re-inserted as NAs at the end of the function call)
  na.idx <- NULL
  if (any(is.na(mx))) {
    na.idx <- which(is.na(mx))
    stopifnot(age[na.idx] == seq(0, 85, 5))
    age0 <- age
    ax0 <- ax
    mx0 <- mx
    age <- age[-na.idx]
    mx <- mx[-na.idx]
    ax <- ax[-na.idx]
  }
  
  # Replace zero rates by a small number for numerical reasons
  mx[mx == 0] <- 1e-10
  # Probability of dying between age x and x+4
  qx <- 5 * mx / (1 + (5 - ax) * mx)
  # If probability of dying is >1, set it to "almost" 1
  qx[qx > 1] <- 0.99999
  qx[age == 85] <- 1 # by definition
  px <- 1 - qx # probability of surviving to next age group
  lx <- rep(NA, length(px))
  lx[age == 0] <- 100000
  for (k in seq(5, 85, 5)) 
    lx[age == k] <- lx[age == k - 5] * px[age == k - 5]
  dx <- lx * qx	
  ax[age >= 70] <- .KTExtension(matrix(lx[age >= 70], nrow = 4))
  
  num.iter <- 4 # Number of iterations - see Preston et al. p.47
  iter.dat <- vector("list", num.iter + 1)
  iter.dat[[1]] <- list(ax = ax, qx = qx, lx = lx, dx = dx)
  for (i in seq(num.iter)) {
    ax.new <- ax
    for (k in seq(5, 80, 5)) 
      ax.new[age == k] <- (-5 / 24 * dx[age == k - 5] +
                             2.5 * dx[age == k] + 5 / 24 * dx[age == k + 5]) / dx[age == k]
    ax.new[age <= 10 | age >= 70] <- ax[age <= 10 | age >= 70] 
    ax <- ax.new
    qx <- 5 * mx / (1 + (5 - ax) * mx)
    qx[qx > 1] <- 0.99999
    qx[age == 85] <- 1
    px <- 1 - qx
    lx <- rep(NA, length(px))
    lx[age == 0] <- 100000
    for (k in seq(5, 85, 5)) 
      lx[age == k] <- lx[age == k - 5] * px[age == k - 5]
    dx <- lx * qx
    # save result of current iteration
    iter.dat[[i + 1]] <- list(ax = ax, qx = qx, lx = lx, dx = dx)	
  }
  
  if (check.conv) {
    ax.iter <- sapply(iter.dat, `[[`, "ax")
    stopifnot(ax.iter[, num.iter] - ax.iter[, num.iter - 1] < 0.01)
  }
  iter.result <- iter.dat[[num.iter + 1]]
  ax <- iter.result$ax
  qx <- iter.result$qx
  lx <- iter.result$lx
  dx <- iter.result$dx
  
  Lx <- rep(NA, length(age)) 
  for (k in seq(0, 80, 5))
    Lx[age == k] <- 5 * lx[age == k + 5] + ax[age == k] * dx[age == k]
  Lx[age == 85] <- lx[age == 85] / mx[age == 85]
  Tx <- rep(NA, length(age)) 
  Tx[age == 85] <- Lx[age == 85]
  for (k in rev(seq(0, 80, 5)))
    Tx[age == k] <- Tx[age == k + 5] + Lx[age == k]
  ex <- Tx / lx
  
  # Re-insert missing values
  if(!is.null(na.idx)) {
    mx1 <- mx0
    mx1[-na.idx] <- mx
    mx <- mx1
    ax1 <- ax0
    ax1[-na.idx] <- ax
    ax <- ax1	
    age <- age0
    qx1 <- rep(NA, length(mx0))
    qx1[-na.idx] <- qx
    qx <- qx1
    ex1 <- rep(NA, length(mx0))
    ex1[-na.idx] <- ex
    ex <- ex1	
    Tx1 <- rep(NA, length(mx0))
    Tx1[-na.idx] <- Tx
    Tx <- Tx1	
    Lx1 <- rep(NA, length(mx0))
    Lx1[-na.idx] <- Lx
    Lx <- Lx1	
    lx1 <- rep(NA, length(mx0))
    lx1[-na.idx] <- lx
    lx <- lx1	
  }
  if (full.table) return(data.frame(ax = ax, mx = mx, qx = qx, ex = ex, Tx = Tx, Lx = Lx, lx = lx, age = age))
  data.frame(mx = mx, qx = qx, ex = ex)
}

#Create vector of Mx from m_dec1_01

mx <- m_dec1_01$mx


#Try to apply code to generate life table

lt.james <- PeriodLifeTable(mx=mx, age=seq(0,85,5),
                            ax=rep(NA,18),full.table=TRUE)

#Export into csv file
write.csv(lt.james, "lt.james.csv")

# Decomposition -----------------------------------------------------------

#Extract out two life tables
#one for 2001 male data in the bottom deprivation decile, and
#one for the top decile

life_table_m_01_bottom <- life_table %>%
  filter(Year == "2001", Sex == "Male", deprivation_decile == "1")
life_table_m_01_top <- life_table %>%
  filter(Year == "2001", Sex == "Male", deprivation_decile == "10")

#Borrow Bennett et al decomposition function code to decompose LE inequality
#between deciles for men in 2001
#remove Tx variables as not clear why these needed for decomposition

CalculateDiffTable <- function(lt1, lt2) {
  # If rates and sexes are provided, then the life table is calculated using
  # these inputs. Otherwise, the life tables of the two populations, lt1 and lt2, 
  # must be provided. It returns a merged life table for the two populations, 
  # used to calculate the contribution of each age group towards the change in 
  # life expectancy in CalculateAgeContribution()
  stopifnot(lt1$age_grp == lt2$age_grp)
  data.frame(
    age = lt1$age_grp,
    lx1 = (lt1$lx)/100000,
    Lx1 = (lt1$Lx)/100000, 
    lx2 = (lt2$lx)/100000,
    Lx2 = (lt2$Lx)/100000, 
    ex1 = lt1$ex,
    ex2 = lt2$ex)
}			

#Run this code on the two abridged life tables for top
#and bottom deciles of males for 2001

lifetab_agedecomp <- CalculateDiffTable(lt1 = life_table_m_01_top, lt2 = life_table_m_01_bottom)

#See https://www.measureevaluation.org/resources/training/online-courses-and-resources/non-certificate-courses-and-mini-tutorials/multiple-decrement-life-tables/lesson-5
#methodology for calculating age-specific decomposition including direct,
#indirect and total effects

lifetab_agedecomp <- lifetab_agedecomp %>%
  mutate(de #direct effect
         = lx1*((Lx2/lx2)-(Lx1/lx1)),
         ie #indirect effect 
         = (lx1*dplyr::lead(lx2)/lx2-dplyr::lead(lx1))*dplyr::lead(ex2),
         #There are no age categories beyond 90+ so indirect effect =0
         ie=if_else(age %in% c(90), 0, ie),
         te #total effect
         = de+ie
  )

write.csv(lifetab_agedecomp, "decomp.csv")

#Data frame now shows decomposition of life expectancy between 
#deciles 1 and 10 for males in 2001 
#separated by direct effects of deaths in age interval x, 
#indirect effects and total

#Now attempt to undertake age-cause specific decomposition

#Extract out two dataframes with all cause-specific mortality/population data:
#one for 2001 male data in the bottom deprivation decile, and
#one for the top decile
#generate mortality variable for both whilst extracting
#and remove unecessary variables

mort_by_cause_dec1 <- death_popn_long %>%
  filter(Year == "2001", 
         Sex == "Male", 
         deprivation_decile == "1") %>%
  mutate(mx = deaths/popn) %>%
  select(-Year, -Sex, -deprivation_decile, -deaths, -popn) %>%
  rename(mx1 = mx)

#Unclear why for decile 10 the male data include rows of data for breast ca
#whereas for decile 1 they don't, in any case filter out for now
mort_by_cause_dec10 <- death_popn_long %>%
  filter(Year == "2001", 
         Sex == "Male", 
         deprivation_decile == "10") %>%
  mutate(mx = deaths/popn) %>%
  filter(cause_death != "07 Malignant neoplasm of breast") %>%
  select(-Year, -Sex, -deprivation_decile, -deaths, -popn) %>%
  rename(mx2 = mx)


#Create function to combine both dataframes and relabel

cause_decomp <- left_join(mort_by_cause_dec1, 
                          mort_by_cause_dec10, 
                          by = c("cause_death", "ageband"))

#Convert to wide format so causes each have a column

cause_decomp_wide <- cause_decomp %>%
  gather(variable, value, -cause_death, -ageband) %>%
  unite(cause_death_variable, cause_death, variable) %>%
  spread(cause_death_variable, value)

#This is now a data frame in wide format with one column for the mortality rate
#by cause for decile one and another column for decile 10

#Need to merge this data frame with age-specific decomposition data
#with contribution of each ageband towards life expectancy gap between groups

#Remove unecessary variables frmo age decomposition data frame

age_decomp <- lifetab_agedecomp %>%
  select(age, te) %>%
  rename(ageband = age)

cause_age_decomp <- left_join(cause_decomp_wide, age_decomp, by = "ageband")

view(cause_age_decomp)

#Re-examining methodology from measureevaluation.org, need all-cause variable
#for each decile

#Quickest will be to identify all variables with string 'mx1' or 'mx2' and sum each

cause_age_decomp <- cause_age_decomp %>%
  mutate(all_cause_mx1 = rowSums(cause_age_decomp[grep("mx1", 
                                                       names(cause_age_decomp))]), 
         all_cause_mx2 = rowSums(cause_age_decomp[grep("mx2", 
                                                       names(cause_age_decomp))]))

view(cause_age_decomp)

#Calculate differences in rates by cause then contribution of each cause to 
#that ageband of mortality

#See https://stackoverflow.com/questions/32618744/dplyr-how-to-reference-columns-by-column-index-rather-than-column-name-using-mu

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_HPB_neoplasm = .[[2]] - .[[3]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_colorectal_neoplasm = .[[4]] - .[[5]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_digestive_neoplasm = .[[6]] - .[[7]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_haematological_malig = .[[8]] - .[[9]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_lung_neoplasm = .[[10]] - .[[11]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_prostate_neoplasm = .[[12]] - .[[13]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_other_neoplasm = .[[14]] - .[[15]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_resp_disease = .[[16]] - .[[17]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_diabetes = .[[18]] - .[[19]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_ihd = .[[20]] - .[[21]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_cva = .[22] - .[[23]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_other_circ = .[[24]] - .[[25]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_intent_inj = .[[26]] - .[[27]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_unint_inj = .[[28]] - .[[29]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_dementia = .[[30]] - .[[31]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_all_other = .[[32]] - .[[33]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_neonatal = .[[34]] - .[[35]])

cause_age_decomp <- cause_age_decomp %>%
  mutate(diff_all_causes = .[[37]] - .[[38]])

#Now calculate proportionate contribution of each cause

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_hpb_neoplasm = diff_HPB_neoplasm/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_colorectal_neoplasm = diff_colorectal_neoplasm/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_digestive_neoplasm = diff_digestive_neoplasm
/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_haem_malig = diff_haematological_malig/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_lung_neoplasm = diff_lung_neoplasm/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_prostate_neoplasm = diff_prostate_neoplasm/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_other_neoplasm = diff_other_neoplasm/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_resp_disease = diff_resp_disease/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_diabetes = diff_diabetes/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_ihd = diff_ihd/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_other_circ = diff_other_circ/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_intent_inj = diff_intent_inj/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_unintent_inj = diff_unint_inj/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_dementia = diff_dementia/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_all_other = diff_all_other/diff_all_causes)

cause_age_decomp <- cause_age_decomp %>%
  mutate(prop_con_neonatal = diff_neonatal/diff_all_causes)

#Next calculate contribution to life expectancy by age:cause

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_hpb_neoplasm = prop_con_hpb_neoplasm*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_colorectal_neoplasm = prop_con_colorectal_neoplasm*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_digestive_neoplasm = prop_con_digestive_neoplasm*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_haem_malig = prop_con_haem_malig*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_lung_neoplasm = prop_con_lung_neoplasm*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_prostate_neoplasm = prop_con_prostate_neoplasm*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_other_neoplasm = prop_con_other_neoplasm*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_resp_disease = prop_con_resp_disease*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_diabetes = prop_con_diabetes*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_ihd = prop_con_ihd*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_other_circ = prop_con_other_circ*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_inten_inj = prop_con_intent_inj*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_unintent_inj = prop_con_unintent_inj*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_dementia = prop_con_dementia*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_all_other = prop_con_all_other*te)

cause_age_decomp <- cause_age_decomp %>%
  mutate(contrib_neonatal = prop_con_neonatal*te)

#View Age-Cause-Specific Contributions in Life Expectancy at birth

view_decomp <- cause_age_decomp %>%
  select(ageband, te, contrib_hpb_neoplasm, contrib_colorectal_neoplasm, 
         contrib_digestive_neoplasm, contrib_haem_malig, 
         contrib_lung_neoplasm, contrib_prostate_neoplasm, 
         contrib_other_neoplasm, contrib_resp_disease, 
         contrib_diabetes, contrib_ihd, contrib_other_circ, 
         contrib_inten_inj, contrib_unintent_inj, 
         contrib_dementia, contrib_all_other, contrib_neonatal)

view(view_decomp)

#Save in CSV file for review
write.csv(view_decomp, "age-cause-decomp-male-le-2001.csv")

#This method has produced breakdown of contribution to LE gap by age and cause
#there is a small difference (0.12 years) between the total gap and sum of the 
#age-specific decomposition - may be due to rounding in Excel?

#Try and visualise - first need to convert this dataframe into long

view_decomp_long <- view_decomp %>%
  gather(cause, contribution, contrib_hpb_neoplasm:contrib_neonatal, 
         factor_key = TRUE)

#Recode causes

levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_hpb_neoplasm"] <- "hpb_neoplasm"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_colorectal_neoplasm"] <- "colorectal_neoplasm"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_digestive_neoplasm"] <- "digestive_neoplasm"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_haem_malig"] <- "haem_malig"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_lung_neoplasm"] <- "lung_neoplasm"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_prostate_neoplasm"] <- "prostate_neoplasm"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_other_neoplasm"] <- "other_neoplasm"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_resp_disease"] <- "resp_disease"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_diabetes"] <- "diabetes"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_ihd"] <- "ihd"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_other_circ"] <- "other_circ"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_intent_inj"] <- "intent_inj"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_unintent_inj"] <- "unintent_inj"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_dementia"] <- "dementia"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_all_other"] <- "all_other"
levels(view_decomp_long$cause)[levels(view_decomp_long$cause)=="contrib_neonatal"] <- "neonatal"

#Visualise using geom_tile
heat_map <- view_decomp_long %>%
  ggplot(aes(x=as.numeric(as.character(ageband)), y=cause)) + 
  geom_tile(aes(fill = contribution), width = 5) + 
  theme_bw() + theme(panel.border = element_blank(),panel.grid = element_blank()) + 
  scale_fill_gradient2(low = "darkgreen", high = "darkred", mid = "white",
                       midpoint = 0,name="Contribution to\n change\nin e0 (years)") + 
  theme_bw() + theme(panel.border = element_blank(),panel.grid = element_blank(), 
                     legend.position = "right",
                     strip.background = element_blank()) + 
  labs(x = "", y = "") + coord_fixed() + 
  theme(axis.text.x=element_text(angle=65,hjust=1)) + 
  scale_x_continuous(breaks = c(0,1,seq(5,90,5)), 
                   labels = c(0,1,seq(5,90,5))) + 
  labs(title = "Age-Cause-Specific Decomposition of Gap in Life Expectancy", 
       subtitle = "between decile 1 and 10 for men in Wales in 2001")

grid.arrange(heat_map,ncol=1)
heat_map





