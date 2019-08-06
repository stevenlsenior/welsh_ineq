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

data_esp %>%
  filter()

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
#######################

#Now calculate life expectancy using life table methods drawing on PHE's Indicator Methods methodology
#see https://rdrr.io/cran/PHEindicatormethods/src/R/LifeExpectancy.R

#Amend aggregated death/population dataframe to remove 'all' age categories and create ordered deprivation deciles
le_data1 <- deaths_popns %>%
  filter(age_grp !="All")
le_data1$deprivation_decile <- factor(as.character(le_data1$deprivation_decile),
                                        levels = c("1", "2", "3", "4", "5", "6", "7", 
                                                   "8", "9", "10"))
le_data1$age_grp <- as.numeric(as.character(le_data1$age_grp))

#Generate life table using Chiang and Silcocks methodology
#First using Chiang II methodology from ONS (leave standard errors/variances/CIs etc for now)
#See methodology at:
#https://webarchive.nationalarchives.gov.uk/20110824183840/http://www.statistics.gov.uk/statbase/ssdataset.asp?vlnk=6949

life_table <- le_data1 %>%
  group_by(Year, Sex, deprivation_decile) %>%
  mutate(ax = if_else(age_grp %in% c("0"), 0.1, 0.5),
         Mx = deaths/popn, 
         n = if_else(age_grp <90, dplyr::lead(age_grp)-age_grp, 2/Mx), 
         qx = if_else(age_grp <90, (n*Mx)/(1+n*(1-ax)*Mx), 1),
         px = 1-qx,
         lx = if_else(age_grp %in% c("0"), 100000, 1),
         px_lag = dplyr::lag(px, default = 1),
         lx = if_else(age_grp >0, cumprod(lx*px_lag), lx),
         dx = lx - dplyr::lead(lx, default = 0),
         Lx = if_else(age_grp <90, n*(dplyr::lead(lx)+(ax*dx)), lx/Mx),
         Tx = rev(cumsum(rev(Lx))),
         ex = Tx/Lx)

#This gives LEs approximately equal to the PHEindicatormethods package
#Will need to explore why differences at some point

#Extract out two life tables, one for 2001 male data in the bottom deprivation decile, and one for the top decile

life_table_m_01_bottom <- life_table %>%
  filter(Year == "2001", Sex == "Male", deprivation_decile == "1")
life_table_m_01_top <- life_table %>%
  filter(Year == "2001", Sex == "Male", deprivation_decile == "10")

#Borrow Bennett et al decomposition function code to decompose LE inequality between deciles for men in 2001         

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
    Tx1 = lt1$Tx, 
    lx2 = (lt2$lx)/100000,
    Lx2 = (lt2$Lx)/100000, 
    Tx2 = lt2$Tx,
    ex1 = lt1$ex,
    ex2 = lt2$ex)
}			

#Run this code on the two abridged life tables for top and bottom deciles of males for 2001

lifetab_agedecomp <- CalculateDiffTable(lt1 = life_table_m_01_top, lt2 = life_table_m_01_bottom)

#See https://www.measureevaluation.org/resources/training/online-courses-and-resources/non-certificate-courses-and-mini-tutorials/multiple-decrement-life-tables/lesson-5
#methodology for calculating age-specific decomposition including direct, indirect and total effects

lifetab_agedecomp <- lifetab_agedecomp %>%
  mutate(de = lx1*((Lx2/lx2)-(Lx1/lx1)),
         ie = lx1*(dplyr::lead(lx2)/lx2)-dplyr::lead(lx1)*dplyr::lead(ex2)
  )

#Credit to Sebastian Fox and PHEindicatormethods package for following amended code
#see https://rdrr.io/cran/PHEindicatormethods/src/R/LifeExpectancy.R

#Methodology for life tables originally from Chiang at https://apps.who.int/iris/handle/10665/62916

#Mortality data has to be arranged by ascending age category with 20 age bands to fit with the age_contents rule
#assumes there are no missing variables, minimum age is 0 and each population category has a population >5000

le_data1 <- le_data1 %>%
  arrange(age_grp)

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

  
le <- le_data1 %>%
  group_by(Year, Sex, deprivation_decile) %>%
  life_tab(deaths, popn, age_grp)

#Compare to PHE function

le_phe <- le_data1 %>%
  group_by(Year, Sex, deprivation_decile) %>%
  phe_life_expectancy(deaths, popn, startage, le_age = 0)
  
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












