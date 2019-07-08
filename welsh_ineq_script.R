#This project is an exploratory data analysis of mortality statistics from Wales 2001-2017

#We begin by looking at mortality data from the most deprived 10% lower-super output areas (LSOAs) and least deprived 10% LSOAs by Welsh IMD 2014 scores, broken down by age, gender and International Classification of Disease-10 (ICD-10) category and exploring trends in mortality rates and life expectancy over this period

#Note: deaths have been categorised into group causes - see paper for full methodology

#Load packages
library(sjmisc)
library(tidyverse)
library(readxl)
library(PHEindicatormethods)
library(LifeTables)

#Load data
deaths_01_16 <- read_excel("data_1981-2016.xls", sheet = 6, col_names = TRUE)
pop_01_16 <- read_excel("data_1981-2016.xls", sheet = 8, col_names = TRUE)
deaths_17 <- read_excel("data_2017.xls", sheet = 4, col_names = TRUE)
pop_17 <- read_excel("data_2017.xls", sheet = 6, col_names = TRUE)

#Inspect new dataframes
head(deaths_01_16)
glimpse(deaths_01_16)

#First several rows of either dataset are redundant, therefore need to skip rows

#Reload data skipping unecessary data rows
deaths_01_16 <- read_excel("data_1981-2016.xls", sheet = 6, skip = 14, col_names = TRUE)

#Reinspect dataframe
head(deaths_01_16)
glimpse(deaths_01_16)

#Better dataframe, therefore load all datasets again using this function
pop_01_16 <- read_excel("data_1981-2016.xls", sheet = 8, col_names = TRUE, range = cell_rows(4:324))
deaths_17 <- read_excel("data_2017.xls", sheet = 4, skip = 12, col_names = TRUE)
pop_17 <- read_excel("data_2017.xls", sheet = 6, skip = 12, col_names = TRUE)

#Now merge mortality and population dataframes before then merging into one single dataframe with mortality and population data for 2001-16
#Write dataframes to csv file for inspection

write.csv(deaths_01_16, "deaths.01.16.csv")
write.csv(pop_01_16, "pop.01.16.csv")
write.csv(deaths_17, "deaths_17.csv")
write.csv(pop_17, "pop.17.csv")

#Rename columns in mortality dataframe with deaths_ prefix and in population dataframe popn_

colnames(deaths_01_16)[5:25] <- paste("deaths_", colnames(deaths_01_16)[5:25], sep = "")
colnames(pop_01_16)[4:24] <- paste("popn_", colnames(pop_01_16)[4:24], sep="")

colnames(deaths_17)[5:25] <- paste("deaths_", colnames(deaths_17)[5:25], sep="")
colnames(pop_17)[4:24] <- paste("popn_", colnames(pop_17)[4:24], sep="")

#Merge mortality dataframes

deaths <- rbind(deaths_01_16, deaths_17)

#Last column of both dataframes differ, therefore change variable names to match

colnames(deaths_17)[25] <- "deaths_All Ages"

deaths <- rbind(deaths_01_16, deaths_17)

#Write to csv file to inspect after ordering by year
deaths <- deaths %>% arrange(desc(Year))
write.csv(deaths, "deaths.csv")

#Merge population dataframes and inspect in csv after ordering

popns <- rbind(pop_01_16, pop_17)
popns <- popns %>% arrange(desc(Year))
write.csv(popns, "popns.csv")

#Alter column names in dataframes for deprivation and cause of death to make identical

colnames(deaths)[2] <- "deprivation_decile"
colnames(popns)[2] <- "deprivation_decile"
colnames(deaths)[4] <- "cause_death"

#Merge mortality and population dataframes 

death_popn <- inner_join(deaths, popns, by = c("Year", "deprivation_decile", "Sex"))

#Inspect dataframe in csv format
write.csv(death_popn, "death.popns.csv")

#In fact given there are more levels in the structure of the mortality data (multiple categories of mortality) than in the population data (one population datapoint for each deprivation decile, gender and year) it probably makes more sense to merge mortality and population data after converting the datasets to long format

#Generate new dataset with aggregated mortality for all causes

agg_deaths <- deaths %>% select(-cause_death) %>% group_by(Year, deprivation_decile, Sex) %>% summarise_all(funs(sum))

#Convert datasets from wide to long format

names(agg_deaths) <- make.names(names(agg_deaths), unique = TRUE)
agg_deaths$Year <- as.factor(as.character(agg_deaths$Year))
agg_deaths$Sex <- as.factor(as.character(agg_deaths$Sex))
agg_deaths$deprivation_decile <- as.factor(as.character(agg_deaths$deprivation_decile))

#Inspect new dataframe
head(agg_deaths)
glimpse(agg_deaths)
agg_deaths <- agg_deaths %>% arrange(desc(Year, Sex))
write.csv(agg_deaths, "agg_deaths.csv")

agg_deaths_long <- gather(agg_deaths, age_grp, deaths, deaths_.1:deaths_All.Ages, factor_key = FALSE)

unique(agg_deaths_long$age_grp)

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

#Save deaths in long format to csv
write.csv(agg_deaths_long, "agg_deaths_long.csv")

#Convert population dataset into long format

names(popns) <- make.names(names(popns), unique = TRUE)
popns$Year <- as.factor(as.character(popns$Year))
popns$Sex <- as.factor(as.character(popns$Sex))
popns$deprivation_decile <- as.factor(as.character(popns$deprivation_decile))

agg_popns_long <- gather(popns, age_grp, popn, popn_.1:popn_All.Ages, factor_key = FALSE)

unique(agg_popns_long$age_grp)

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

#Reinspect population dataframe
head(agg_popns_long)
glimpse(agg_popns_long)
write.csv(agg_popns_long, "agg_popns_long.csv")

#Merge mortality and population dataframes after filtering mortality data before 2001

agg_deaths_long$Year <- as.numeric(as.character(agg_deaths_long$Year))
agg_deaths_long <- agg_deaths_long %>% filter(Year>2000)
agg_popns_long$Year <- as.numeric(as.character(agg_popns_long$Year))

deaths_popns <- left_join(agg_deaths_long, agg_popns_long, by = c("Year", "deprivation_decile", "Sex", "age_grp"))

#Inspect new merged dataframe
head(deaths_popns)
glimpse(deaths_popns)
write.csv(deaths_popns, "deaths_popns.csv")

#Generate crude-death rates for deciles 1 and 10 for men and women - filter data

crude_data <- deaths_popns %>% filter(Year %in% (2001:2017), deprivation_decile == 1 | deprivation_decile == 10, age_grp == "All")

#Order dataset

crude_data <- crude_data %>% arrange(Year, deprivation_decile, Sex)

#Calculate age-specific mortality rates 

crude_data <- crude_data %>% mutate(crude_mort = (deaths/popn)*100000)

crude_data %>% filter(Sex == "Male") %>%
  ggplot(aes(x=Year, y=crude_mort)) + geom_line(aes(colour=deprivation_decile))

crude_data %>% filter(Sex == "Female") %>%
  ggplot(aes(x=Year, y=crude_mort)) + geom_line(aes(colour=deprivation_decile))

#Now create age-standardised mortality rates using European Standard Population (ESP2013) by gender and cause of death for top 10% and bottom 10% deciles of deprivation - using dsr calculation from PHE Indicator Methods package

#First filter data to get just top and bottom decile and remove aggregated deaths/populations for deciles

dec1_dec10 <- deaths_popns %>% filter(deprivation_decile == "1" | deprivation_decile == "10") %>% filter(age_grp != "All")

#Need to collapse data for age-bands 0 and 1 (0-1 and 1-4) into one age band to match ESP2013 standard population

under_five <- dec1_dec10 %>% filter(age_grp == "0" | age_grp == "1")

under_five <- under_five %>% select(-age_grp) %>% group_by(Year, deprivation_decile, Sex) %>%
  summarise_all(funs(sum))
nrow(under_five)
under_five$age_grp <- c(rep(0, 68))

dec1_dec10_5_plus <- dec1_dec10 %>% filter(!age_grp %in% c(0,1))
dec1_dec10_5_plus$age_grp <- as.numeric(dec1_dec10_5_plus$age_grp)
dec1_dec10_v2 <- rbind(dec1_dec10_5_plus, under_five)

dec1_dec10_v2 <- dec1_dec10_v2 %>% filter(age_grp != "NA")

#Need to arrange data by age-band and rename columns

dec1_dec10_v2 <- dec1_dec10_v2 %>% rename(ageband = age_grp)
dec1_dec10_v2 <- dec1_dec10_v2 %>% arrange(ageband)

dec1_dec10_v2_m <- dec1_dec10_v2 %>% filter(Sex == "Male")
dec1_dec10_v2_f <- dec1_dec10_v2 %>% filter(Sex == "Female")

#Create graphs using phe_dsr from PHE indocator methods package and ggplot of trends in direct age-standardised mortality rates for deciles 1 and 10 for men and women

phe_dsr(dec1_dec10_v2_m, deaths, popn) %>% filter(deprivation_decile == "1" | deprivation_decile == "10") %>% ggplot(aes(x=Year, y=value)) + geom_line(aes(colour=deprivation_decile)) 

phe_dsr(dec1_dec10_v2_f, deaths, popn) %>% filter(deprivation_decile == "1" | deprivation_decile == "10") %>% ggplot(aes(x=Year, y=value)) + geom_line(aes(colour=deprivation_decile))

#Now using this approach go back to original dataset earlier in analysis
#and generate cause-specific age-adjusted mortality rates for each decile 
#and gender during the period 2001-2017

#Re-inspect dataframe with data on all causes of mortality

glimpse(death_popn)

#This dataset needs to be converted from wide to long format
#with aggregated deaths (for all causes combined)
#also generated to allow graphs of trends in cause-specific
#and all-cause mortality to be calculated

names(death_popn) <- make.names(names(death_popn), unique = TRUE)
glimpse(death_popn)

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

#Inspect new dataframe
glimpse(death_popn_long)
write.csv(death_popn_long, "death_popn_long.csv")

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
#and aggregate deaths and populations in 0-1 and 1-4 age bands and

death_popn_long <- death_popn_long %>% filter(ageband != "All")
death_popn_long$ageband <- as.numeric(death_popn_long$ageband)

test <- death_popn_long %>% 
  mutate(new_ageband = if_else(ageband %in% c("0", "1"), 0, ageband)) %>%
  group_by(Year, deprivation_decile, Sex, cause_death, new_ageband) %>%
  summarise(deaths = sum(deaths))

#Convert ageband to numeric variable to avoid error

death_popn_long$ageband <- as.numeric(as.character(death_popn_long$ageband))

#Re-attempt merge

test <- death_popn_long %>% 
  mutate(new_ageband = if_else(ageband %in% c("0", "1"), 0, ageband)) %>%
  group_by(Year, deprivation_decile, Sex, cause_death, new_ageband) %>%
  summarise(deaths = sum(deaths), popn = sum(popn))

#Inspect new dataframe and compare to previous
head(test)
glimpse(test)
write.csv(test, "agg_agebands.csv")
write.csv(death_popn_long, "non_agg_agebands.csv")

#This appears to work therefore use this code to prepare and create
#data for age-adjusted mortality rates by gender, cause, year and WIMD decile

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
  filter(deprivation_decile == "1" | deprivation_decile == "10") %>%
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

unique(agg_data_m$cause_death)

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

unique(agg_data_f$cause_death)

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

#Try to manually calculate age-adjusted mortality rates

#First obtain European Standard Population (2013)

esp2013
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

#Try graphs

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
       subtitle = "Males",
       y = "Age-adjusted mortality rate per 100,000 people")

#Now calculate life expectancy by year, gender, cause of death and dep_decile 

test <- death_popn_long %>%
  group_by(Year, Sex, deprivation_decile, cause_death) %>%
  phe_life_expectancy(deaths = deaths, popn, startage = ageband)

#Try different approach

x <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90)
nDx <- death_popn_long$deaths
nKx <- death_popn_long$popn
nMx <- nDx/nKx

life.table <- function(x, nMx){
  b0 <- 0.07; b1 <- 1.7;
  nmax <- Length(x);
  n <- c(diff(x),999);
  nax <- n / 2;
  nax[1] <- b0 + b1 * nMx[1];
  nax[2] <- 1.5  ;              
  nax[nmax] <- 1/nMx[nmax]; 	  # e_x at open age interval
  nqx <- (n*nMx) / (1 + (n-nax)*nMx);
  nqx<-ifelse( nqx > 1, 1, nqx); # necessary for high nMx
  nqx[nmax] <- 1.0;
  lx <- c(1,cumprod(1-nqx)) ;  # survivorship lx
  lx <- lx[1:length(nMx)];
  ndx <- lx * nqx ;
  nLx <- n*lx - nax*ndx;       # equivalent to n*l(x+n) + (n-nax)*ndx
  nLx[nmax] <- lx[nmax]*nax[nmax];
  Tx <- rev(cumsum(rev(nLx)));
  ex <- ifelse( lx[1:nmax] > 0, Tx/lx[1:nmax] , NA);
  lt <- data.frame(x=x,nax=nax,nmx=nMx,nqx=nqx,lx=lx,ndx=ndx,nLx=nLx,Tx=Tx,ex=ex);
  return(lt)
}

test <- death_popn_long %>%
  group_by(Year, Sex, deprivation_decile, cause_death) %>%
  life.table()

#Try another

death_popn_long <-
  death_popn_long %>% mutate(crude_rate = deaths/popn)

test <- death_popn_long %>%
  filter(Sex == "Male") %>%
  group_by(Year, deprivation_decile, cause_death) %>%
  lt.mx(crude_rate, age = c(0,1,seq(5,90,5)))

#The data should not be disaggregated by cause of death
#so aggregate on this variable

deaths_sum <- death_popn_long %>%
  select(-crude_rate) %>%
  group_by(Year, deprivation_decile, Sex, ageband) %>%
  summarise(deaths = sum(deaths))

#Merge with population data again
agg_popns_long <- agg_popns_long %>%
  rename(ageband = age_grp)

agg_popns_long$deprivation_decile <- as.numeric(as.character(agg_popns_long$deprivation_decile))
agg_popns_long$Sex <- as.character(agg_popns_long$Sex)
agg_popns_long$ageband <- as.numeric(agg_popns_long$ageband)

deaths_sum <- inner_join(deaths_sum, agg_popns_long, 
                        by = c("Year", "Sex", "deprivation_decile", "ageband"))

test <- deaths_sum %>%
  group_by(Year, Sex, deprivation_decile) %>%
  phe_life_expectancy(deaths = deaths, popn, startage = ageband)

#This appears to work, create graphs of life expectancy at birth

deaths_sum$deprivation_decile <- factor(as.character(deaths_sum$deprivation_decile),
                                        levels = c("1", "2", "3", "4", "5", "6", "7", 
                                                   "8", "9", "10"))

deaths_sum %>%
  group_by(Year, Sex, deprivation_decile) %>%
  phe_life_expectancy(deaths = deaths, popn, startage = ageband) %>%
  filter(Sex == "Male" & ageband == "0") %>%
  ggplot(aes(x=Year, y=value, colour = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  labs(title = "Trends in life expectancy at birth by deprivation decile",
       subtitle = "Males",
       y = "Life expectancy at birth in years")

deaths_sum %>%
  group_by(Year, Sex, deprivation_decile) %>%
  phe_life_expectancy(deaths = deaths, popn, startage = ageband) %>%
  filter(Sex == "Female" & ageband == "0") %>%
  ggplot(aes(x=Year, y=value, colour = deprivation_decile)) + 
  geom_line(aes(group = deprivation_decile)) +
  labs(title = "Trends in life expectancy at birth by deprivation decile",
       subtitle = "Females",
       y = "Life expectancy at birth in years")

#===

test <- deaths_sum %>% group_by(Year, deprivation_decile, Sex, ageband) %>%
  mutate(age_spec_rate = deaths/popn)

life_tab_m <- test %>%
  filter(Sex == "M") %>%
  group_by(Year, deprivation_decile) %>%
  lt.mx(nmx = age_spec_rate, sex = "male", age = c(0,1,seq(5,90,5)), nax=NULL)

#===

test <- deaths_popns %>% filter(Year == "2017" & deprivation_decile == "1" & Sex == "Male" & age_grp != "All")
test$popn <- as.integer(test$popn)

test %>% group_by(Year, deprivation_decile) %>% phe_life_expectancy(test, deaths, popn, age_grp, age_contents = 
                      c("0", "1", seq(5, 90, 5)))

test %>% phe_life_expectancy(deaths, popn, age_grp, age_contents = 
                        c("0", "1", seq(5, 90, 5)))

#Filter 2017 male data for decile 1 for data wrangling to get code to generate LE

m17_dec1 <- deaths_popns %>% filter(Year == 2017, deprivation_decile == 1, Sex == "Male") %>% ungroup() %>% select(-Year, -deprivation_decile, -Sex)

#Inspect new dataset

head(m17_dec1)

#Create crude mortality rate variable

m17_dec1 <- m17_dec1 %>% mutate(crude.death.rate = deaths/popn)

#Create new test dataset to test out data manipulations for calculating life expectancy

test <- m17_dec1

#Alter crude mortality rate variable to be called "qx" according to standard nomenclature and age group to be described as "x"

colnames(test)[1] <- "x"
colnames(test)[4] <- "mx"

#First remove last row (all deaths)

test <- test %>% filter(!x == "All")

#Remove observed deaths and population count from test dataframe

test <- test %>% select(-deaths, -popn)

#Generate life tables using R package

#Filter data for both genders

test2 <- deaths_popns %>% filter(Year == 2017, deprivation_decile == 1, Sex == "Male") %>% ungroup() %>% select(-Year, -deprivation_decile)

#calculate crude mortality rates

test2 <- test2 %>% mutate(mx = deaths/popn)

#Rename age column

colnames(test2)[2] <- "x"
test2 <- test2 %>% filter(!x == "All")

test2$x <- as.double(test2$x)
test2$Sex <- as.character(test2$Sex)
test2 <- test2 %>% select(-deaths, -popn, -Sex)

library(LifeTables)

lt <- lt.mx(test2, sex = "male", age=c(0,1,seq(10,90,5)))

#Try creating lx and dx variables from ONS methodology

n <- nrow(test)

dx <- lx <- numeric(n)

dx[1] <- 100000 * test$mx[1]

lx[1] <- 100000

for(i in seq_len(n)[-1]){
  lx[i] <- lx[i-1] - dx[i-1]
  dx[i] <- lx[i] * test$mx[i]
}

#Now merge with test dataframe

test <- cbind(test, lx, dx)

#Create variable Lx describing number of person years lived at each individual age

test <- test %>% mutate(Lx = (lx+lead(lx))/2)

#Amend final value to be 0

test <- test %>% mutate(Lx = replace(Lx, 20, 0))

#Try to create life expectancy at age x (ex) variable

test <- test %>% mutate(ex = rev(cumsum(rev(Lx)))/lx)


#Generate survival variable "lx" describing survival from initial standard 100,000 cohort in any age category

test <- test %>% mutate(lx = 100000-(cumsum(lag(deaths, default = 0))))

#Inspect new variable
head(test)

#This seems to have worked so now need to add all other variables

test <- test %>% mutate(
  ax = c(rep(0.5)), #Fraction lived
  qx = mx/(1+(1-ax)*mx), #Probability of dying between ages x and x+1
  px = 1-qx, #Probability of surviving between ages x and x+1
  dx  = lx*qx, #Numbers dying
  Lx = lx-(1-ax)*dx, #Numbers alive between ages
  Tx = rev(cumsum(rev(Lx)))
  )

#Perform some conditional calculations for row 90

test$ax[20] <- 1/test$mx[20]
test$qx[20] <- 1 #Last element of qx is 1
test$Lx[20] <- test$ax[20]*test$dx[20]

#Finally create life expectancy variable for the data frame
test <- test %>% mutate(ex = Tx/lx)

ex = Tx/lx

#Copied function from 'Life Tables and R Programming'
life.table <- function(age_grp, crude.death.rate){
  b0 <- 0.07; b1<- 1.7;
  n <- c(diff(age_grp),999)
  nax <- n/2;
  nax[1] <- b0 + b1 *crude.death.rate[1]
  nax[2] <- 1.5 ;
  nax[nmax] <- 1/crude.death.rate[nmax]
  nqx <- (n*crude.death.rate) / (1 + (n-nax)*crude.death.rate)
  nqx <- ifelse( nqx >1, 1, nqx);
  nqx[nmax] <- 1.0
  lx <- c(1,cumprod(1-nqx));
  lx <- lx[1:Length(crude.death.rate)]
  ndx <- lx * nqx;
  nLx<- n*lx - nax*ndx;
  nLx[nmax] <- lx[nmax]*nax[nmax]
  Tx <- rev(cumsum(rev(nLx)))
ex <- ifelse(lx[1:nmax] > 0, Tx/lx[1:nmax], NA);
lt <- data.frame(x=x,nax=nax,crude.death.rate=crude.death.rate,nqx=nqx,lx=lx,ndx=ndx,nLx=nLx,Tx=Tx,ex=ex)
return(lt)
}

x <- 

life.table(m17_dec1)











