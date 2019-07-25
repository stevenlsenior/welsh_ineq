#### Using PHE life tables function ####

# Source Jonny's script

source("welsh_ineq_script_UPDATED_040719.R")

# Need data frame with deaths and populations by age group, deprivation decile, sex, and year

n <- length(unique(death_popn_long$cause_death))

d <- death_popn_long %>%
     group_by(Year,
              deprivation_decile,
              Sex, 
              ageband) %>%
     summarise(deaths = sum(deaths),
               popn = median(popn)) # median because each c.o.d. has same population

View(d)

# Test with 2001, female, decile 1 data (LE at birth)

t <- phe_life_expectancy(data = filter(d,
                                       Year == 2001,
                                       Sex == "Female",
                                       deprivation_decile == 1),
                         deaths = deaths,
                         population = popn,
                         startage = ageband,
                         le_age = 0)

View(t)

# Set up empty data frame

life_exp <- t[FALSE,]

# Use a for loop to get LE at birth for each deprivation decile, year, and sex

for(i in 2001:2017){
  for(j in 1:10){
    for(k in c("Female", "Male")){
      t <- phe_life_expectancy(data = filter(d,
                                             Year == i,
                                             deprivation_decile == j,
                                             Sex == k),
                               deaths = deaths,
                               population = popn,
                               startage = ageband,
                               le_age = 0)
      
      life_exp <- rbind(life_exp, t)
    }
  }
}

# Calculate gap measure over time for women and men

le_gap_f <- life_exp %>%
            filter(deprivation_decile %in% c(1, 10),
                   Sex == "Female") %>%
            select(year = Year,
                   deprivation_decile,
                   le_at_birth = value) %>%
            spread(key = deprivation_decile,
                   value = le_at_birth) %>%
            mutate(gap = `10` - `1`)

le_gap_m <- life_exp %>%
            filter(deprivation_decile %in% c(1, 10),
                   Sex == "Male") %>%
            select(year = Year,
                   deprivation_decile,
            le_at_birth = value) %>%
            spread(key = deprivation_decile,
                   value = le_at_birth) %>%
            mutate(gap = `10` - `1`)

#Create a function to generate life table using Chiang and Silcocks methodology
#Credit to Sebastian Fox and PHEindicatormethods package for amended code
#see https://rdrr.io/cran/PHEindicatormethods/src/R/LifeExpectancy.R

#Methodology for life tables originally from Chiang at https://apps.who.int/iris/handle/10665/62916

#Mortality data has to be arranged by ascending age category with 20 age bands to fit with the age_contents rule
#assumes there are no missing variables, minimum age is 0 and each population category has a population >5000

le_data1 <- deaths_popns %>%
filter(age_grp != "All") %>%
  arrange(age_grp)
le_data1$deprivation_decile <- factor(as.character(le_data1$deprivation_decile),
                                        levels = c("1", "2", "3", "4", "5", "6", "7", 
                                                   "8", "9", "10"))
le_data1$age_grp <- as.numeric(as.character(le_data1$age_grp))

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

#Run this function on mortality data to produce LE data

le <- le_data1 %>%
group_by(Year, Sex, deprivation_decile) %>%
  life_tab(deaths, popn, age_grp)

#Display life expectancy data by year, deprivation decile and gender

le %>% select(Year, deprivation_decile, Sex, ei)

#LEs appear to be several years less than that calculated using the phe_life_expectancy function?



          
