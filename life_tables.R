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


          
