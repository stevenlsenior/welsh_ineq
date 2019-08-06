#This by James Bennet 2018 trying to repeat Sam Harper's blog and to understand Arriaga's method of decomposition
# http://samharper.org/new-blog/tag/decomposition
# Note that our period life table is set for 18 age groups so I had to adjust his death rates to have the same number of age groups

#Arriaga's paper here http://www.academia.edu/14523056/Measuring_and_Explaining_the_Change_in_Life_Expectancies


library(INLA);library(ggplot2); library(data.table); library(Hmisc); library(plyr); library(doBy); library(dplyr)  ; 
library(GGally); library(reshape2); library(RColorBrewer); library(gridExtra); library(MASS);library(stats4); 
library(base);library(scales); library(truncnorm);library(tensorA); library(grid)

source("S:\\Projects\\Disease_trends\\FC II\\mortality_forecast\\src/period_life_table.R")


f <- function(pal) brewer.pal(brewer.pal.info[pal, "maxcolors"], pal)
mycols <- c(f("Dark2"), f("Set1")[1:8], f("Set2"), f("Set3"),"#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921",
            "#C0717C", "#CBD588", "#5F7FC7", "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030",
            "#AD6F3B", "#CD9BCD", "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F",
            "#D1A33D", "#8A7C64", "#599861" )

plot(1:length(mycols),col=mycols[1:length(mycols)],pch=16,cex=3)

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

########################################################
# This file contains the functions used to calculate the contribution of each age 
# group to a difference in life expectancy at birth between two populations. The 
# method used is due to Arriaga (1984) and is detailed in Preston pp 64-65. 
# 
CalculateDiffTable <- function(lt1, lt2) {
  # If rates and sexes are provided, then the life table is calculated using
  # these inputs. Otherwise, the life tables of the two populations, lt1 and lt2, 
  # must be provided. It returns a merged life table for the two populations, 
  # used to calculate the contribution of each age group towards the change in 
  # life expectancy in CalculateAgeContribution()
  stopifnot(lt1$age == lt2$age)
  data.frame(
    age = lt1$age,
    lx1 = lt1$lx,
    Lx1 = lt1$Lx, 
    Tx1 = lt1$Tx, 
    lx2 = lt2$lx,
    Lx2 = lt2$Lx, 
    Tx2 = lt2$Tx,
    ex1 = lt1$ex,
    ex2 = lt2$ex)
}					
CalculateAgeContribution <- function(dtbl) {
  # See Preston pp64-65 for details of calculations
  N <- nrow(dtbl)
  
  #this first line is de (direct effect) in Harper version
  dtbl$Dx <- with(dtbl, lx1 / lx1[1] * (Lx2 / lx2 - Lx1 / lx1) + 
                    ##diff(x) gives the difference between x[i] and x[i-1],
                    #so here it is differences in the ratio lx1/lx2 with NA as the last
                    c(Tx2[2:N], NA) / lx1[1] * c(-diff(lx1 / lx2), NA))
  
  #this for final ag group only (Nth age group) just the direct effect (de) 
  #does not look consistent with the de equation above but actually Tx[N] and Lx[N] are equal
  dtbl$Dx[N] <- with(dtbl, 
                     lx1[N] / lx1[1] * (Tx2[N] / lx2[N] - Tx1[N] / lx1[N]))
  
  # work out % by age group of total difference sum of Dx should be difference in e0 between 2 populations
  dtbl$percent <- dtbl$Dx / sum(dtbl$Dx) * 100
  dtbl
}
DecomposeLifeExpDiff <- function(lt1, lt2) {
  # Takes either mortality rates and sexes for two populations (mx1, sex1, 
  # mx2, sex2) or life tables for two populations (lt1, lt2) and returns
  # a data frame with the % contribution of each age group towards the change
  # in life expectancy at birth from population 1 to population 2
  dtbl <- CalculateDiffTable(lt1, lt2)
  CalculateAgeContribution(dtbl)					
}


########################################
#mxE1 <- exp(-5.5 + c(-1,-3,-2.9,-1.8,-1.7,-1.7,-1.6,-1.4,-.8,-.2,.3,1.0,1.5,1.9,2.4,2.8,3.4,3.8))
#mxE2 <- exp(-5.5 + c(-1,-3,-2.2,-1.9,-1.7,-1.7,-1.6,-1.4,-.8,-.2,.3,1.0,1.5,1.9,2.4,2.8,3.4,3.8))
#function to give table
#lt1 <- PeriodLifeTable(mx=mxE1,age=seq(0, 85, 5),ax=rep(NA,18),full.table=TRUE)
#lt2 <- PeriodLifeTable(mx=mxE2,age=seq(0, 85, 5),ax=rep(NA,18),full.table=TRUE)

#PeriodLifeTable(mx=mxE2[1:11],age=c(0,5,15,24,34,44,54,64,74,84,85),ax=rep(NA,11),full.table=TRUE)


#CalculateDiffTable(lt1,lt2)
#DecomposeLifeExpDiff(lt1,lt2)


#redo what Sam Harper did in his blog code

dat <- read.csv("S:\\Projects\\Disease_trends\\Forecasting_US_Quentin\\Cause specific FC\\Beltran-Sanchez\\
                Arriaga/Underlying Cause of Death 1999-2015.csv")
#factor for age + ord3r correctly
dat$age <- factor(dat$Ten.Year.Age.Groups,levels=c("< 1 year","1-4 years","5-14 years","15-24 years","25-34 years",
                                                   "35-44 years","45-54 years","55-64 years","65-74 years",
                                                   "75-84 years","85+ years"))
#Female=1
dat$sex <- as.numeric(dat$Gender); table(dat$Gender,dat$sex)

dat$icd <- as.numeric(substr(dat$ICD.10.113.Cause.List.Code,7,9)); table(dat$icd)

dat$cause <- ifelse(dat$icd==16, "6 HIV",
                    ifelse(dat$icd %in% c(20:44), "2 Cancers",
                           ifelse(dat$icd==46, "3 Diabetes",
                                  ifelse(dat$icd==52, "4 Alzheimer's",
                                         ifelse(dat$icd==53, "1 CVDs",
                                                ifelse(dat$icd==76, "5 Flu/pneumonia",
                                                       ifelse(dat$icd==82, "7 Chronic Resp dx",
                                                              ifelse(dat$icd==93, "8 Liver dx",
                                                                     ifelse(dat$icd==97, "9 Kidney dx",
                                                                            ifelse(dat$icd==114, "10 MV crashes",
                                                                                   ifelse(dat$icd==122, "11 Poisoning",
                                                                                          ifelse(dat$icd==124, "12 Suicide",
                                                                                                 ifelse(dat$icd==127, "13 Homicide",
                                                                                                        ifelse(dat$icd %in% c(1:15,17,18,45,47,50,51,79,87:92,96,102:105,108:111,115,116,118:121,123,130,131,134:136), "14 Residual",      "bin")))))))))))))); table(dat$cause,exclude=FALSE)

### we actually remove the bin ones later. same as harper on his blog

dat$sex <- factor(dat$sex); dat$year <- factor(dat$Year)
dat$cause <- factor(dat$cause,levels=c("1 CVDs","2 Cancers","3 Diabetes","4 Alzheimer's","5 Flu/pneumonia",
                                       "6 HIV","7 Chronic Resp dx","8 Liver dx","9 Kidney dx","10 MV crashes",
                                       "11 Poisoning", "12 Suicide","13 Homicide","14 Residual","bin"))
dat <- dat[, which(names(dat) %in% c("sex","age","Deaths","Population","year","cause"))]

dat0 <- dat %>% 
  group_by(sex,age,year,cause) %>%
  summarise(deaths=sum(Deaths), pop=mean(Population), rate=deaths/pop)
sum(dat0$deaths); #sum(dat0$pop[dat0$cause=="6 HIV"])


#dat1 is the cause split death/pops/rates
dat1 <- dat0[dat0$age %in% c("< 1 year","1-4 years"),]  %>%
  group_by(sex,year,cause) %>%
  summarise(deaths=sum(deaths), pop=sum(pop), rate=deaths/pop)
dat1$age <- "0-4"

dat2 <- dat0[dat0$age %in% c("5-14 years"),]; dat2$pop <- dat2$pop/2; dat2$deaths <- dat2$deaths/2; datA <- dat2; datA$age <- "5-9"; datB <- dat2; datB$age <- "10-14"; dat1 <- rbind(dat1,datA,datB)
dat2 <- dat0[dat0$age %in% c("15-24 years"),]; dat2$pop <- dat2$pop/2; dat2$deaths <- dat2$deaths/2; datA <- dat2; datA$age <- "15-19"; datB <- dat2; datB$age <- "20-24"; dat1 <- rbind(dat1,datA,datB)
dat2 <- dat0[dat0$age %in% c("25-34 years"),]; dat2$pop <- dat2$pop/2; dat2$deaths <- dat2$deaths/2; datA <- dat2; datA$age <- "25-29"; datB <- dat2; datB$age <- "30-34"; dat1 <- rbind(dat1,datA,datB)
dat2 <- dat0[dat0$age %in% c("35-44 years"),]; dat2$pop <- dat2$pop/2; dat2$deaths <- dat2$deaths/2; datA <- dat2; datA$age <- "35-39"; datB <- dat2; datB$age <- "40-44"; dat1 <- rbind(dat1,datA,datB)
dat2 <- dat0[dat0$age %in% c("45-54 years"),]; dat2$pop <- dat2$pop/2; dat2$deaths <- dat2$deaths/2; datA <- dat2; datA$age <- "45-49"; datB <- dat2; datB$age <- "50-54"; dat1 <- rbind(dat1,datA,datB)
dat2 <- dat0[dat0$age %in% c("55-64 years"),]; dat2$pop <- dat2$pop/2; dat2$deaths <- dat2$deaths/2; datA <- dat2; datA$age <- "55-59"; datB <- dat2; datB$age <- "60-64"; dat1 <- rbind(dat1,datA,datB)
dat2 <- dat0[dat0$age %in% c("65-74 years"),]; dat2$pop <- dat2$pop/2; dat2$deaths <- dat2$deaths/2; datA <- dat2; datA$age <- "65-69"; datB <- dat2; datB$age <- "70-74"; dat1 <- rbind(dat1,datA,datB)
dat2 <- dat0[dat0$age %in% c("75-84 years"),]; dat2$pop <- dat2$pop/2; dat2$deaths <- dat2$deaths/2; datA <- dat2; datA$age <- "75-79"; datB <- dat2; datB$age <- "80-84"; dat1 <- rbind(dat1,datA,datB)
dat2 <- dat0[dat0$age %in% c("85+ years"),]; dat1 <- rbind(dat1,dat2)

ageGs <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85+ years")
dat1$age <- factor(dat1$age,levels=ageGs); table(dat1$age)
sum(dat1$deaths); sum(dat1$pop[dat1$cause=="6 HIV"])

####   Harper appears to remove the cause=NA(=bin) here but surely they should be left in for e0, so will leave them in for now   ####
#### this seems very odd but it gives the correct e0 which agrees with his blog so must be right.
dat1 <- dat1[dat1$cause!="bin", ]
sum(dat1$deaths); sum(dat1$pop)

#datAC is the all cause version
datAC <- dat1  %>% 
  group_by(sex,age,year) %>%
  summarise(deaths=sum(deaths), pop=pop[1], rate=deaths/pop) %>%
  arrange(sex,year,age)	
sum(datAC$deaths); sum(datAC$pop)

#check that e0 is consistent with Harper's
source("S:\\Projects\\Disease_trends\\Forecasting_US_Quentin\\Cause specific FC\\Beltran-Sanchez\\Arriaga/period_life_table.R")
datAC %>%
  group_by(sex,year) %>%
  summarise(e0=PeriodLifeTable(mx=rate,age=seq(0, 85, 5),ax=rep(NA,18),full.table=TRUE)$ex[1])

#check death rates have been split up properly by age
#bin <- dat[dat$year==2014 &dat$sex==1 & dat$cause!="bin",] %>% 
#	group_by(age) %>%
#	summarise(deaths=sum(Deaths), pop=mean(Population), rate=deaths/pop)
#plot(1:18,datAC[datAC$year==2014 &datAC$sex==1,]$rate,log="",xlim=c(0,18))
#points(c(0.2,0.8,2.5, 4.5,6.5, 8.5, 10.5, 12.5, 14.5, 16.5, 18),bin$rate, pch=18,col=2)

isx <- 2 # 1 is actuall women I think

lt1 <- PeriodLifeTable(mx=datAC[datAC$year==2014 &datAC$sex==isx,]$rate ,age=seq(0, 85, 5),ax=rep(NA,18),full.table=TRUE)
lt2 <- PeriodLifeTable(mx=datAC[datAC$year==2015 &datAC$sex==isx,]$rate ,age=seq(0, 85, 5),ax=rep(NA,18),full.table=TRUE)
dec1 <- DecomposeLifeExpDiff(lt1,lt2)
dec1$ageG <- factor(ageGs,levels=ageGs)
ggplot(dec1, aes(y=percent, x=ageG, fill=ifelse(percent>0,"seagreen","salmon"))) +geom_bar(stat="identity") + coord_flip() + 
  ggtitle(paste0("Sex",isx,", Proportion of e0 difference attributable to each age")) + 
  scale_fill_discrete(name="Contribution to\nchange in e0",labels=c("-ve ", "+ve ")) # Presumably done alphabetically - check colour order!!



if(FALSE){ # this assumes that sum should be across rows and columns
  ##proportion of deaths by age and cause, presumably adding to 1
  datG1 <- dat1[dat1$sex==isx & dat1$year==2014,] %>%
    group_by(sex,year) %>%
    mutate(Sdeaths=sum(deaths))
  datG2 <- dat1[dat1$sex==isx & dat1$year==2015,] %>%
    group_by(sex,year) %>%
    mutate(Sdeaths=sum(deaths))
  
  #check adding up by sex and cause
  datG1$pdeaths <- datG1$deaths/datG1$Sdeaths
  datG2$pdeaths <- datG2$deaths/datG2$Sdeaths
  #put into matrix format
  pdeathsG1 <- dcast(datG1, cause ~ age, value.var="pdeaths")
  pdeathsG2 <- dcast(datG2, cause ~ age, value.var="pdeaths")
}

if(FALSE){ # this assumes that sum should be for each cause group
  ##proportion of deaths by age for each cause, ie adding to 1 for each cause
  #firt just sum of deaths for each cause group
  datG1 <- dat1[dat1$sex==isx & dat1$year==2014,] %>%
    group_by(cause,sex,year) %>%
    mutate(Sdeaths=sum(deaths)) %>%
    mutate(pdeaths=deaths/Sdeaths)
  datG2 <- dat1[dat1$sex==isx & dat1$year==2015,] %>%
    group_by(cause,sex,year) %>%
    mutate(Sdeaths=sum(deaths)) %>%
    mutate(pdeaths=deaths/Sdeaths)
  #so sum should be 14
  
  #put into matrix format
  pdeathsG1 <- dcast(datG1, cause ~ age, value.var="pdeaths")
  pdeathsG2 <- dcast(datG2, cause ~ age, value.var="pdeaths")
}

if(TRUE){ # this assumes that there is 1 sum for all ages i.e. output is only 14 long
  #firt just sum of deaths for each cause group
  datG1 <- dat1[dat1$sex==isx & dat1$year==2014,] %>%
    group_by(cause,sex,year) %>%
    summarise(Sdeaths=sum(deaths))
  #propr by cause
  datG1$pdeaths <- datG1$Sdeaths/sum(datG1$Sdeaths)
  
  #firt just sum of deaths for each cause group
  datG2 <- dat1[dat1$sex==isx & dat1$year==2015,] %>%
    group_by(cause,sex,year) %>%
    summarise(Sdeaths=sum(deaths))
  #propr by cause
  datG2$pdeaths <- datG2$Sdeaths/sum(datG2$Sdeaths)
  #so sum should be 1
  
  #put into matrix format
  pdeathsG1 <- datG1$pdeaths
  pdeathsG2 <- datG2$pdeaths
}


##

#dec1$Dx #=te, contribution in change of years of e0 by age
#lt1$mx # mx2014,  death rate by age

#put together as in Harper
if(FALSE){
  #for 18*14 versions of pdeaths:
  out <- 
    dec1$Dx*
    (
      t(
        lt1$mx %*% t(pdeathsG1[,2:19]) - lt2$mx %*% t(pdeathsG2[,2:19])
      ) %*% 
        (1/(lt1$mx - lt2$mx))
    )
}  #FALSE

if(TRUE){
  #for 14*1 version of pdeaths:
  out <- 
    (as.matrix(dec1$Dx)) %*%
    t(
      t(lt1$mx %*% t(pdeathsG1) - lt2$mx %*% t(pdeathsG2)) %*% as.matrix(1/(lt1$mx - lt2$mx))
    )
  out <- t(out)
} #FALSE

if(FALSE){
  #for version where do one cause at atime and then put together - if look at stat code this sems more likely
  
  out <- data.frame(matrix(ncol=18,nrow=14))
  for(ic in 1:14){
    out[ic,1:18] <- as.vector(dec1$Dx*((lt1$mx * pdeathsG1[ic,2:19] - lt2$mx * pdeathsG2[ic,2:19])/(lt1$mx - lt2$mx)))
  }
  
}

#######
#plot(colSums(out),dec1$Dx)
#I think out needs somehow to be divided by 18 
#outbac <- out
print("THIS IS ARBITRARY DIVISION TO MAKE SUM OF AGE CONTRIBUTIONS AND SUM OF CAUSE CONTRIBUTION BOTH AGREE WITH DIFFERENCE IN E0, HENCE WE CAN USE HEAT MAP OF ALL SINC ALL ADDS UP TO TOTAL")
out <- out/18


out_by_age <- data.frame(by_age=colSums(out))
out_by_age$by_age <- out_by_age$by_age/sum(out_by_age$by_age) # turn into proportions
#add factor labels
out_by_age$ageG <- factor(ageGs,levels=ageGs)

plot(out_by_age$by_age*100,dec1$percent); abline(0,1,col=4)


ggplot(out_by_age, aes(y=by_age, x=ageG, fill=ifelse(by_age>0,"seagreen","salmon"))) +geom_bar(stat="identity") + coord_flip() + 
  ggtitle(paste0("Sex",isx,", Proportion of e0 difference attributable to each age")) + 
  scale_fill_discrete(name="Contribution to\nchange in e0",labels=c("-ve ", "+ve "))

#######

out_by_cause <- data.frame(by_cause=rowSums(out))
out_by_cause$by_cause <- out_by_cause$by_cause/sum(out_by_cause$by_cause) # turn into proportions
#add factor labels
out_by_cause$cause <- factor(c("1 CVDs","2 Cancers","3 Diabetes","4 Alzheimer's","5 Flu/pneumonia","6 HIV","7 Chronic Resp dx","8 Liver dx","9 Kidney dx","10 MV crashes","11 Poisoning", "12 Suicide","13 Homicide","14 Residual"),levels=c("1 CVDs","2 Cancers","3 Diabetes","4 Alzheimer's","5 Flu/pneumonia","6 HIV","7 Chronic Resp dx","8 Liver dx","9 Kidney dx","10 MV crashes","11 Poisoning", "12 Suicide","13 Homicide","14 Residual"))

ggplot(out_by_cause, aes(y=by_cause, x=cause, fill=ifelse(by_cause>0,"seagreen","salmon"))) +geom_bar(stat="identity") + coord_flip() + 
  ggtitle(paste0("Sex",isx,", Proportion of e0 difference attributable to each age")) + 
  scale_fill_discrete(name="Contribution to\nchange in e0",labels=c("-ve ", "+ve "))
##

#check sum of split equals original difference
(lt1$ex[1] - lt2$ex[1]) 
sum(out)

##TILES OF CAUSE-AGE
outtile <- out
rownames(outtile) <- c("1 CVDs","2 Cancers","3 Diabetes","4 Alzheimer's","5 Flu/pneumonia","6 HIV","7 Chronic Resp dx","8 Liver dx","9 Kidney dx","10 MV crashes","11 Poisoning", "12 Suicide","13 Homicide","14 Residual")
colnames(outtile) <- ageGs

bin <- melt(outtile); bin$Var1 <- factor(bin$Var1,levels=c("1 CVDs","2 Cancers","3 Diabetes","4 Alzheimer's","5 Flu/pneumonia","6 HIV","7 Chronic Resp dx","8 Liver dx","9 Kidney dx","10 MV crashes","11 Poisoning", "12 Suicide","13 Homicide","14 Residual"))
bin$Var2 <- factor(bin$Var2,levels=ageGs)


mypal <- colorRampPalette( c("darkgreen","springgreen2","gold","red","darkred") ) # this version with yellow instead of gold was used beofre and with same values below.
mypal <- colorRampPalette( c("darkgreen","springgreen2","white","gold","red","darkred") ) # this version with yellow instead of gold was used beofre and with same values below.

k1 <- ggplot(bin, aes(x=Var2,y=Var1)) + geom_tile(aes(fill=value)) +
  #scale_fill_gradientn(guide="colourbar", name="Contribution to\n change\nin e0 (years)",colours=mypal(100),mid="white", values=c(0,.3,.35,.375,.4,.425,.45,.475,.5,.6,1)) +   #
  scale_fill_gradient2(low = "darkgreen", high = "darkred", mid = "white",    midpoint = 0,name="Contribution to\n change\nin e0 (years)") + 
  theme_bw() + theme(panel.border = element_blank(),panel.grid = element_blank(), 
                     legend.position = "right",strip.background = element_blank())+labs(x = "", y = "") + coord_fixed() + theme(axis.text.x=element_text(angle=65,hjust=1))
grid.arrange(k1,ncol=1)
























