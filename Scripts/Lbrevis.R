squid <- read.csv("./Data/Lbrevis QM Data.csv", header = TRUE)
abiotic <- read.csv("./Data/Lbrevis Abiotic Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)


# Organizations and Plots

#Squid Abundance per Month and Station
AbundbyMoSt <- squid %>%
  group_by(Month, Station) %>%
  mutate(Abundance = length(Squid.Number))
View(AbundbyMoSt)
ggplot(aes(x = Month, y = Abundance), data = AbundbyMoSt)+
  geom_bar(stat = "identity", position="dodge")+#default ggplot will use count
  #need to specify x and y using stat = "identity"
  facet_wrap(~Station, ncol=1)

#Squid Sex per Month and Station
squidsex<-subset(squid, squid$sex!="")
View(squidsex)
squidsex <- squid %>% 
  drop_na(Sex)
ggplot(aes(x=Month, fill = Sex), data = squidsex)+
  geom_histogram(position = "dodge")+facet_wrap(~Station, ncol=1)

#Average Mantle and Gladius Length per Month and Station
LenbyMoSt<-squid %>%
  group_by(Month, Station) %>%
  summarize(Mantle.Length = mean(Mantle.Length, na.rm = T),
            Gladius.Length = mean(Gladius.Length, na.rm =T))
View(LenbyMoSt)

#Plot of Mantle Length Per Month
boxplot(Mantle.Length ~ Month,
        data = LenbyMoSt,
        main = "Mantle Lengths per Month",
        xlab = "Month Number",
        ylab = "Average Mantle Length",
        col = "blue",
        border = "black")

#Plot of Mantle Length per Station
boxplot(Mantle.Length ~ Station,
        data = LenbyMoSt,
        main = "Mantle Lengths per Station",
        xlab = "Station",
        ylab = "Average Mantle Length",
        col = "red",
        border = "black")

#Plot of Gladius Length Per Month
boxplot(Gladius.Length ~ Month,
        data = LenbyMoSt,
        main = "Gladius Lengths per Month",
        xlab = "Month Number",
        ylab = "Average Gladius Length",
        col = "blue",
        border = "black")

#Plot of Gladius Length per Station 
boxplot(Gladius.Length ~ Station,
        data = LenbyMoSt,
        main = "Gladius Lengths per Station",
        xlab = "Station",
        ylab = "Average Gladius Length",
        col = "red",
        border = "black") 

#Regression of mantle length and glaidus length 
plot(Mantle.Length ~ Gladius.Length, data = squid)
mod <- lm(Mantle.Length ~ Gladius.Length, data = squid)
abline(mod, col='red')
summary(lm(Mantle.Length ~ Gladius.Length, data = squid))
#R2 value of 0.954
anova(update(mod, . ~ . + as.factor(Month)))


#Anova Analyses

#Anova of squid abundance against month and station and abiotic variables
anovaAbund <- aov(Abundance ~ Month * Station + Temperature * Salinity, data = AbundbyMoSt)
summary(anovaAbund)

#Anova of Mantle Length and Station 
anovaMant <- aov(Mantle.Length ~ Month * Station, data = LenbyMoSt)
summary(anovaMant)

#Anova of Gladius Length and Station
anovaGlad <- aov(Gladius.Length ~ Month * Station, data = LenbyMoSt)
summary(anovaGlad)

#Gladius length of a squid more reliable as a measurment due to consistency of 
#structure and resistence of chitin compared to soft mantle.  Gladius length 
#chosen as default length measurment.

#Anova for female gladius lengths with station with interaction with abiotic factors


#Anova for male gladius length with station with interaction with temperature
males <- subset(squid, squid$Sex == "M")
anovamale <- aov(Gladius.Length ~ Station * Temperature + Salinity, data = males)
summary(anovamale)
plot(anovamale)

#anova for female gladisu length with stations with interaction with temperature
females <- subset(squid, squid$Sex == "F")
anovafemale <- aov(Gladius.Length ~ Station * Temperature + Salinity, data = females)
summary(anovafemale)
plot(anovafemale)

#Anova for juvenile gladius length with station with interaction with temperature
juvenile <- subset(squid, squid$Sex == "J")
anovaJuv <- aov(Gladius.Length ~ Station * Temperature + Salinity, data = juvenile)
summary(anovaJuv)
plot(anovaJuv)

#Possible seasonal differences in sex gladius lengths


#Anova for male gladius length with month with interaction with temperature
anovamaleMo <- aov(Gladius.Length ~ Month * Temperature + Salinity, data = males)
summary(anovamaleMo)
plot(anovamaleMo)

#Anova for female gladius lengths with month with interaction with abiotic factors
anovafemMo <- aov(Gladius.Length ~ Month * Temperature + Salinity, data = females)
summary(anovafemMo)
plot(anovafemMo)

#Anova for juvenile gladius length with month with interaction with temperatur
anovaJuvMo <- aov(Gladius.Length ~ Month * Temperature + Salinity, data = juvenile)
summary(anovaJuvMo)
plot(anovaJuvMo)

#Abiotic Factors on Gladius Length in relation to Station
GladAbioStation<-squid %>%
  group_by(Station, Salinity, Temperature) %>%
  summarize(Gladius.Length = mean(Gladius.Length, na.rm =T))
View(GladAbioStation)
plot(Gladius.Length ~ Salinity, data = GladAbioStation)
plot(Gladius.Length ~ Temperature, data = GladAbioStation)
anovaAbioSt <- aov(Gladius.Length ~ Station * Temperature + Salinity,
                   data = GladAbioStation)
summary(anovaAbioSt)
plot(anovaAbioSt)

#Abiotic Factors on Gladius Length in relation to Month
GladAbioMonth <- squid %>%
  group_by(Month, Salinity, Temperature) %>%
  summarize(Gladius.Length = mean(Gladius.Length, na.rm = T))
View(GladAbioMonth)
anovaAbioMo <- aov(Gladius.Length ~ Month * Temperature + Salinity,
                   data = GladAbioMonth)
summary(anovaAbioMo)
plot(anovaAbioMo)

###################

#Working on future way to replace month number with month name

#If data doesn't recognize the type of variable then need to redifne
squid$Month<-factor(squid$Month, levels=c("1","2","3"..."7","8","9"...."12"))







