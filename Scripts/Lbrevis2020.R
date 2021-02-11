#GENERAL WORKUP 2020#

squid2 <- read.csv("./Data/Lbrevis 2020 Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
install.packages("reshape2")
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
view(squid2)

# Organizations and Plots

#Squid Abundance per Month and Station
AbundbyMoSt2 <- squid2 %>%
  group_by(Month, Station) %>%
  mutate(Abundance = length(Squid.Number))
View(AbundbyMoSt2)
ggplot(aes(x = Month, y = Abundance), data = AbundbyMoSt2)+
  geom_bar(stat = "identity", position="dodge")+#default ggplot will use count
  #need to specify x and y using stat = "identity"
  facet_wrap(~Station, ncol=1)

#Squid Sex per Month and Station
squidsex2<-subset(squid2, squid2$Sex!="")
View(squidsex2)
squidsex2 <- squid2 %>% 
  drop_na(Sex)
ggplot(aes(x=Month, fill = Sex), data = squidsex2)+
  geom_histogram(position = "dodge")+facet_wrap(~Station, ncol=1)
 

#Average Mantle and Gladius Length per Month and Station
LenbyMoSt2<-squid2 %>%
  group_by(Month, Station) %>%
  summarize(Mantle.Length = mean(Mantle.Length, na.rm = T),
            Gladius.Length = mean(Gladius.Length, na.rm =T))
View(LenbyMoSt2)

#Plot of Mantle Length Per Month
boxplot(Mantle.Length ~ Month,
        data = LenbyMoSt2,
        main = "Mantle Lengths per Month",
        xlab = "Month Number",
        ylab = "Average Mantle Length",
        col = "blue",
        border = "black")

#Plot of Mantle Length per Station
boxplot(Mantle.Length ~ Station,
        data = LenbyMoSt2,
        main = "Mantle Lengths per Station",
        xlab = "Station",
        ylab = "Average Mantle Length",
        col = "red",
        border = "black")

#Plot of Gladius Length Per Month
boxplot(Gladius.Length ~ Month,
        data = LenbyMoSt2,
        main = "Gladius Lengths per Month",
        xlab = "Month Number",
        ylab = "Average Gladius Length",
        col = "blue",
        border = "black")

#Plot of Gladius Length per Station 
boxplot(Gladius.Length ~ Station,
        data = LenbyMoSt2,
        main = "Gladius Lengths per Station",
        xlab = "Station",
        ylab = "Average Gladius Length",
        col = "red",
        border = "black") 

#Plot of Gladius Length over Month per Station


#Regression of mantle length and glaidus length 
plot(Mantle.Length ~ Gladius.Length, data = squid2)
mod <- lm(Mantle.Length ~ Gladius.Length, data = squid2)
abline(mod, col='red')
summary(lm(Mantle.Length ~ Gladius.Length, data = squid2))
#R2 value of 0.954
anova(update(mod, . ~ . + as.factor(Month)))


#Anova Analyses

############NEED UPDATED ABIOTIC DATA######################


#Anova of squid abundance against month and station and abiotic variables
anovaAbund2 <- aov(Abundance ~ Month * Station + Temperature * Salinity, data = AbundbyMoSt2)
summary(anovaAbund2)

#Anova of Mantle Length and Station 
anovaMant2 <- aov(Mantle.Length ~ Month * Station, data = LenbyMoSt2)
summary(anovaMant2)

#Anova of Gladius Length and Station
anovaGlad2 <- aov(Gladius.Length ~ Month * Station, data = LenbyMoSt2)
summary(anovaGlad2)

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
