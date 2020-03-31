squid <- read.csv("./Data/Lbrevis QM Data.csv", header = TRUE)
abiotic <- read.csv("./Data/Lbrevis Abiotic Data.csv", header = TRUE)
install.packages("dplyr")
library(dplyr)

########################
month <- c(1:12)
month.name[month]
head(squid)
# summarize quantiative variables

#Squid Abundance per Month and Station
AbundbyMoSt <- squid %>%
  group_by(Month, Station) %>%
  summarize(Abundance = count(Squid.Number))
View(AbundbyMoSt)

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

anovaMant <- aov(Mantle.Length ~ Month +Station, data = LenbyMoSt)
summary(anovaMant)
anovaGlad <- aov(Gladius.Length ~ Month + Station, data = LenbyMoSt)
summary(anovaGlad)

#Squid Sex per Month and Station
SexbyMoSt <- squid %>%
  group_by(Month, Station) %>%
  summarize(Sex = length(Sex))
View(SexbyMoSt)

#Abiotic Factors
GladAbioStation<-squid %>%
  group_by(Station, Salinity, Temperature) %>%
  summarize(Gladius.Length = mean(Gladius.Length, na.rm =T))
View(GladAbioStation)
plot(Gladius.Length ~ Salinity, data = GladAbioStation)

anova2 <- aov(Gladius.Length[Station] ~ Salinity[Station] + Temperature [Station],
              data = GladAbioStation)
summary(anova2)

GladAbioMonth <- squid %>%
  group_by(Month, Salinity, Temperature) %>%
  summarize(Gladius.Length = mean(Gladius.Length, na.rm = T))
View(GladAbioMonth)


# show boxplots for lengths:
boxplot(Gladius.Length ~ Month, data = squid)

# regression of mantle length and glaidus length
plot(Mantle.Length ~ Gladius.Length, data = squid)
mod <- lm(Mantle.Length ~ Gladius.Length, data = squid)
abline(mod, col='red')
summary(lm(Mantle.Length ~ Gladius.Length, data = squid))
#R2 value of 0.954
anova(update(mod, . ~ . + as.factor(Month)))
###################








squid <- as.data.frame(squid)
squid
sitesex <-
  squid %>%
  group_by(Station) %>%
  group_by(Month) %>%
  summarise(squid, length(squid$Sex, na.rm = TRUE))
  
sitesex

squid <- as.data.frame(squid)
squid$Month <- as.character(squid$Month)
squid$Mantle.Length <- as.numeric(squid$Mantle.Length)

mantlelengthmonth <- 
  squid%>%
  group_by(Month)%>%
  summarise(ML_Month = mean(Mantle.Length, na.rm = TRUE), ML_MonthSE = sd(Mantle.Length, na.rm = TRUE)/(sqrt(count(Mantle.Length, na.rm = TRUE))))
mantlelengthmonth

summarize()
aggregate()

squid$Month
squid$Mantle.Length
class(squid$Mantle.Length)
class(squid$Month)

