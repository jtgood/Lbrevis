getwd()
setwd("C:/Users/jeffr/Documents/College of Charleston/Classes/Quantitative Methods/Lbrevis")
getwd()
squid <- read.csv("./Data/Lbrevis QM Data.csv", header = TRUE)
abiotic <- read.csv("./Data/Lbrevis Abiotic Data.csv", header = TRUE)
install.packages("dplyr")
library(dplyr)

squid <- as.data.frame(squid)
squid
sitesex <-
  squid %>%
  group_by(squid$Station) %>%
  mutate(squid, sexavg = mean(squid$Sex, na.rm = TRUE))%>%
  mutate(squid, sexstr = sd(sexavg/sqrt(length(squid$Sex))))
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

