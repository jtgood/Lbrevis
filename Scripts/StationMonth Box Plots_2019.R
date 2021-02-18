#Script for Station Box Plots over year, 2019:

squid <- read.csv("./Data/Lbrevis QM Data.csv", header = TRUE)
abiotic <- read.csv("./Data/Lbrevis Abiotic Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
install.packages("reshape")
library(reshape)
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
view(squid)

###############################################################################
#Code that actually fucking works:

#Month Names
Month = c(1:12)
Month.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
squid.month.names = cbind(Month, Month.name)
squid = merge(squid,squid.month.names)
view(squid)

#Filtering for data for each site
squid.Anc <- filter(squid, Station == "Anchorage")
view(squid.Anc)
squid.Ftj = filter(squid, Station == "Fort Johnson")
view(squid.Ftj)
squid.LA = filter(squid, Station == "Lower Ashley")
squid.UA = filter(squid, Station == "Upper Ashley")

#Binding each subset with Month names
squid$Month.name = factor(squid$Month.name, levels = Month.name)
squid.Anc$Month.name = factor(squid.Anc$Month.name, levels = Month.name)
View(squid.Anc)
squid.Ftj$Month.name = factor(squid.Ftj$Month.name, levels = Month.name)
view(squid.Ftj)
squid.LA$Month.name = factor(squid.LA$Month.name, levels = Month.name)
squid.UA$Month.name = factor(squid.UA$Month.name, levels = Month.name)

#Seperate Box Plots for each Station

ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.Anc)+
  geom_boxplot(show.legend=F, fill = "blue1") + labs(title="Anchorage", x="Month", y = "Gladius Length (cm)") 

ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.Ftj)+
  geom_boxplot(show.legend=F, fill = "blue2") + labs(title="Fort Johnson", x="Month", y = "Gladius Length (cm)")

ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.LA)+
  geom_boxplot(show.legend=F, fill = "blue3") + labs(title="Lower Ashley", x="Month", y = "Gladius Length (cm)")

ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.UA)+
  geom_boxplot(show.legend=F, fill = "blue4") + labs(title="Upper Ashley", x="Month", y = "Gladius Length (cm)")

#All station summaries in one figure. 
ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid)+
  geom_boxplot(show.legend=F, fill = "blue") + 
  labs(x="Month", y = "Gladius Length (cm)") + 
  facet_wrap(~Station)
################################################################################

#Box plot of gladius length distribution at each station overall in 2019
boxplot(Gladius.Length ~ Station , data = squid)
ggplot(aes(x=Station, y = Gladius.Length, fill = Station), data = squid) + 
  geom_boxplot(show.legend=F) + labs(x="Station", y = "Gladius Length (cm)") +
  scale_fill_brewer(palette = 3)




