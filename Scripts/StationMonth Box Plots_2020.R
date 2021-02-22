#Script for Station Box Plots over year, 2020:

squid20 <- read.csv("./Data/Lbrevis 2020 Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
install.packages("reshape")
library(reshape)
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
view(squid20)

###############################################################################
#Code that actually fucking works:

#Month Names
Month = c(1:12)
Month.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
squid.month.names = cbind(Month, Month.name)
squid20 = merge(squid20, squid.month.names)
view(squid20)

#Filtering for data for each site
squid.Anc.20 <- filter(squid20, Station == "Anchorage")
view(squid.Anc.20)
squid.Ftj.20 = filter(squid20, Station == "Fort Johnson")
view(squid.Ftj.20)
squid.LA.20 = filter(squid20, Station == "Lower Ashley")
squid.UA.20 = filter(squid20, Station == "Upper Ashley")
view(squid.UA.20)

#Binding each subset with Month names
squid20$Month.name = factor(squid20$Month.name, levels = Month.name)
squid.Anc.20$Month.name = factor(squid.Anc.20$Month.name, levels = Month.name)
View(squid.Anc.20)
squid.Ftj.20$Month.name = factor(squid.Ftj.20$Month.name, levels = Month.name)
view(squid.Ftj.20)
squid.LA.20$Month.name = factor(squid.LA.20$Month.name, levels = Month.name)
squid.UA.20$Month.name = factor(squid.UA.20$Month.name, levels = Month.name)

#Seperate Box Plots for each Station

ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.Anc.20)+
  geom_boxplot(show.legend=F, fill = "firebrick1") + labs(title="Anchorage", x="Month", y = "Gladius Length (cm)") 

ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.Ftj.20)+
  geom_boxplot(show.legend=F, fill = "firebrick2") + labs(title="Fort Johnson", x="Month", y = "Gladius Length (cm)")

ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.LA.20)+
  geom_boxplot(show.legend=F, fill = "firebrick3") + labs(title="Lower Ashley", x="Month", y = "Gladius Length (cm)")

ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.UA.20)+
  geom_boxplot(show.legend=F, fill = "firebrick4") + labs(title="Upper Ashley", x="Month", y = "Gladius Length (cm)")

#All station summaries in one figure. 
ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid20)+
  geom_boxplot(show.legend=F, fill = "firebrick") + 
  labs(x="Month", y = "Gladius Length (cm)") + 
  facet_wrap(~Station)
################################################################################

#Box plot of gladius length distribution at each station overall in 2020
boxplot(Gladius.Length ~ Station , data = squid)
ggplot(aes(x=Station, y = Gladius.Length, fill = Station), data = squid) + 
  geom_boxplot(show.legend=F) + labs(x="Station", y = "Gladius Length (cm)") +
  scale_fill_brewer(palette = 3)



################################################################################