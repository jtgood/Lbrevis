#Ranges: Seperate Gladius length of Males and females (2019):

squid <- read.csv("./Data/Lbrevis QM Data.csv", header = TRUE)
abiotic <- read.csv("./Data/Lbrevis Abiotic Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
view(squid)

#Filter by Sex
squid.male <- filter(squid, Sex == 'M')
view(squid.male)
squid.female <- filter(squid, Sex == 'F')
view(squid.female)
squid.juv <- filter(squid, Sex == 'I')
view(squid.juv)

#Filter by Month
mo <- c("8","9","10","11","12")#used as a target element in the dplyr piping function.
squid.male.month = filter(squid.male, Month %in% mo)
view(squid.male.month)
squid.female.month = filter(squid.female, Month %in% mo)
view(squid.female.month)
squid.juv.month = filter(squid.juv, Month %in% mo)
view(squid.juv.month)

#Min and Max of Gladius sizes
min(squid.male.month[ ,9], na.rm = T)
max(squid.male.month[ ,9], na.rm = T)
min(squid.female.month[ ,9], na.rm = T)
max(squid.female.month[ ,9], na.rm = T)
min(squid.juv.month[ ,9], na.rm = T)
max(squid.juv.month[ ,9], na.rm = T)

#Male Gladius Lengths between August 2019 and November 2019: 1.0 cm - 5.7 cm 
#Female Gladius Lengths between August 2019 and November 2019: 2.9 cm - 8.9 cm
#Immature Gladius Lengths between August 2019 and November 2019: 1.0 cm - 6.2 cm
