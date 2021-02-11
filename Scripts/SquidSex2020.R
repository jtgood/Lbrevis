#Ranges: Seperate Gladius length of Males and females (2020):

squid2 <- read.csv("./Data/Lbrevis 2020 Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
install.packages("reshape2")
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
view(squid2)

#Filter by Sex
squid2.male <- filter(squid2, Sex == 'M')
view(squid2.male)
squid2.female <- filter(squid2, Sex == 'F')
view(squid2.female)
squid2.juv <- filter(squid2, Sex == 'I')
view(squid2.juv)

#Filter by Month
mo2 <- c("8","9","10","11")#used as a target element in the dplyr piping function.
squid2.male.month = filter(squid2.male, Month %in% mo2)
view(squid2.male.month)
squid2.female.month = filter(squid2.female, Month %in% mo2)
view(squid2.female.month)
squid2.juv.month = filter(squid2.juv, Month %in% mo2)
view(squid2.juv.month)

#Min and Max of Gladius sizes
min(squid2.male.month[ ,9], na.rm = T)
max(squid2.male.month[ ,9], na.rm = T)
min(squid2.female.month[ ,9], na.rm = T)
max(squid2.female.month[ ,9], na.rm = T)
min(squid2.juv.month[ ,9], na.rm = T)
max(squid2.juv.month[ ,9], na.rm = T)

#Male Gladius Lengths between August 2020 and November 2020: 2.8 cm - 5.4 cm 
#Female Gladius Lengths between August 2020 and November 2020: 2.5 cm - 8.4 cm
#Immature Gladius Lengths between August 2020 and November 2020: 1.9 cm - 3.5 cm