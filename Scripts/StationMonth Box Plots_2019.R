#Script for Station Box Plots over year:

squid <- read.csv("./Data/Lbrevis QM Data.csv", header = TRUE)
abiotic <- read.csv("./Data/Lbrevis Abiotic Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
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
view(squid.Anch)
squid.Ftj = filter(squid, Station == "Fort Johnson")
squid.LA = filter(squid, Station == "Lower Ashley")
squid.UA = filter(squid, Station == "Upper Ashley")

#Binding each subset with Month names
squid.Anc$Month.name = factor(squid.Anc$Month.name, levels = c("Jan", "Feb",
  "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))
squid.Ftj$Month.name = factor(squid.Ftj$Month.name, levels = c("Jan", "Feb", 
  "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))
squid.LA$Month.name = factor(squid.LA$Month.name, levels = c("Jan", "Feb", 
  "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))
squid.UA$Month.name = factor(squid.UA$Month.name, levels = c("Jan", "Feb", 
  "March", "April", "May", "June", "July", "Aug", "Sept", "Oct", "Nov", "Dec"))

#Seperate Box Plots for each Station

#Doesn't work: 
ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.Anc)+
  geom_boxplot(show.legend=F, fill = "blue") + labs(title="Anchorage", x="Month", y = "Gladius Length (cm)") 
#All, november and december months are missing. 
#######################GUS############################
##works: 
ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.Anch)+
  geom_boxplot(show.legend=F, fill = "blue") + labs(title="Anchorage", x="Month", y = "Gladius Length (cm)")




LenbyMoStTest <- melt(LenbyMoSt, id.var = "Label")

ggplot(aes(x = Month, y = Gladius.Length), data = Smith)+
  geom_boxplot()+#default ggplot will use count
  #need to specify x and y using stat = "identity"
  facet_wrap(~Station, ncol=1)

boxplot(Gladius.Length ~ Month , data = LenbyMoSt)

boxplot(Gladius.Length ~ Month,
        data = LenbyMoSt,
        main = "Gladius Lengths per Month",
        xlab = "Month Number",
        ylab = "Average Gladius Length",
        col = "blue",
        border = "black")




################################################################################
