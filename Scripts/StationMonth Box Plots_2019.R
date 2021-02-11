#Script for Station Box Plots over year:

```{r}
squid <- read.csv("./Data/Lbrevis QM Data.csv", header = TRUE)
abiotic <- read.csv("./Data/Lbrevis Abiotic Data.csv", header = TRUE)

install.packages("tidyverse")
install.packages("purrr")
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
view(squid)
```

##############################################################################################################################

#Code that actually fucking works:

#Month Names
```{r}
Month = c(1:12)
Month.name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
squid.month.names = cbind(Month, Month.name)
squid = merge(squid,squid.month.names)
View(squid)
```

#Filtering for data for each site
```{r}
squid.Anc <- filter(squid, Station == "Anchorage")
view(squid.Anc)
squid.Ftj = filter(squid, Station == "Fort Johnson")
squid.LA = filter(squid, Station == "Lower Ashley")
squid.UA = filter(squid, Station == "Upper Ashley")
```

#Seperate Box Plots for each Station
```{r}
ggplot(aes(x = Month.name, y = Gladius.Length, fill=Month.name), data = squid.Anc)+
  geom_boxplot(show.legend=F, fill = "blue") + labs(title="Anchorage", x="Month", y = "Gladius Length (cm)") 
```
All, november and december months are missing. 


##############################################################################################################################

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




##############################################################################################################################
