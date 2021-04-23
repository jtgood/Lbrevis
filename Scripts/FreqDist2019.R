#Frequency Distribution Plots 2019
squid <- read.csv("./Data/Lbrevis QM Data.csv", header = TRUE)
abiotic <- read.csv("./Data/Lbrevis Abiotic Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
install.packages("reshape")
install.packages("ggpubr")
library(reshape)
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
view(squid)

#Size frequency distribution plot with Station 
hist(squid$Gladius.Length)

squid.Anc <- squid %>% filter(Station == "Anchorage")
View(squid.Anc)
squid$Station
squid.Ftj = filter(squid, Station == "Fort Johnson")
view(squid.Ftj)
squid.LA = filter(squid, Station == "Lower Ashley")
squid.UA = filter(squid, Station == "Upper Ashley")

squid.Anc.hist = ggplot(squid.Anc, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="Anchorage", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,85))
squid.Anc.hist
squid.Ftj.hist = ggplot(squid.Ftj, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="Fort Johnson", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,85))
squid.Ftj.hist
squid.LA.hist = ggplot(squid.LA, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="Lower Ashley", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,85))
squid.LA.hist
squid.UA.hist = ggplot(squid.UA, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="Upper Ashley", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,85))
squid.UA.hist

#Size Distributuion with Station Facet Wrap Plot
sizedist.station = ggarrange(squid.Anc.hist, squid.Ftj.hist, squid.LA.hist, squid.UA.hist,
                     ncol = 1, nrow = 4, align = "hv")
sizedist.station

###Size Distribution Plot with Month
#Filter for each Month
squid.Jan <- squid %>% filter(Month.name == "Jan")
View(squid.Jan)
squid.Feb <- squid %>% filter(Month.name == "Feb")
squid.Mar <- squid %>% filter(Month.name == "Mar")
squid.Apr <- squid %>% filter(Month.name == "Apr")
squid.May <- squid %>% filter(Month.name == "May")
view(squid.May)
squid.Jun <- squid %>% filter(Month.name == "Jun")
squid.Jul <- squid %>% filter(Month.name == "Jul")
squid.Aug <- squid %>% filter(Month.name == "Aug")
squid.Sep <- squid %>% filter(Month.name == "Sep")
squid.Oct <- squid %>% filter(Month.name == "Oct")
squid.Nov <- squid %>% filter(Month.name == "Nov")
squid.Dec <- squid %>% filter(Month.name == "Dec")

#Histograms for Each Month
squid.Jan.hist = ggplot(squid.Jan, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="January", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Jan.hist
squid.Feb.hist = ggplot(squid.Feb, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="Feburary", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Feb.hist
squid.Mar.hist = ggplot(squid.Mar, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="March", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Mar.hist
squid.Apr.hist = ggplot(squid.Apr, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="April", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Apr.hist
squid.May.hist = ggplot(squid.May, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="May", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.May.hist
squid.Jun.hist = ggplot(squid.Jun, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="June", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Jun.hist
squid.Jul.hist = ggplot(squid.Jul, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="July", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Jul.hist
squid.Aug.hist = ggplot(squid.Aug, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="August", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Aug.hist
squid.Sep.hist = ggplot(squid.Sep, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="September", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Sep.hist
squid.Oct.hist = ggplot(squid.Oct, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="October", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Oct.hist
squid.Nov.hist = ggplot(squid.Nov, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="November", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Nov.hist
squid.Dec.hist = ggplot(squid.Dec, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="December", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Dec.hist

#Size Distributuion with Season(Month) Facet Wrap Plot
sizedist.month1 = ggarrange(squid.Jan.hist, squid.Feb.hist, squid.Mar.hist,
                            squid.Apr.hist, ncol = 1, nrow = 4, align = "hv")
sizedist.month1
sizedist.month2 = ggarrange(squid.May.hist, squid.Jun.hist, squid.Jul.hist, 
                            squid.Aug.hist, ncol = 1, nrow = 4, align = "hv")
sizedist.month2
sizedist.month3 = ggarrange(squid.Sep.hist, squid.Oct.hist, squid.Nov.hist, 
                            squid.Dec.hist, ncol = 1, nrow = 4, align = "hv")
sizedist.month3

