#Squid Frequency Distribution Plots 2020

squid20 <- read.csv("./Data/Lbrevis 2020 Data.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
install.packages("reshape")
library(reshape)
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)

#Size frequency distribution plot 2020
hist(squid20$Gladius.Length)

squid.Anc.20 <- filter(squid20, Station == "Anchorage")
view(squid.Anc.20)
squid.Ftj.20 = filter(squid20, Station == "Fort Johnson")
view(squid.Ftj.20)
squid.LA.20 = filter(squid20, Station == "Lower Ashley")
squid.UA.20 = filter(squid20, Station == "Upper Ashley")
view(squid.UA.20)

squid.Anc.20.hist = ggplot(squid.Anc.20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="Anchorage", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,85))
squid.Anc.20.hist
squid.Ftj.20.hist = ggplot(squid.Ftj.20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="Fort Johnson", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,85))
squid.Ftj.20.hist
squid.LA.20.hist = ggplot(squid.LA.20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="Lower Ashley", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,85))
squid.LA.20.hist
squid.UA.20.hist = ggplot(squid.UA.20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="Upper Ashley", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,85))
squid.UA.20.hist

#Size Distributuion with Station Facet Wrap Plot 2020
sizedist.station.20 = ggarrange(squid.Anc.20.hist, squid.Ftj.20.hist, squid.LA.20.hist, 
                        squid.UA.20.hist, ncol = 1, nrow = 4, align = "hv")
sizedist.station.20

###Size Distribution Plot with Month

#Month Names
Month = c(1:12)
Month.nameND = c("Jan", "Feb", "Mar(ND)", "Apr(ND)", "May(ND)", "Jun", "Jul(ND)", "Aug", "Sep", "Oct", "Nov", "Dec")
squid.month.namesND = cbind(Month, Month.nameND)
squid20 = merge(squid20, squid.month.namesND)
view(squid20)

#Filter for each Month
squid.Jan20 <- squid20 %>% filter(Month.nameND == "Jan")
View(squid.Jan20)
squid.Feb20 <- squid20 %>% filter(Month.nameND == "Feb")
squid.Mar20 <- squid20 %>% filter(Month.nameND == "Mar(ND)")
squid.Apr20 <- squid20 %>% filter(Month.nameND == "Apr(ND)")
view(squid.Apr20)
squid.May20  <- squid20 %>% filter(Month.nameND == "May(ND)")
view(squid.May20)
squid.Jun20 <- squid20 %>% filter(Month.nameND == "Jun")
squid.Jul20 <- squid20 %>% filter(Month.nameND == "Jul(ND)")
squid.Aug20 <- squid20 %>% filter(Month.nameND == "Aug")
squid.Sep20 <- squid20 %>% filter(Month.nameND == "Sep")
squid.Oct20 <- squid20 %>% filter(Month.nameND == "Oct")
squid.Nov20 <- squid20 %>% filter(Month.nameND == "Nov")
squid.Dec20 <- squid20 %>% filter(Month.nameND == "Dec")

#Histograms for Each Month
squid.Jan20.hist = ggplot(squid.Jan20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="January", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Jan20.hist
squid.Feb20.hist = ggplot(squid.Feb20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="Feburary", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Feb20.hist
squid.Mar20.hist = ggplot(squid.Mar20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="March (ND)", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Mar20.hist
squid.Apr20.hist = ggplot(squid.Apr20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="April (ND)", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Apr20.hist
squid.May20.hist = ggplot(squid.May20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "blue", color = "#e9ecef", binwidth = 1)+
  labs(title="May (ND)", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.May20.hist
squid.Jun20.hist = ggplot(squid.Jun20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="June", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Jun20.hist
squid.Jul20.hist = ggplot(squid.Jul20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="July (ND)", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Jul20.hist
squid.Aug20.hist = ggplot(squid.Aug20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="August", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Aug20.hist
squid.Sep20.hist = ggplot(squid.Sep20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="September", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Sep20.hist
squid.Oct20.hist = ggplot(squid.Oct20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="October", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Oct20.hist
squid.Nov20.hist = ggplot(squid.Nov20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="November", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Nov20.hist
squid.Dec20.hist = ggplot(squid.Dec20, aes(x=Gladius.Length))+
  geom_histogram(show.legend=F, fill = "red", color = "#e9ecef", binwidth = 1)+
  labs(title="December", y="Abundance", x = "Gladius Length (cm)")+
  scale_x_continuous(limits=c(0,8.5)) + scale_y_continuous(limits=c(0,60))
squid.Dec20.hist

#Size Distributuion with Season(Month) Facet Wrap Plot
sizedist20.month1 = ggarrange(squid.Jan20.hist, squid.Feb20.hist, squid.Mar20.hist,
                            squid.Apr20.hist, ncol = 1, nrow = 4, align = "hv")
sizedist20.month1
sizedist20.month2 = ggarrange(squid.May20.hist, squid.Jun20.hist, squid.Jul20.hist, 
                             squid.Aug20.hist, ncol = 1, nrow = 4, align = "hv")
sizedist20.month2
sizedist20.month3 = ggarrange(squid.Sep20.hist, squid.Oct20.hist, squid.Nov20.hist, 
                              squid.Dec20.hist, ncol = 1, nrow = 4, align = "hv")
sizedist20.month3
