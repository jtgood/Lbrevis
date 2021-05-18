#STATOLITH DATA 2020#

stat20 <- read.csv("./Data/Lbrevis Statolith 2020.csv", header = TRUE)
install.packages("tidyverse")
install.packages("purrr")
install.packages("reshape2")
install.packages("FSA")
install.packages("FSAdata")
install.packages("nlstools")
library(purrr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(FSA)
library(FSAdata)
library(nlstools)
view(stat20)

#Seperate data for just Sept-Dec statoliths of 2020:
compstat <- stat20 %>% filter(Month %in% c("September", "November", "December"))
view(compstat)

#Plot Size Age Data
plot(compstat$Average.Age.Count~compstat$Gladius.length)
exp.model <- lm(Gladius.length~exp(Average.Age.Count), compstat)

statgraph <- ggplot(aes(x = Average.Age.Count, y = Gladius.length), data = compstat)+ 
  geom_point(size=2, shape=16)+
  stat_smooth(method="lm",formula=y~exp(x),fill="red")+
  geom_text(x = 2, y = 300, label = eq(compstat$Gladius.length,compstat$Average.Age.Count), parse = TRUE)+
print(statgraph + labs(y="Gladius Lengths (cm)", x = "Average Age Count (Days)"))
#Equation for line and R2
#??????????????????????????????????????????????????????????????????????????????

#Log transformed Y model 
ggplot(data=compstat, aes(x=Average.Age.Count, y=log(Gladius.lengthmm)))+
  geom_point()+geom_smooth(method="lm", se=TRUE)+
  theme_classic()#better theme for publication, removes background gray gridline in ggplot
#manually calculate confidence interval if I need to report outside of a graph
#May have to change method=LM if wanting to fit exponetial 
  
######################VBGF For Squid Statoliths################################  
#VBGF does not fit the data.  Due to squid's excelerating growth, an exponential
#model is more appropriate to fit data: See Jackson et al., 1997.  Took a while 
#to figure out: Make sure Age is on X axis and size on Y.  This is the accepted
#way to show such things. 

#Calculate starting VBGF from data
squidsvTypical <- vbStarts(Gladius.length~Average.Age.Count, data = compstat)
unlist(squidsvTypical)####Warning occured, does not fit the VBGF model 
#Fit Parameters





