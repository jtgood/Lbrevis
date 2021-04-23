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
#?????????????????????????????????????????????????????????????
statgraph <- ggplot(aes(x = Gladius.length, y = Average.Age.Count), data = compstat)+ 
  geom_point(size=2, shape=16)+
  stat_smooth(method="lm",formula=y~log(x),fill="red")+
  geom_text(x = 2, y = 300, label = eq(compstat$Gladius.length,compstat$Average.Age.Count), parse = TRUE)
print(statgraph + labs(y="Average Age Count (Days)", x = "Gladius Lengths (cm)"))
#Equation for line and R2
eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}
#??????????????????????????????????????????????????????????????????????????????
  
######################VBGF For Squid Statoliths################################  

#Calculate starting VBGF from data
squidsvTypical <- vbStarts(Gladius.lengthmm~Average.Age.Count, data = compstat)
unlist(squidsvTypical)####Warning occured, units off by magnitude? 
#Fit Parameters
vbTypical <- Gladius.lengt~Linf*(1-exp(-K*(Average.Age.Count-t0)))
fitTypical <- nls(vbTypical, data=compstat, start=svTypical)
#Plot and Sumamry
fitPlot(fitTypical,xlab="Average Age Count",ylab="Gladius Length (mm)",main="")
overview(fitTypical)
#Bootstrapping
bootTypical <- nlsBoot(fitTypical, niter=1000)
confint(bootTypical, plot=TRUE)
#P-value testing
htest(bootTypical, "K", 0.5, "less") #Not enough evidence to conclude
#that K is liess than 0.5

#Example of creating corresponding confidence intervals with bootstrap values
#Prediction function allows for estimation of length at a given age:
new <- data.frame(age=8)
predict(fitTypical, new)
ests <- bootTypical$coefboot
pv <- ests[,"Linf"]*(1-exp(-ests[,"K"]*(8-ests[,"t0"])))
quantile(pv,c(0.025,0.975))
#340.2 - 359.9 = mean length of all age 8 male croaker is between these numbers

#Plot confidence intervals on VBGF plot
#Uses example of age range 0-15 (ages2plot)
ages2plot <- 0:15
fitPlot(fitTypical,xlab="Age",ylab="Total Length (mm)",xlim=range(ages2plot),
        main="")
LCI <- UCI <- numeric(length(ages2plot))
for (i in 1:length(ages2plot)) {
  pv <- ests[,"Linf"]*(1-exp(-ests[,"K"]*(ages2plot[i]-ests[,"t0"])))
  LCI[i] <- quantile(pv,0.025)
  UCI[i] <- quantile(pv,0.975)
}
lines(UCI~ages2plot, type="l", col="blue", lwd=2, lty=2)
lines(LCI~ages2plot, type="l", col="blue", lwd=2, lty=2)
#Plots VBGF With 95% Bootstrap Confidence Interval Lines (Blue Lines) 

fitPlot(fitTypical,xlab="Age",ylab="Total Length (mm)",xlim=range(ages2plot),main="")
LCI <- UCI <- LPI <- UPI <- numeric(length(ages2plot))
for (i in 1:length(ages2plot)) {
  pv <- ests[,"Linf"]*(1-exp(-ests[,"K"]*(ages2plot[i]-ests[,"t0"])))
  LCI[i] <- quantile(pv,0.025)
  UCI[i] <- quantile(pv,0.975)
  LPI[i] <- quantile(pv-bootTypical$rse,0.025)
  UPI[i] <- quantile(pv+bootTypical$rse,0.975)
}
lines(UCI~ages2plot,type="l",col="blue",lwd=2,lty=2)
lines(LCI~ages2plot,type="l",col="blue",lwd=2,lty=2)
lines(UPI~ages2plot,type="l",col="red",lwd=2,lty=2)
lines(LPI~ages2plot,type="l",col="red",lwd=2,lty=2)
#Plots VBGF With 95% Bootstrap Confidence Interval Lines (Blue Lines) and 95%
#prediction bounds (Red Lines)
#Scatter plots of bootstrap parameters of the above graphs
plot(bootTypical)
#Results further illustrate the strong correlation between pairs of the parameters.

##Assumption Checking 
residPlot(fitTypical)
hist(residuals(fitTypical), main="")



