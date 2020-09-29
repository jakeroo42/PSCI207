#Final Project Data 
#Jake Rabinowitz 
#PSCI 207

library(dbplyr)
library(tidyr)
library(rio)
library(weights)

setwd("Dropbox/Data Science/Final project/")

oct2019.gen <- import("psci207 - fall 2019 final project data/2019 october - nbc south poll.sav")
oct2019.state <- import("2019 october - nbc south poll - national and regional.sav")

AL <- oct2019.state[oct2019.state$state == 1,]
MS <- oct2019.state[oct2019.state$state == 25,]
GA <- oct2019.state[oct2019.state$state == 11,]
TN <- oct2019.state[oct2019.state$state == 43,]


#Issues in America By Percent Graph

##########
overall.graph <- function(){
  t <- sort(wpct(oct2019.gen$issue_matters_most, weight = oct2019.gen$weight_natl) * 100)
  
  color <- c("Royalblue1","Royalblue1","Royalblue1","Royalblue1","Royalblue1",
             "Royalblue1","Royalblue1","Royalblue1", "Royalblue1", "firebrick2")
  bplot <- barplot(t, col = color, xlab = "Issue", 
                   main = "What People Chose as Their Most Important\n Issue Facing America",  
                   ylim= c(0, 25), names.arg= c("No Answer","Foreign Policy","Terrorism", 
                                              "Other","Education","Immigration", "The Environment","Government\n Ethics",
                                              "Health Care","Jobs and\n the Economy"), yaxt ='n')
  text(bplot, t + 1.5 , paste(round(t, 1), '%', sep="") ,cex=1.4, col = "black") 
  
  axis(2,
       at = seq(0,25,5),
       labels = paste0(seq(0,25,5), "%"),
       las = 2)
  legend("topleft", legend="n = 20,701",
         col="red", cex=1.5, lty = 0, bty = "n")
}

#Issues in the 4 Main States, MS, AL, GA, TN

color = c("firebrick2", "Royalblue1", "Royalblue1")

###Alabama 
AL.graph <- function(){
  AL <- oct2019.state[oct2019.state$state == 1,]
  AL.data <- sort(wpct(AL$al_priority, weight = AL$weight_AL_genpop) * 100, decreasing = T)[c(1:3)]
  bplot.AL <- barplot(AL.data, horiz = T, main = "Top Issues in Alabama", 
          col = color, xaxt = "n",
          names.arg = c("Jobs/\n Economy", "Education", "Healthcare"), xlim = c(0, 45), cex.names = .85)
  
  axis(1,
       at = seq(0,45,5),
       labels = paste0(seq(0,45,5), "%"),
       las = 1)
  text(AL.data+1.5, bplot.AL, paste( round(AL.data, 1), "%", sep="") ,cex=1.4) 
  
}

###Mississippi
MS.graph <- function(){
  MS <- oct2019.state[oct2019.state$state == 25,]
  MS.data <- sort(wpct(MS$ms_priority, weight = MS$weight_MS_genpop) * 100, decreasing = T)[c(1:3)]
  bplot.MS <- barplot(MS.data, horiz = T, main = "Top Issues in Mississippi", 
                      col = color, xaxt = "n",
                      names.arg = c("JJobs/\n Economy", "Education", "Healthcare"), xlim = c(0, 45), cex.names = .85)
  
  axis(1,
       at = seq(0,45,5),
       labels = paste0(seq(0,45,5), "%"),
       las = 1)
  text(MS.data+1.5, bplot.MS, paste(round(MS.data, 1), "%", sep="") ,cex=1.4) 
  
}

###Georgia 
GA.graph <- function(){
  GA <- oct2019.state[oct2019.state$state == 11,]
  GA.data <- sort(wpct(GA$ga_priority, weight = GA$weight_GA_genpop) * 100, decreasing = T)[c(1:3)]
  bplot.GA <- barplot(GA.data, horiz = T, main = "Top Issues in Georgia", 
                      col = color, xaxt = "n",
                      names.arg = c("Jobs/\n Economy", "Healthcare", "Education"), xlim = c(0, 45), cex.names = .85)
  
  axis(1,
       at = seq(0,45,5),
       labels = paste0(seq(0,45,5), "%"),
       las = 1)
  text(GA.data+1.5, bplot.GA, paste(round(GA.data, 1), "%", sep="") ,cex=1.4) 
  
}

###Tennessee
TN.graph <- function(){
  TN <- oct2019.state[oct2019.state$state == 43,]
  TN.data <- sort(wpct(TN$tn_priority, weight = TN$weight_TN_genpop) * 100, decreasing = T)[c(1:3)]
  bplot.TN <- barplot(TN.data, horiz = T, main = "Top Issues in Tennessee", 
                      col = color, xaxt = "n",
                      names.arg = c("Jobs/\n Economy", "Healthcare", "Education"), xlim = c(0, 45), cex.names = .85)
  
  axis(1,
       at = seq(0,45,5),
       labels = paste0(seq(0,45,5), "%"),
       las = 1)
  text(TN.data+1.5, bplot.TN, paste(round(TN.data, 1), "%", sep="") ,cex=1.4) 
  
}

layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))
overall.graph()
TN.graph()
GA.graph()
AL.graph()
MS.graph()


##########

#What people think of the national economy and the US economy

#AL
AL.Econ.Graph <- function(){
  AL.Econ.nat <- wpct(AL$conditon_national_economy, weight = AL$weight_AL_genpop)[c(1:4)] * 100
  AL.Econ.al <- wpct(AL$al_economy_rate, weight = AL$weight_AL_genpop)[c(1:4)] * 100
  AL.plot <- barplot(rbind(AL.Econ.nat, AL.Econ.al), main="Condition of the Alabama State Economy Vs National Economy\n Rated by the Citizens of Alabama", 
                     col=c("firebrick", "dodgerblue1"), beside=T, 
                     names.arg = c("Very Good", "Fairly Good", "Fairly Bad", "Very Bad"), 
                     yaxt = 'n', ylim = c(0,60))
  axis(2,
       at = seq(0,55,5),
       labels = paste0(seq(0,55,5), "%"),
       las = 2)
  text(AL.plot[1,], AL.Econ.nat+3.5, paste(round(AL.Econ.nat, 1), "%", sep="") ,cex=1, srt = 45) 
  text(AL.plot[2,], AL.Econ.al+3.5, paste(round(AL.Econ.al, 1), "%", sep="") ,cex=1, srt = 45) 
  
  legend("topright", inset= c(-.3, .2),
         c("Alabama","America"), fill=c("firebrick", "dodgerblue1"), horiz=F, cex=0.8, bty = 'n')
}

#MS
MS.Econ.Graph <- function(){
  MS.Econ.nat <- wpct(MS$conditon_national_economy, weight = MS$weight_MS_genpop)[c(1:4)] * 100
  MS.Econ.ms <- wpct(MS$ms_economy_rate, weight = MS$weight_MS_genpop)[c(1:4)] * 100
  MS.plot <- barplot(rbind(MS.Econ.nat, MS.Econ.ms), main="Condition of the Mississippi State Economy Vs National Economy\n Rated by the Citizens of Mississippi", 
                     col=c("firebrick", "dodgerblue1"), beside=T, 
                     names.arg = c("Very Good", "Fairly Good", "Fairly Bad", "Very Bad"), 
                     yaxt = 'n', ylim = c(0,60))
  axis(2,
       at = seq(0,55,5),
       labels = paste0(seq(0,55,5), "%"),
       las = 2)
  text(MS.plot[1,], MS.Econ.nat+3.5, paste(round(MS.Econ.nat, 1), "%", sep="") ,cex=1, srt = 45) 
  text(MS.plot[2,], MS.Econ.ms+3.5, paste(round(MS.Econ.ms, 1), "%", sep="") ,cex=1, srt = 45) 
  
  legend("topright", inset= c(-.3, .2),
         c("Mississippi","America"), fill=c("firebrick", "dodgerblue1"), horiz=F, cex=0.8, bty = 'n')
}

#GA
GA.Econ.Graph <- function(){
  GA.Econ.nat <- wpct(GA$conditon_national_economy, weight = GA$weight_GA_genpop)[c(1:4)] * 100
  GA.Econ.ga <- wpct(GA$ga_economy_rate, weight = GA$weight_GA_genpop)[c(1:4)] * 100
  GA.plot <- barplot(rbind(GA.Econ.nat, GA.Econ.ga), main="Condition of the Georgia State Economy Vs National Economy\n Rated by the Citizens of Georgia", 
                     col=c("firebrick", "dodgerblue1"), beside=T, 
                     names.arg = c("Very Good", "Fairly Good", "Fairly Bad", "Very Bad"), 
                     yaxt = 'n', ylim = c(0,65))
  axis(2,
       at = seq(0,65,5),
       labels = paste0(seq(0,65,5), "%"),
       las = 2)
  text(GA.plot[1,], GA.Econ.nat+3.5, paste(round(GA.Econ.nat, 1), "%", sep="") ,cex=1, srt = 45) 
  text(GA.plot[2,], GA.Econ.ga+3.5, paste(round(GA.Econ.ga, 1), "%", sep="") ,cex=1, srt = 45) 
  
  legend("topright", inset= c(-.3, .2),
         c("Georgia","America"), fill=c("firebrick", "dodgerblue1"), horiz=F, cex=0.8, bty = 'n')
}

#TN
TN.Econ.Graph <- function(){
  TN.Econ.nat <- wpct(TN$conditon_national_economy, weight = TN$weight_TN_genpop)[c(1:4)] * 100
  TN.Econ.tn <- wpct(TN$tn_economy_rate, weight = TN$weight_TN_genpop)[c(1:4)] * 100
  TN.plot <- barplot(rbind(TN.Econ.nat, TN.Econ.tn), main="Condition of the Tennessee State Economy Vs National Economy\n Rated by the Citizens of Tennessee", 
                     col=c("firebrick", "dodgerblue1"), beside=T, 
                     names.arg = c("Very Good", "Fairly Good", "Fairly Bad", "Very Bad"), 
                     yaxt = 'n', ylim = c(0,65))
  axis(2,
       at = seq(0,65,5),
       labels = paste0(seq(0,65,5), "%"),
       las = 2)
  text(TN.plot[1,], TN.Econ.nat+3.5, paste(round(TN.Econ.nat, 1), "%", sep="") ,cex=1, srt = 45) 
  text(TN.plot[2,], TN.Econ.tn+3.5, paste(round(TN.Econ.tn, 1), "%", sep="") ,cex=1, srt = 45) 
  
  legend("topright", inset= c(-.3, .2),
         c("Tennessee","America"), fill=c("firebrick", "dodgerblue1"), horiz=F, cex=0.8, bty = 'n')
}

par(mfrow=c(2,2))

TN.Econ.Graph()
GA.Econ.Graph()
MS.Econ.Graph()
AL.Econ.Graph()





###########
#Looking at my economy metrics 

#Order goes TN, GA, AL, MS, USA

#Unemployment data frame
unEm.rateOct <- c(3.4,3.4,2.8,5.5,3.6)
unem.rate2018 <- c(3.5,3.9,3.9,4.8,3.9)
unEm.rate <- rbind(unem.rate2018, unEm.rateOct)

#Personal income data frame
PI.rate18Q3Q4 <- c(4.2, 4.6, 5.7, 5.9, 5.2)
PI.rate18Q419Q1 <- c(3.7, 4.6, 4.5, 3.6, 3.4)
PI.rate19Q1Q2 <- c(5.0,4.8,4.9,3.3, 5.4) 
PI.rate <- rbind(PI.rate18Q3Q4, PI.rate18Q419Q1, PI.rate19Q1Q2)

#GDP data frame
GDP.rate18Q3Q4 <- c(1.6, 2.1, 2.1, 0.5, 2.2)
GDP.rate18Q419Q1 <- c(2.4, 3.1, 2.7, 1.9, 3.1)
GDP.rate19Q1Q2 <- c(1.3,1.1,1.8,2.3,2.0)
GDP.rate <- rbind(GDP.rate18Q3Q4, GDP.rate18Q419Q1, GDP.rate19Q1Q2)
 
colorPlot3 <- c("rosybrown1", "rosybrown3","rosybrown4")
color = c("lightblue", "lightblue4")

unEm.rate.graph <- function(){
  unEm.rate.plot <- barplot(unEm.rate, main="Unemployment Rate", 
                            col=color, 
                            names.arg = c("Tennessee", "Georgia", "Alabama", "Mississippi", "America"), 
                            yaxt = 'n', ylim = c(0,7), beside = T)
  axis(2,
       at = seq(0,7,1),
       labels = paste0(seq(0,7,1), "%"),
       las = 2)
  text(unEm.rate.plot[2,], unEm.rate[2,]+.5, paste(unEm.rate[2,], "%", sep="") ,cex=1.5, srt = 45) 
  text(unEm.rate.plot[1,], unEm.rate[1,]+.5, paste(unEm.rate[1,], "%", sep="") ,cex=1.5, srt = 45) 
}

PI.rate.graph <- function(){
  PI.rate.plot <- barplot(PI.rate, main="Percentage Increase in Personal Income", 
                            col=colorPlot3, 
                            names.arg = c("Tennessee", "Georgia", "Alabama", "Mississippi", "America"), 
                            yaxt = 'n', ylim = c(0,7), beside = T)
  axis(2,
       at = seq(0,7,1),
       labels = paste0(seq(0,7,1), "%"),
       las = 2)
  text(PI.rate.plot[1,], PI.rate[1,]+.6, paste(PI.rate[1,], "%", sep="") ,cex=1.3, srt = 45) 
  text(PI.rate.plot[2,], PI.rate[2,]+.6, paste(PI.rate[2,], "%", sep="") ,cex=1.3, srt = 45) 
  text(PI.rate.plot[3,], PI.rate[3,]+.6, paste(PI.rate[3,], "%", sep="") ,cex=1.3, srt = 45) 
}

GDP.rate.graph <- function(){
  GDP.rate.plot <- barplot(GDP.rate, main="Percentage Increase in State GDP", 
                          col=colorPlot3, 
                          names.arg = c("Tennessee", "Georgia", "Alabama", "Mississippi", "America"), 
                          yaxt = 'n', ylim = c(0,4), beside = T)
  axis(2,
       at = seq(0,4,1),
       labels = paste0(seq(0,4,1), "%"),
       las = 2)
  text(GDP.rate.plot[1,], GDP.rate[1,]+.4, paste(GDP.rate[1,], "%", sep="") ,cex=1.3, srt = 45) 
  text(GDP.rate.plot[2,], GDP.rate[2,]+.4, paste(GDP.rate[2,], "%", sep="") ,cex=1.3, srt = 45) 
  text(GDP.rate.plot[3,], GDP.rate[3,]+.4, paste(GDP.rate[3,], "%", sep="") ,cex=1.3, srt = 45) 
}

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
unEm.rate.graph()
PI.rate.graph()
GDP.rate.graph()


#Making the legend separate
colorPlot3 <- c("rosybrown1", "rosybrown3", "rosybrown4")
color = c("lightblue", "lightblue4")
par(mfrow= c(1,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft",
       c("2018 Annual","October 2019", "2018 Q3 - Q4","2018 Q4 - 2019 Q1", "2019 Q1 - Q2"), 
       fill=c(color, colorPlot3), horiz=F, cex=1, bty = 'n')






