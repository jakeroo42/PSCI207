##############################
##PSCI 207: Applied Data Science
##Problem Set 2
##[Jake Rabinowitz]
##############################

library(dplyr)
library(tidyr)
library(rio)
library(gapminder)

## If you worked with anybody else on this problem set, please list their names here:

setwd("Dropbox/Data Science/Pset 2")



#######################
## Question 1
#######################


#######
## 1A
#######

#load the data in, dataframe is named "gapminder"
data("gapminder")

#get the dimensions
dim(gapminder)

#The data set has 1704 rows and 6 columns
#The unit of analysis is Countries


#######
## 1B
#######

#using logic and bracket notation 
norwayPop.1952 <- gapminder[gapminder$country == "Norway" & gapminder$year == "1952",]$pop

#The population of Norway in 1952 was 3,327,728
norwayPop.1952

#######
## 1C
#######
#getting the correlation between life expectancy and GDP per capita
cor(gapminder$lifeExp, gapminder$gdpPercap)

#The correlation is 0.5837062, which is pretty strongly positivly correlated.
#This means that the higher the GDP, the longer life expectancy tends to be

#######
## 1D
#######

#bracket and conditional logic to see what countries have a life expenctancy under 40, 
#after the year 2000
gapminder[gapminder$year > '2000' & gapminder$lifeExp < '40.0',]$country

#There are 3 countries: Swaziland, Zambia, and Zimbabwe 
#Swaziland, 2007 lifeExp = 39.6
#Zambia, 2002 lifeExp = 39.2
#Zimbabwe, 2002 lifeExp = 40.0 (rounded), actual number is 39.989

#######
## 1E
#######

#getting the 
data.1977 <- data.frame(gapminder[gapminder$year == '1977',])
data.2007 <- data.frame(gapminder[gapminder$year == '2007',])

totalPopData <- merge(data.1977, data.2007, by =  'country')
totalPopData <- totalPopData[c(1,3,4,8,9)]

countryPopRate <- totalPopData[totalPopData$lifeExp.x > totalPopData$lifeExp.y,]


#getting the % of countries whose population went down from 1977 to 2007,
#using the number of rows in each data frame, which has a country column, 
#14 out of 142 to be exact

percent <- (nrow(countryPopRate) / nrow(data.1977)) * 100

#9.86% of countries had a longer life expectancy in 1977 than in 2007, these 
#countries are: Botswana, Central African Republic, Congo, Dem. Rep., Congo, Rep., 
#Cote d'Ivoire, Iraq, Kenya, Lesotho, Mozambique, Namibia, South Africa, Swaziland,
#Zambia, Zimbabwe, the data frame is called "countryPopRate"


#######################
## Question 2
#######################


#######
## 2A
#######

genfoward <- import("genforward sept 2017.sav")
genfoward.untouched <- genfoward

#looking at attributes for Q1, need to find all people who voted 1 or 2

genfowardForTrump <- genfoward[genfoward$Q1 == 1 | genfoward$Q1 == 2,]

perForTrump <- (nrow(genfowardForTrump) / nrow(genfoward.untouched)) * 100

#14.7 people that took the survey voted that they approve or somewhat approve of the
#way Trump is handling his job

#######
## 2B
#######

#First check attributes of PartyID: lean rep = 5, mod rep = 6, strong rep = 7 

genfRep <- genfoward[genfoward$PartyID7 == 5 | genfoward$PartyID7 == 6| genfoward$PartyID7 == 7,]

#cleaned the data frame to only columns I need to answer the question
genfRepClean <- genfRep[c("GenF_ID", "Q1","PartyID7","gender")]

#gender 1 = male, gender 2 = female
genfRep.Male <- genfRepClean[genfRepClean$gender == 1,]
genfRep.Female <- genfRepClean[genfRepClean$gender == 2,]

nrow(genfRep.Male) #there are 225 republican males
nrow(genfRep.Female) #there are 121 republican females

#for approved 
#looking at attributes for Q1, need to find all people who voted 1 or 2
repMale.Approve <- genfRep.Male[genfRep.Male$Q1 == 1 | genfRep.Male$Q1 == 2,]
repFemale.Approve <- genfRep.Female[genfRep.Female$Q1 == 1 | genfRep.Female$Q1 == 2,]

perRepMaleApproveTrump <- ((nrow(repMale.Approve) / (nrow(genfRep.Male)))) * 100
perRepFemaleApproveTrump <- ((nrow(repFemale.Approve) / (nrow(genfRep.Female)))) * 100

#52.4% of republican men voted they somewhat or strongly approve of the way Trump is doing his job
#45.5% of republican women voted they somewhat or strongly approve of the way Trump is doing his job

#for disapproved
#looking at attributes for Q1, need to find all people who voted 4 or 5
repMale.Dispprove <- genfRep.Male[genfRep.Male$Q1 == 4 | genfRep.Male$Q1 == 5,]
repFemale.Dispprove <- genfRep.Female[genfRep.Female$Q1 == 4 | genfRep.Female$Q1 == 5,]

perRepMaleDispproveTrump <- ((nrow(repMale.Dispprove) / (nrow(genfRep.Male)))) * 100
perRepFemaleDispproveTrump <- ((nrow(repFemale.Dispprove) / (nrow(genfRep.Female)))) * 100

#33.1% of republican men voted they somewhat or strongly disapprove of the way Trump is doing his job
#38.0% of republican women voted they somewhat or strongly disapprove of the way Trump is doing his job

#######
## 2C
#######

#Only looking people who voted for Trump 
genfoward.Trump <- genfoward[genfoward$Q0 == 2,]

#Gathering all the Q13 questions
genfowardClean.Trump <- gather(genfoward.Trump,
                               key = "top.issue", value = "val", 
                               starts_with("Q13_"))
#Removing all the 0's 
genfowardClean.Trump <- genfowardClean.Trump[genfowardClean.Trump$val != 0,]

#Removing the val colum
genfowardClean.Trump$val <- NULL

#Grouping by and counting the most important problems facing America
countIssues <- data.frame(genfowardClean.Trump %>% group_by(top.issue) %>% tally())

#Ranking the issues from most to least
orderCountIssues <- countIssues[order(-countIssues$n),]

#The top 2 issues that 2016 Trump voters considered to be facing this country are:
#Q13_21, Q13_6, or Terrorism and homeland security and Heath care

(orderCountIssues[1,2]/ nrow(genfowardClean.Trump)) * 100
#43 out of the total people voted for Trump listed Terrorism and homeland security as 
#the number 1 most important problem or 18.2%

(orderCountIssues[2,2] / nrow(genfowardClean.Trump)) * 100
#25 out of the total people voted for Trump listed Health care as 
#the number 1 most important problem or 10.6%


#######
## 2D
#######

#looking at just Clinton Voters
genfoward.Clinton<- genfoward[genfoward$Q0 == 1,]

#see how many clinton voters put 1 for Q13_6 or Q13_21
clinton.HealthCare <- genfoward.Clinton[genfoward.Clinton$Q13_6 == 1,]
clinton.Terrorism <- genfoward.Clinton[genfoward.Clinton$Q13_21 == 1,]

numHC <- nrow(clinton.HealthCare) #116 put Health care as their top choice
numTerror <- nrow(clinton.Terrorism) #38 put Terrorism and homeland security

perClintonVoters <- ((numHC + numTerror) / nrow(genfoward.Clinton)) * 100

#A total of 18.1% of people who voted for Clinton put either Health care or 
#Terrorism and homeland security as the number 1 problem facing the nation

#######
## 2E
#######

###First clean data frame to make it only women over 30###
genfoward.Women <- genfoward[genfoward$gender == 2,]
genf.Women.More30 <- genfoward.Women [genfoward.Women$age > 30,]

#Gathering all the Q13 questions
genfClean.Women.More30 <- gather(genf.Women.More30,
                               key = "top.issue", value = "val", 
                               starts_with("Q13_"))
#Removing all the 0's 
genfClean.Women.More30 <- genfClean.Women.More30[genfClean.Women.More30$val != 0,]

#Removing the val colum
genfClean.Women.More30$val <- NULL

#Grouping by and counting the most important problems facing America
countIssues.WomenMore30 <- data.frame(genfClean.Women.More30 %>% group_by(top.issue) %>% tally())

#Ranking the issues from most to least
orderCountIssues.WomenMore30 <- countIssues.WomenMore30[order(-countIssues.WomenMore30$n),]

#The top 3 issues that women over 30 care about are: Q13_6, Q13_14, Q13_21, or Health care, Racism,
#and Terrorism and homeland security, respectively. 


###Looking at women 30 and under###
genf.Women.Less30 <- genfoward.Women[genfoward.Women$age <= 30,]

#Gathering all the Q13 questions
genfClean.Women.Less30 <- gather(genf.Women.Less30,
                                 key = "top.issue", value = "val", 
                                 starts_with("Q13_"))
#Removing all the 0's 
genfClean.Women.Less30 <- genfClean.Women.Less30[genfClean.Women.Less30$val != 0,]

#Removing the val colum
genfClean.Women.Less30$val <- NULL

#Grouping by and counting the most important problems facing America
countIssues.WomenLess30 <- data.frame(genfClean.Women.Less30 %>% group_by(top.issue) %>% tally())

#Ranking the issues from most to least
orderCountIssues.WomenLess30 <- countIssues.WomenLess30[order(-countIssues.WomenLess30$n),]

#The top 3 issues that women 30 and under care about are: Q13_14, Q13_6, Q13_3, or Racism, Health care, 
#and Environment and climate change, respectively. They care about roughly the same issues,
#however women under 30 look like they care more about the environment than terrorism 






