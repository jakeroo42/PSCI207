##############################
##PSCI 207: Applied Data Science
##Problem Set 4
##[Jake Rabinowitz]
##############################

## If you worked with anybody else on this problem set, please list their names here:
#Max Weissberg

require(rio)
require(tidyr)
require(purrr)
require(weights)
require(dplyr)

setwd("Dropbox/Data Science/problem set 4/")


#######################
## Question 1
#######################

#######
## 1A
#######

randomBDay <- sample(c(1:365), 500, replace = TRUE)
jan1 <- 1
jan1 %in% randomBDay
#Becuase that line returned true, that means at least one person has a birthday on jan1

#######
## 1B
#######

#initialize empty january 1 list 
jan1List <- list()

#For loop to to run random birthday list 1000 times
for (i in 1:1000) {
  is.Jan1 <- jan1 %in% sample(c(0:365), 500, replace = TRUE)
  jan1List <- append(jan1List, is.Jan1)
}


#######
## 1C
#######

#Turning the true false list into numeric to take average
jan1List.numeric <- sapply(jan1List, as.numeric)

pct.Jan1 <- (1- mean(jan1List.numeric) )*100

#There is a 26.4% chance that no one in the group will have a 
#birthday on jan 1, from a group of 500 people

#######
## 1D
#######

#Function to determine chance of someone not having a birthday on January 1
#Input: number of people you want to in sample
#Output: a percent of the chance no one in the sample has a birthday on January 1
jan1 <- 1

birthday <- function(numPeople){
  jan1List <- list()
  for (i in 1:1000) {
    is.Jan1 <- jan1 %in% sample(c(1:365), numPeople, replace = TRUE)
    jan1List <- append(jan1List, is.Jan1)
  }
  jan1List.numeric <- sapply(jan1List, as.numeric)
  pct.not.Jan1 <- (1- mean(jan1List.numeric)) * 100
  return(pct.not.Jan1)
}


birthday(1000)
#With 1000 people there is a 4.9% chance no one will have a birthday on January 1

#######
## 1E (Optional)
#######

probList <- list()
bdayVector <- c(500:1500)

for (numPeople in bdayVector){
  probList <- append(probList, birthday(numPeople))
}

plot(bdayVector,probList, main = "Probability That No One Has a Birthday on January 1 for 500 to 1500 People", 
     ylab = "Percent Chance Someone does not Have a Bithday on January 1",
     xlab = "Number of People ", bty = "l")


#######################
## Question 2
#######################

#######
## 2A
#######

#Loading the zip file and creating one data frame 
vector <- dir(pattern ="pop-density-")
popDensityMap <- map(vector, read.csv, stringsAsFactors = F)
completePopDensity <- bind_rows(popDensityMap)
dim(completePopDensity)

#The complete data frame has 3140 rows and 5 columns

#######
## 2B
#######

#Loading in presidential vote results

presVoteResults <- import("2016-president-results.csv")

dim(presVoteResults)
#The presVoteResults data frame has 4639 rows and 9 columns 
#The reason there is more rows in the presVoteResults is because that data frame includes
#jurisdiction and fipscode, but there are more juirisdictions than fipscodes, for example, 
#Maine has many jurisdictions with all the same fipscode 

#######
## 2C
#######

#grouping the presVoteResults data frame so its unit of analysis if fipscode 

presVoteResults.fips <- dplyr::summarize(group_by(presVoteResults, fipscode), 
                                  sum(trump), sum(clinton), 
                                  sum(johnson), sum(stein), sum(totalvotes),
                                  sum(other))

dim(presVoteResults.fips)
#The presVoteResults.fips data frame has a dim of 3111 rows and 7 columns

#######
## 2D
#######

#merge the presVoteResults.fips with the completePopDensity data frame

names(presVoteResults.fips)[1] <- "Fipscode" 
merged <- merge(completePopDensity, presVoteResults.fips, by = "Fipscode")

#merge on fipscode 
#doing a full/ outer merge 

dim(merged)
#data set has 3111 rows and 11 columns

#######
## 2E
#######

#add the three new columns 

#new column on population density
merged$popDensity <- merged$Population / merged$Land.Area.in.square.miles

#new column on Trumps total votes
merged$trumpOverallVote <- merged$`sum(trump)` / merged$`sum(totalvotes)`

#new column on 
merged$twoParty <- merged$`sum(trump)` / (merged$`sum(trump)` + merged$`sum(clinton)`)

#calculating the means 
mean(merged$popDensity, na.rm = T)
mean(merged$trumpOverallVote, na.rm = T)
mean(merged$twoParty, na.rm = T)


#mean pop density = 258.4554
#mean trump overall = 63.26%
#mean two party = 66.68%

#######
## 2F
#######

trumpOverallVote.weighted <- weighted.mean(merged$trumpOverallVote, merged$`sum(totalvotes)`, na.rm = T)
#mean of trump overall ove weighted with total votes is 46.2%

#######
## 2G
#######

#graph the log of the population density on x-axis vs Trump's vote percentage on y-axis

plot(log(merged$popDensity), merged$trumpOverallVote *100, 
     main = "Log of Population Density vs Trump Overall Vote Percentage by County", 
     xlab = "Log of Population Density", ylab = "Trump Overall Vote Percentage", col = "blue",
     cex = .5,bty = "l")

#This tells me that there is a negative correlation between population density and Trump's votes.
#In other words, Trump tends to be more favored in rural areas


#######
## 2H
#######

#Group voting data, population, and area data by state
merged.byState <- summarize(group_by(merged, State), 
                                  sum(`sum(trump)`), sum(`sum(clinton)`), 
                                  sum(Population), sum(Land.Area.in.square.miles))


#Renaming data frame
names(merged.byState) <- c("State", "Trump.Votes", "Clinton.Votes", "Population", "Area")

#Calculating new columns
merged.byState$popDensity <- merged.byState$Population / merged.byState$Area

merged.byState$twoPatry <- merged.byState$Trump.Votes /(merged.byState$Trump.Votes + 
                                                         merged.byState$Clinton.Votes)


cor(log(merged.byState$popDensity), merged.byState$twoPatry)
#The correlation by state between pop density and the Rep 2-Party vote percentage is -0.611

cor(log(merged$popDensity), merged$twoParty)
#The correlation by county between pop density and the Rep 2-Party vote percentage is -0.493

#These correlations are close but the one by state is much more correlated. This tells us that the 
#unit of analysis is very important to look at because if you didn't know the unit of analysis 
#then the results would not have any meaning. 


## Question 3
#######################

#######
## 3A
#######

#loading the July 2019 data in

july2019 <- import("july-2019-sm-poll.sav")

mean(july2019$weight)

#The mean is 1. If the average wasnt 1 then that would skew the data because that would mean
#that, as a whole, the average person is under or over represented.


#######
## 3B
#######

min(july2019$weight)
max(july2019$weight)

#The data appears to be trimmed at 7. I know this because the max weight value is an integer, when
#normally the weights are not round numbers.


#######
## 3C
#######

#Finding the person with the weight of 7 
july2019[which(july2019$weight == max(july2019$weight)),]

#The person with respondentID 10849564995 is weighted at a 7. And identifies as an hispanic female
#and they are weighed heavily because there are fewer hispanic femlale proportional to ther race
#and gender combinations


#######
## 3D
#######

mean(july2019$age, na.rm = T)
#The unweighted average is 52.1 years old 

weighted.mean(july2019$age, july2019$weight, na.rm = T)
#The weighted averge age is 45 years old 

#Because the weighted average age is lower than the unweighted average age, this tells me that there 
#was a disproportionate amount of young people that took the survey in relation the actual
#to national age distribution

#The difference in weighted to unweighted age is 7.1 years

abs(weighted.mean(july2019$age, july2019$weight, na.rm = T) - mean(july2019$age, na.rm = T))

#######
## 3E
#######

attributes(july2019$race)
#white = 1; black = 2; hispanic = 3; asian = 4; other = 5

#Unweighted percentage of people who took the survey 
wpct(july2019$race, weight=NULL, na.rm=TRUE)

#Weighted percentage of people who took the survey 
wpct(july2019$race, weight= july2019$weight, na.rm=TRUE)

#######
## 3F
#######

attributes(july2019$taxes_improve_infrastructure)
#1 = Very willing, 2 = Somewhat willing, 5 = No answer

#Somewhat or very willing to pay higher taxes to pay for infrastructure improvements
wpct(july2019$taxes_improve_infrastructure, weight= july2019$weight, na.rm=TRUE)[[1]] +
  wpct(july2019$taxes_improve_infrastructure, weight= july2019$weight, na.rm=TRUE)[[2]]

#57.1% of people, both democrats and republicans are very willing or somewhat willing to pay 
#more taxes if it went to improving infrestructure

attributes(july2019$party)
#1 = rep, 3 = dem 

#Republicans
july2019.rep <- july2019[july2019$party == 1,]

wpct(july2019.rep$taxes_improve_infrastructure, weight= july2019.rep$weight, na.rm=TRUE)[[1]] +
  wpct(july2019.rep$taxes_improve_infrastructure, weight= july2019.rep$weight, na.rm=TRUE)[[2]]

#48.9% of republicans are very willing or somewhat willing to pay more taxes if 
#it went to improving infrestructure

#Democrats
july2019.dem <- july2019[july2019$party == 3,]

wpct(july2019.dem$taxes_improve_infrastructure, weight= july2019.dem$weight, na.rm=TRUE)[[1]] +
  wpct(july2019.dem$taxes_improve_infrastructure, weight= july2019.dem$weight, na.rm=TRUE)[[2]]

#70.0% of democrats are very willing or somewhat willing to pay more taxes if 
#it went to improving infrestructure

#######
## 3G
#######

#I took the median age of the july201 and got 54, so I looked at trust in the fed and state gov
#for people 54 and over and people 53 and younger.
median(july2019$age)

attributes(july2019$trust_fed_gov)
attributes(july2019$trust_state_gov)
#Fed: 1 = Just about always, 2 = Most of the time, 5 = Almost Never 
#State: 1 = Just about always, 2 = Most of the time, 5 = Almost Never 

#Federal government trust for all ages
wpct(july2019$trust_fed_gov, weight= july2019$weight, na.rm=TRUE)[[1]] +
  wpct(july2019$trust_fed_gov, weight= july2019$weight, na.rm=TRUE)[[2]]

wpct(july2019$trust_fed_gov, weight= july2019$weight, na.rm=TRUE)[[5]]
#14.8% of Americans almost always or most of the time trust the federal gov, and 28.1% almost never trust
#the federal gov

#Federal government trust 53 and younger

july2019.younger53 <- july2019[july2019$age <= 53,]

wpct(july2019.younger53$trust_fed_gov, weight= july2019.younger53$weight, na.rm=TRUE)[[1]] +
  wpct(july2019.younger53$trust_fed_gov, weight= july2019.younger53$weight, na.rm=TRUE)[[2]]

wpct(july2019.younger53$trust_fed_gov, weight= july2019.younger53$weight, na.rm=TRUE)[[5]]

#16.0% of Americans 53 and younger almost always or most of the time trust the federal gov, 
#and 27.9% almost never trust the federal gov


#Federal government trust 54 and older

july2019.older54 <- july2019[july2019$age >= 54,]

wpct(july2019.older54$trust_fed_gov, weight= july2019.older54$weight, na.rm=TRUE)[[1]] +
  wpct(july2019.older54$trust_fed_gov, weight= july2019.older54$weight, na.rm=TRUE)[[2]]

wpct(july2019.older54$trust_fed_gov, weight= july2019.older54$weight, na.rm=TRUE)[[5]]

#12.4% of Americans 54 and older almost always or most of the time trust the federal gov, 
#and 28.4% almost never trust the federal gov

#State government trust for all ages
wpct(july2019$trust_state_gov, weight= july2019$weight, na.rm=TRUE)[[1]] +
  wpct(july2019$trust_state_gov, weight= july2019$weight, na.rm=TRUE)[[2]]

wpct(july2019$trust_state_gov, weight= july2019$weight, na.rm=TRUE)[[5]]

#22.9% of all Americans almost always or most of the time trust the state gov, 
#and 22.3% almost never trust the state gov

#State government trust 53 and younger

wpct(july2019.younger53$trust_state_gov, weight= july2019.younger53$weight, na.rm=TRUE)[[1]] +
  wpct(july2019.younger53$trust_state_gov, weight= july2019.younger53$weight, na.rm=TRUE)[[2]]

wpct(july2019.younger53$trust_state_gov, weight= july2019.younger53$weight, na.rm=TRUE)[[5]]

#22.6% of Americans 53 and younger almost always or most of the time trust the state gov, 
#and 22.8% almost never trust the state gov

#State government trust 54 and older

wpct(july2019.older54$trust_state_gov, weight= july2019.older54$weight, na.rm=TRUE)[[1]] +
  wpct(july2019.older54$trust_state_gov, weight= july2019.older54$weight, na.rm=TRUE)[[2]]

wpct(july2019.older54$trust_state_gov, weight= july2019.older54$weight, na.rm=TRUE)[[5]]

#23.4% of Americans 54 and older almost always or most of the time trust the state gov, 
#and 21.3% almost never trust the state gov

#I found that younger people, the younger half of the data set, tended to have more trust for 
#the federal gov, while the reverse is true for the state gov. I think one way I could improve
#upon my preliminary data analysis is run the same code but for age groups 10 years apart.
#That way, I could get a better understanding of which age groups tend to trust the gov
#the most and the least


