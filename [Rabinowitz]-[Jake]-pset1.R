##############################
##PSCI 207: Applied Data Science
##Problem Set 1
##[Jake Rabinowitz]
##############################

## If you worked with anybody else on this problem set, please list their names here:
library("dplyr")
library("tidyr")
library("ggplot2")
library("rio")

#set working directory
setwd("/Users/jakerabinowitz/Dropbox/Data Science/PSet 1")

#######################
## Question 1
#######################


#######
## 1A
#######

#read in data
recentGrads <- read.csv("recent-grads.csv") 

dim(recentGrads) #get dimension of recentGrads

#there are 173 rows and 15 columns 
#the unit of analysis is a person

#######
## 1B
#######

majorCat <- data.frame(recentGrads %>% group_by(Major_category) %>% tally())
names(majorCat)[2] <- "Count" #rename column "Count"

#ordering the dataframe from highest to lowest count
orderMajorCat <- majorCat[order(-majorCat$Count),] 

#get the max count of majors within each major category
orderMajorCat[which.max(orderMajorCat$Count),]

#get number of categories
nrow(orderMajorCat) 


#There are 16 major categories.
#Engineering has the most majors in it with 29 different majors.

#######
## 1C
#######

#removing the NA from food science major b/c there is no men/women data
recentGradsNA <- na.omit(recentGrads) 

sumWomen <- sum(recentGradsNA$Women) #sum all women
sumTotal <- sum(recentGradsNA$Total) #sum of everyone in the data set

perWomen <- (sumWomen/sumTotal) * 100

#There are a total of 3,895,228 Women
#The percentage of women is 57.5%

#######
## 1D
#######

#using the recent grads NA data set, which has the NA removed, add a column PerWomen
recentGradsNA$PerWomen <- (recentGradsNA$Women / recentGradsNA$Total) * 100

#Finding the major of the max percentage of women 
recentGradsNA[which.max(recentGradsNA$PerWomen),]

#Finding the major of the min percentage of women 
recentGradsNA[which.min(recentGradsNA$PerWomen),]

#The max percentile majors of women is Early Childhood Education and it's 96.9%
#The min percentile majors of women is Military Technologies and it's 0%

#######
## 1E
#######

#create a dataframe that counts number of people per major categry
majorCatNumPeople <- data.frame(recentGradsNA %>% 
                                  group_by(Major_category) %>% 
                                  summarise(NumPeople = sum(Total)))

#get the cell that has the health data (need to fix the hard code)
totalHealth <- majorCatNumPeople[9,2]

#create a dataframe that counts the number of people with full time jobs in each major category 
majorCatYrRound <- data.frame(recentGradsNA %>% 
                                  group_by(Major_category) %>% 
                                  summarise(FullTimeYrRound = sum(Full_time_year_round)))

#get the cell that has the health data (need to fix the hard code)
yrRoundHealth <- majorCatYrRound[9,2]

#get the percentage of year round full time jobs in health vs total health grads
perHealthGradFullTime <- (yrRoundHealth / totalHealth) * 100

#There are 463,230 people in the health sector
#49.3% of graduates in the major catagory "Health" have full time, year round jobs.

#######
## 1F
#######

#create the spread column
recentGrads$Spread <- recentGrads$P75th - recentGrads$P25th

#order the data frame from lowest to highest
orderByUnempRate <- recentGrads[order(recentGrads$Unemployment_rate),]

#only include data with unemployment rate less than 6%
unempRate6 <- orderByUnempRate[orderByUnempRate$Unemployment_rate < 0.06,]

#find max spread for unemployment rates less than 6%
maxSpread <- unempRate6[which.max(unempRate6$Spread),]


#The max spread in salary for any major with less than a 6% unemployment rate is $77500 
#and is in the major is Astronomy and Astrophysics 

#######################
## Question 2
#######################


#######
## 2A
#######

load("exit-poll-2016.RData") #load data as exit

#set exit to exitA so we can refer to exit as the orginal dataset 
exitA <- exit

#Spread the data so there is only one instance of id, and now 2 columns for favorable rating 
exitSpread <- spread(exitA, favorable.cand, favorable.rating)

#checking the number of row to makes sure there are exactly 2957 ids
nrow(exitSpread) 

#this line of code assures that there are no duplicates in 
length(unique(exitSpread$id)) == nrow(exitSpread) #will return true

#renaming some columns 
names(exitSpread)[c(11, 12)] <- c("Favorable.Clinton", "Favorable.Trump")

#The unit of observation is a voter
#There are exactly 2 rows per id 

#######
## 2B
#######

#Changing the educ columns from 1,0 to their labels, so I can use the unite function
#replace all 1's with the name of the column, and replace all 0 with blanks

exitSpread$educ.hs[exitSpread$educ.hs == 1] <- "hs"
exitSpread$educ.hs[exitSpread$educ.hs == 0] <- ""
exitSpread$educ.somecoll[exitSpread$educ.somecoll == 1] <- "some college"
exitSpread$educ.somecoll[exitSpread$educ.somecoll == 0] <- ""
exitSpread$educ.bach[exitSpread$educ.bach == 1] <- "bachelors"
exitSpread$educ.bach[exitSpread$educ.bach == 0] <- ""
exitSpread$educ.postgrad[exitSpread$educ.postgrad == 1] <- "postgrad"
exitSpread$educ.postgrad[exitSpread$educ.postgrad == 0] <- ""

#Use the unite function to unite the education columms
exitEducClean <- unite(exitSpread, "educ", c(educ.hs, educ.somecoll, educ.bach, educ.postgrad), sep = '')
exitEducClean$educ[exitEducClean$educ == 99999999] <- NA #replace all missing educs with NA


#######
## 2C
#######

#start with our semi clean data frame: exitEducClean
head(exitEducClean)

#separate the 1 column into 3
exitClean <- separate(exitEducClean, sex.age.race, c("sex", "age", "race"), sep = " ")

#replace all unknows with NA
exitClean$sex[exitClean$sex == 'unknown'] <- NA
exitClean$age[exitClean$age == -999] <- NA
exitClean$race[exitClean$race == 'NA'] <- NA


#######
## 2D
#######

#change the name of the dataframe to work on it for part D
exitThirdPatry <- exitClean

#If there is a 1 or a 2 in PRSRPA16, then change it to a 0
exitThirdPatry$PRSPA16[exitThirdPatry$PRSPA16 == 1 | exitThirdPatry$PRSPA16 == 2] <- 0

#Any nuber that's not 0, make it a 1
exitThirdPatry$PRSPA16[exitThirdPatry$PRSPA16 != 0] <- 1

#Change the name of the PRSPA16
names(exitThirdPatry)[2] <- 'third.party'

#######
## 2E
#######

#change name of dataframe to work on it for part E
exitMarried <- exitThirdPatry

#Set 1 = to true and then set 2 = false
exitMarried$married[exitMarried$married == 1] <- TRUE
exitMarried$married[exitMarried$married == 2] <- FALSE

#######
## 2F
#######

#rename the data frame to work on it for part F
exitPartF <- exitMarried

#changing the PHIL3 column variables and name
exitPartF$PHIL3[exitPartF$PHIL3 == 1] <- 'Liberal'
exitPartF$PHIL3[exitPartF$PHIL3 == 2] <- 'Moderate'
exitPartF$PHIL3[exitPartF$PHIL3 == 3] <- 'Conservative'
names(exitPartF)[3] <- 'political.ideology'


#changing the partyid column variables

exitPartF$partyid[exitPartF$partyid == 1] <- 'Democrat'
exitPartF$partyid[exitPartF$partyid == 2] <- 'Republican'
exitPartF$partyid[exitPartF$partyid == 3] <- 'Independent'
exitPartF$partyid[exitPartF$partyid == 4] <- 'Something Else'


#######
## 2G
#######

#the final dataframe
finalExitClean <- exitPartF

View(exit)
View(finalExitClean)


#######################
## Question 3
#######################

#######
## 3A
#######

shutdownPoll <-import("NBC SurveyMonkey Government Shutdown Poll Dataset.sav")

attributes(shutdownPoll$q15)
#Variable q15: "Do you think President Trump's recently reported remarks about African 
#countries and Haiti have impacted negotiations over a federal government shutdown?"


attributes(shutdownPoll$q43)
#Varibale q43: "Do you think President Trump's recently reported derogatory remarks about 
#African countries and Haiti have impacted negotiations over a federal government shutdown?"

#The obvious difference is the word “derogatory” in q43. I think that this could have an 
#effect on the way people choose to answer. Q43 is more biased than q15 because, although, 
#Trump’s comments were derogatory, it’s is still a subjective question and reminds the person 
#taking the survey that the comments were ill-intentioned. I hypothesize that more people 
#will respond “yes” to q43 than q15, however I don’t think it will majorly affect the data.

#######
## 3B
#######

#from attributes, I know 1 is yes so I will table and the look at how many 1's there are
#then divide by the number of rows to get a percentage


(table(shutdownPoll$q15) / nrow(shutdownPoll)) * 100
(table(shutdownPoll$q43) / nrow(shutdownPoll)) * 100

#there are 884 yesses from q15, which corresponds to 25.6%
#and there are 909 yesses from q43, which corresponds to 26.3%

#There is definitely an increase in the number of yesses from q15 to q43. Honestly, the increase 
#is not as major as I thought it would be, but it clearly shows how one word can make the difference 
#in a survey question. The 25 additional yesses from q15 to q43 corresponds to a 2.83% increase in 
#the number of yesses, which is faily significant. 



#######################
## Question 4
#######################


#######
## 4A
#######

#After cleaning the data, I would come up with some hypotheses that I could test. Some 
#research questions that I could easily answer with this data are average race and education 
#of a Trump voter. I would also be interested to see how many people who voted for Trump still 
#approve of Trump. On the converse, I would also like to see how many people who didn’t vote 
#for Trump approve of him. Additionally, this data could provide answer to the race, gender, 
#and education of someone who supports the Green New Deal as well as Medicare for All. Lastly, 
#I would like to use the data to see how many “outliers” there are. For example, how many people 
#voted from Trump but support a Green New Deal, or how many young educated women voted and or 
#support Trump. 

#I would definitely like to get the average demographic of a Trump voter. I could do this by 
#creating some pie charts of race, education, and then a bar graph of ages. Next, I would look 
#specifically at the number of people who voted for Trump but don’t support him, and the people 
#that didn’t vote for him but now support him. Additionally, I would look at other pie charts 
#that give me the race, gender, and education of the population who support a Green New Deal 
#and also support Medicare for all.  


#######
## 4B
#######

#The political question I have is: would you support the impeachment of Trump?
#And the non-political question I have is: what is your households’ total annual income?
  