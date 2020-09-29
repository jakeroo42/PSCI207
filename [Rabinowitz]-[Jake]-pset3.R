##############################
##PSCI 207: Applied Data Science
##Problem Set 3
##[Jake Rabibowitz]
##############################

## If you worked with anybody else on this problem set, please list their names here: Max Weissberg
library(dplyr)
library(tidyr)
library(rio)
require(lubridate)
require(stringr)
require(purrr)
require(compare)



setwd("Dropbox/Data Science/problem set 3/")


#######################
## Question 1
#######################

#import the data
unicef <- import("unicef data.xlsx")
unicef.raw <- unicef


#######
## 1A
#######

#the three problems I see with the data are
#1: The column names are improperly named 
#2: There are many types of blank cells that are not just N/A
#3: The data is organized in kind of a long and wide format, very confusing 


#######
## 1B
#######

#Renaming the columns of the data frame with first row 
names(unicef) <- unicef[1,]

#Deleting first two rows
unicef <- unicef[-(1:2),]


#######
## 1C
#######


#I think this shows the percentage of N/A in each row. So if the whole row is N/A then the number is 1
rowMeans(is.na(unicef))

#Delete any rows with all missing values
unicef <- unicef[rowMeans(is.na(unicef)) != 1,]

#Delete any columns with all missing values
unicef <- unicef[,!(is.na(names(unicef)))]

#getting dim of data frame
dim(unicef)

#The data frame as 216 rows and 12 columns 

#######
## 1D
#######

#rename/ clean the column names

attr(unicef, "names") <- trimws(attr(unicef,"names"))
attr(unicef, "names") <- tolower(attr(unicef,"names"))

unicef <- rename(unicef,
                 total.pop = "total population (thousands)", 
                 annual.no.births = "annual no. of births (thousands)")
#not sure what to do to clean them more, they look pretty good 

#######
## 1E
#######

#eliminate the rows with notes
unicef <- unicef[-(which(unicef =="Notes:"):nrow(unicef)),]
      
#######
## 1F
#######

#change the dashes to NAs
unicef[unicef == "-"] <- NA

#Make every column numeric 
unicef[,-1] <- sapply(unicef[,-1], as.numeric)

#######
## 1G
#######

#multiply those columns by 1000
unicef[,c('total.pop', 'annual.no.births')] <- unicef[,c('total.pop', 'annual.no.births')] * 1000

#######
## 1H
#######

#This preserves the unicef data frame
unicef.h <- unicef

#Getting the unicef data frame 
unicef.country <- unicef.h[-(which(unicef.h == "SUMMARY INDICATORS"):nrow(unicef.h)),]

#Getting and cleaning the summary indicators data frame
unicef.summary <- unicef.h[which(unicef.h == "SUMMARY INDICATORS"):nrow(unicef.h),]
unicef.summary <- unicef.summary[-1,]


#Rows for two data frames
nrow(unicef.country)
#Country is: 197


nrow(unicef.summary)
#Summary is: 10


#######
## 1I
#######

unicef.country$infant.mort.rate <-unicef.country$`under-5 mortality rate (u5mr) 2015` - unicef.country$`under-5 mortality rate (u5mr) (1990)`

unicef.country.increase.mort.rate <- unicef.country[unicef.country$infant.mort.rate > 0,]

#Three countries have an increase in mortality rate: Niue with a 9, Dominica with 4, and Lesotho with 2

#######
## 1J
#######

#Plotting the literacy vs life expectancy 

x <- unicef.country$`life expectancy at birth (years)`
y <- unicef.country$`total adult literacy rate          (%)`

plot(x, y, main = "Percent Adult Literacy vs Life Expectancy by Country", xlab = "Life Expectancy (Years)", 
     ylab = "Percent Adult Literacy", col = "blue")


linModel <- lm(x~y)
abline(linModel, col = 'red')



#######################
## Question 2
#######################


#######
## 2A
#######

vector <- dir(pattern ="athletes-")

athleteMap <- map(vector, read.csv, stringsAsFactors = F)

completeOlympicData <- bind_rows(athleteMap)

dim(completeOlympicData)
#There are 187303 rows and 12 columns in the data 

length(unique(completeOlympicData$athlete))
#There are 132968 unique athletes in the data frame

#######
## 2Bi
#######

olympic <- completeOlympicData

#getting the seperate medal counts
gold <- aggregate(gold~country, olympic, sum)
silver <- aggregate(silver~country, olympic, sum)
bronze <- aggregate(bronze~country, olympic, sum)
total <- aggregate(total~country, olympic, sum)

#merging all the medal counts together 
countryMedal = Reduce(function(x, y) merge(x, y, all=TRUE), 
                    list(gold, silver, bronze, total))

#Creating a column thats the absolute difference between gold and silver
countryMedal$gold.silver.dif <- abs(countryMedal$gold - countryMedal$silver)

#getting the max difference between gold and silver medals
countryMedal[which(countryMedal$gold.silver.dif == max(countryMedal$gold.silver.dif)),]

#The max difference is from the USA and its 997 medals (2638 gold and 1641 silver)

#######
## 2Bii
#######

#Creating a data frame with contries that have won more than 30 total medals 
countryMedal.over30 <- countryMedal[countryMedal$total >= 30,]

#getting the percent of medals that are gold for countries with over 30 total medals
countryMedal.over30$perGold <- countryMedal.over30$gold / countryMedal.over30$total

#finding the max percentage
countryMedal.over30[which(countryMedal.over30$perGold == max(countryMedal.over30$perGold)),]

#India, which has 70.1% of its medals has gold 

#######
## 2Ci
#######

#making the data frame just year and season
olympic.SeasonYear <- completeOlympicData[10:11]

#getting the unique season and year olympic events
olympic.SeasonYear <- olympic.SeasonYear[!duplicated(olympic.SeasonYear),]

nrow(olympic.SeasonYear)
#There have been 51 olympic events 

#######
## 2Cii
#######

#making a data frame just the event and country
olympic.event.country <- completeOlympicData[10:12]

#Tabling all unique countries by the games 
gamesByCountry <- data.frame(table(unique(olympic.event.country)$country))

#Renaming data frame
names(gamesByCountry) <- c("Country", "NumGames")

#Finding the countries that participated in all the Olympic games 
participateInAll <- gamesByCountry[gamesByCountry$NumGames == 51,]

#France, Great Britain, and Italy are the only three countries to participate in all the olympic games 

#######
## 2Ciii
#######
#Get a table with only Greece, the events, and the dates
olympic.event.country.greece <- olympic.event.country[olympic.event.country$country == "Greece",]

#Get rid of any duplicated rows to just get the games that Greece participated in
olympic.SeasonYear.Greece <- olympic.event.country.greece[!duplicated(olympic.event.country.greece),]

#See which events Greece did not participate in
anti_join(olympic.SeasonYear,olympic.SeasonYear.Greece)

#Greece did not participate in the winter 1924, 1928, 1932, and 1960 Olympic games

#######
## 2Di
#######

#Make a table using only individual athletes
olympic.athlete <- aggregate(total ~ athlete, completeOlympicData, sum)


#See which athlete won the most olympic medals
olympic.athlete[which(olympic.athlete$total == max(olympic.athlete$total)),]
#Michael Phelps with 28 total medals

#######
## 2Dii
#######



#Make a table using only individual athletes
olympic.athlete <- aggregate(total ~ athlete, completeOlympicData, sum)

#Rank the athletes that have won the most in decreasing order of total medal count
olympic.athlete[order(-olympic.athlete$total),]

#Michael Phelps with 28 total medals, 18 for Larysa Latynina, and 15 for Nikolay Andrianov. There are 4 
#athletes with 13 total medals

#######
## 2Diii
#######

#aggregate the data frame by athlete and season 
athlete.agg.season <- aggregate(gold~athlete+season,completeOlympicData,sum)[1]

#elminate any duplicates
athlete.both.seasons <- data.frame(athlete.agg.season[duplicated(athlete.agg.season),])

#count the rows
nrow(athlete.both.seasons)

#There are 550 athletes that have participated in both summer and winter olympics 


#######
## 2Div
#######

#Aggregate the data so only 2 season athletes are in the data frame
medalBoth <- aggregate(total ~ athlete+season, completeOlympicData, sum)

#Only have medal winners
medalBothTrue <- medalBoth[medalBoth$total > 0,]

#Eliminate duplicates 
medalBothSeasonsTrue <- data.frame(medalBothTrue[1][duplicated(medalBothTrue[1]),])

#Count the athletes
nrow(medalBothSeasonsTrue)

#There have been 40 athletes that have won a medal in both the Winter and SUmmer Olympics 

names(medalBothSeasonsTrue)[1] <- "athlete"

#Merging the athlete names to the whole data set to get years
bothSeasonMerge <- merge(medalBothSeasonsTrue, completeOlympicData, by = 'athlete', all = F)

#Ordering the data from most recent to oldest
bothSeasonsYearRank <- bothSeasonMerge[order(-bothSeasonMerge$year),]

#The most recent two season athlete to win a medal was in 2016 and is Kim So-Hui in Taekwondo of South Korea 

#######################
## Question 3
#######################


#######
## 3A
#######

load("gen-forward.RData")

#This will load 2 data frames, oct, dec

#I think the best merge you can do is a full merge on the GenF_ID

#######
## 3B
#######

#Finding number of respondants who took survey in oct
nrow(oct)
#1876 people took the survey in october

#Finding number of respondants who took the suvey in dec 
nrow(dec)
#1844 people took the survey in december 

#People who took the survey in oct but not dec
nrow(anti_join(oct, dec, by ="GenF_ID"))
#489

#People who took the survey in dec but not oct
nrow(anti_join(dec, oct, by ="GenF_ID"))
#457
merge(oct, dec, by = "GenF_ID", all = T)


#I think there would be 2333 total people that took the survey at least once,
#489 + 457 + 1387 = 2333

#People who took both
bothMonths.2time <- merge(oct, dec, by = "GenF_ID", all = F)
nrow(bothMonths.2time)
#1387 people took the survey both time 

#Everyone across the two months that took at least one survey
bothMonths.1orMore <- merge(oct, dec, by = "GenF_ID", all = T)
nrow(bothMonths.1orMore)
#2333 

#######
## 3C
#######

fullData <- merge(oct, dec, by = "GenF_ID", all = T, suffixes = c(".oct", ".dec"))

dim(fullData)

#The new data frame has 37 columns and 2333 rows 

#######
## 3D
#######

#Not sure how to do this


#######
## 3E
#######

#Not sure how to do this





