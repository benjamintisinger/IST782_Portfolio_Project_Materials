
#Import data set using "From Text (readr)"

#Rename dataset
Loldata <- IST687_project_df

#Create a subset containing only the candidate variables we will need for analysis
Loldata= subset(Loldata, select= c(3, 7, 8, 9, 12, 16, 19, 29, 34, 38, 55, 52, 68, 69))

#Clean the subset by getting rid of the last 90,000 rows
Loldata = Loldata[c(2:10001),]

#Check if there are any NA values
sum(is.na(Loldata))

#Rename wordy columns
library(data.table)
setnames(Loldata, "magicDamageDealtToChampions", "magicToChamp")

setnames(Loldata, "physicalDamageDealtToChampions", "physicalToChamp")

#Check the structure to make sure the data is in the appropriate datatype
str(Loldata)

#Business Questions:
#(1) What is the win rate per champ:
#Create a subset containing only the "championName" and "win" columns
library(tidyverse)
library(dplyr)
#Finding how many unique Game IDs there are to get number of total games
uniqueGameIDs<-data.frame(unique(Loldata$GameID))
#Counting how many unique games there are
count(uniqueGameIDs)
#Subsetting Loldata to create new variable with champion wins
champstats <- subset(Loldata, select=c(14, 4))
#Grouping each champion by number of wins
champstats <- champstats %>% group_by(championName) %>% summarise (win = sum(win))

# #Getting a win percentage. Because there are 1000 games consisting of 10,000 players
# only 5000 players can be winners.
champstats$champWinPercent <- champstats$win/1000*100
#Finding highest win rate
max(champstats$win)
#Identifying which row the highest win rate champ is in
which(champstats$win==max(champstats$win), arr.ind=TRUE)
#Printing row 71
champstats[71,]
#Finding lowest win rate
min(champstats$win)
#Identifying which row the lowest win rate champ is in
which(champstats$win==min(champstats$win), arr.ind=TRUE)
#Printing row 11
champstats[11,]
#The champion with the highest winrate is Lux with a 10.8% win rate, 
champstats_top10= champstats %>% 
  arrange(desc(champWinPercent)) %>% 
  slice(1:10)

champstats_low10= champstats_sorted= champstats %>% 
  arrange(champWinPercent) %>% 
  slice(1:10)
  
glimpse(champstats)

ggplot(champstats_top10, aes(x=championName, y=champWinPercent))+
geom_bar(stat="identity" )+
  coord_flip()+
  geom_text(aes(label= champWinPercent))+
  scale_y_continuous(breaks = seq(1,10, len = 10))+
  ggtitle("Top Ten Champion Wins Percent")

ggplot(champstats_low10, aes(x=championName, y=champWinPercent))+
  geom_bar(stat="identity" )+
  coord_flip()+
  geom_text(aes(label= champWinPercent))+
  scale_y_continuous(breaks = seq(1,10, len = 10))+
  ggtitle("Bottom Ten Champion Wins Percent")

#(2) What is the correlation between total damage and win rate?
library(ggpubr)
#Adding magic and physical dam to get total dam
Loldata$totalToChamp <- Loldata$magicToChamp + Loldata$physicalToChamp
#Grouping total damage by champion
champstats$champTotalDam <- Loldata %>% group_by(championName) %>% summarise (totalToChamp = sum(totalToChamp)) %>% select (totalToChamp)
#Putting vector into champstats dataframe for downstream processing
champstats$champTotalDam <- pull(champstats$champTotalDam)
#Confirming new datatype
str(champstats)
#Running Person's correlation test
cor.test(champstats$champTotalDam, champstats$win, method = c("pearson"))

#lm regression
lm(champTotalDam ~ win, data=champstats)
summary(lm(champTotalDam ~ win, data=champstats))
#y= 15719+ 31560win
#With every increase of win, the the total damage on average increases around 31560. 
#Because the pvalue is less than 0.05, we reject the null hyptheis, there is a relationship between the two variables. 

library(ggplot2)
ggplot(champstats, aes(x=win, y=champTotalDam))+
  geom_point()+
  geom_smooth()
  

#(3) What is the correlation between damage received and probability of game ending in surrender?
#Grouping game ended in surrender by champion
champstats$champEndedInSur <- Loldata %>% group_by(championName) %>% summarise (gameEndedInSurrender = sum(gameEndedInSurrender)) %>% select (gameEndedInSurrender)
#Grouping total damage taken by champion
champstats$champTotalDamTaken <- Loldata %>% group_by(championName) %>% summarise (totalDamageTaken = sum(totalDamageTaken)) %>% select (totalDamageTaken)
#Putting vectors into champstats dataframe for downstream processing
champstats$champTotalDamTaken <- pull(champstats$champTotalDamTaken)
champstats$champEndedInSur <- pull(champstats$champEndedInSur)
#Confirming new datatype
str(champstats)
#Running Person's correlation test
cor.test(champstats$champTotalDamTaken, champstats$champEndedInSur, method = c("pearson"))

#lm regression
lm(champEndedInSur~ champTotalDamTaken, champstats)
summary(lm(champEndedInSur~ champTotalDamTaken, champstats))
#y= 3.221e+00+1.083e-05damagetaken
#With every damage taken to the champion, the champion ending in surrended increases by 1.083e-05.
#Because the pvalue is less than 0.05, we reject the null hyptheis, there is a relationship between the two variables. 

ggplot(champstats, aes(x=champTotalDamTaken, y=champEndedInSur))+
  geom_point()+
  geom_smooth()+
  ggtitle("Correlation Between Damage Taken to Champion and Champion Ending in Surrender")


#(4) What utility champions have the highest win rate at level 18?
#Subsetting Loldata to create a new data frame with just utility champs and select columns of interest
utilityChamps <- subset(Loldata, select=c(14, 3, 4, 7))
utilityChamps <- subset(utilityChamps,utilityChamps$individualPosition=="UTILITY")
#Grouping each utility champion by number of wins
utilStats <- utilityChamps %>% group_by(championName) %>% summarise (win = sum(win))
#If there are no wins, set to NA
utilStats[utilStats==0] <- NA
#Drop NAs
utilStats<-drop_na(utilStats)
# #Getting a win percentage for util champs.
utilStats$utilWinPercent <- utilStats$win/1000*100
#Finding highest win rate among util champs.
max(utilStats$win)
#Identifying which row the highest win rate util champ is in
which(utilStats$win==max(utilStats$win), arr.ind=TRUE)
#Printing row 24
utilStats[24,]
'Lux has the highest win rate for utility champs at 7.9%'
sum(utilStats$utilWinPercent)
sum(Loldata$individualPosition=="UTILITY")
sum(Loldata$individualPosition=="JUNGLE")
sum(Loldata$individualPosition=="TOP")
sum(Loldata$individualPosition=="BOTTOM")
sum(Loldata$individualPosition=="MIDDLE")
Loldata %>% count(individualPosition)
(unique(Loldata$individualPosition))
sum(Loldata$individualPosition=="Invalid")


util_top10= utilStats %>% 
  arrange(desc(utilWinPercent)) %>% 
  slice(1:10)

util_low10= utilStats %>% 
  arrange(utilWinPercent) %>% 
  slice(1:10)

glimpse(champstats)

ggplot(util_top10, aes(x=championName, y=utilWinPercent))+
  geom_bar(stat="identity" )+
  coord_flip()+
  geom_text(aes(label= utilWinPercent))+
  scale_y_continuous(breaks = seq(1,10, len = 10))+
  ggtitle("Top Ten Utility Champion Wins Percent")

ggplot(util_low10, aes(x=championName, y=utilWinPercent))+
  geom_bar(stat="identity" )+
  coord_flip()+
  geom_text(aes(label= utilWinPercent))+
  scale_y_continuous(breaks = seq(1,10, len = 10))+
  ggtitle("Bottom Ten Utility Champion Wins Percent")


#17 records have invalidPosition="Invalid", causing the utilitychamps count to be 1983 instead of 2000.
view(Loldata %>% filter(individualPosition=="Invalid"))



#(5)Utility champions per level with wins and losses?
utilityChamps$champLevel=="18"
length(which(utilityChamps$champLevel==18))
utilityChamps %>% 
  filter(champLevel==18) %>% 
  select(championName)%>% 
  distinct() %>% 
  print(n=29)

util_level18<- ifelse (utilityChamps$champLevel==18, "TRUE", "FALSE")

view(utilityChamps)

utilityChamps[order(-utilityChamps$champLevel)[1:10],]


utiltop10<- utilStats[order(-utilStats$utilWinPercent)[1:10],1]
utilWins_With_Levels<-merge(utiltop10, utilityChamps, by="championName")
utilLose_With_Levels<-utilWins_With_Levels %>%filter (win=="FALSE")
utilWins_With_Levels<-utilWins_With_Levels %>%filter (win=="TRUE")  

utilwins_count=utilWins_With_Levels %>%
  group_by(championName, champLevel) %>% 
  count(champLevel)

ggplot(utilwins_count, aes(x=champLevel, y=n, colour=championName))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(1,18, len = 18))+
  scale_y_continuous(breaks = seq(1,14, len = 14))+
  ggtitle("Utility Champion Levels Per Win")


utilloss_count=utilLose_With_Levels %>%
  group_by(championName, champLevel) %>% 
  count(champLevel)

ggplot(utilloss_count, aes(x=champLevel, y=n, colour=championName))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(1,18, len = 18))+
  scale_y_continuous(breaks = seq(1,14, len = 14))+
  ggtitle("Utility Champion Levels Per Loss")
