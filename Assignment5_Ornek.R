# Assignment 5 Ertan Ornek
this_dir <- function(directory)
setwd( file.path(getwd(), directory) )
setwd("D:\\Backup\\Dropbox\\Canvas\\455\\Week9\\Assignment5_Ornek")
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html

library(usmap)
library(ggplot2)


# Our client is interested in learning about the fraudelent activities recorded by Office of Inspector General 
# The fraudelent activities mainly take place in Medicare by individuals or organizations
# The main business questions are:
# Are the fraudulent activities concentrated on certain areas (counties)?
# If the fraudulent activities were normalized by population, does the map significantly change?


all_data<-read.csv("UPDATED.csv",header=TRUE,sep=",")
all_city_county<-read.csv("uscities.csv")

all_dataDF<-data.frame(all_data)
all_city_countyDF<-data.frame(all_city_county)
all_city_countyDF$city<-toupper(all_city_countyDF$city)
str(all_dataDF)

#Now we attach county names and other fields to our fraud dataset

all_data_withCountynames<-merge(all_dataDF,all_city_countyDF,by.x = c("CITY","STATE"),by.y = c("city","state_id"))


#let's summarize the data by county
library(dplyr)

print (min(all_data_withCountynames$EXCLDATE))
print (max(all_data_withCountynames$EXCLDATE))
print (2018-1977)

faudcases<- group_by(all_data_withCountynames, county_name,STATE,county_fips,population)

percounty<-summarise(faudcases,numberofcases=n())

percounty$state<-percounty$STATE
percounty$county<-paste("County", percounty$county_name, sep=" ")
percounty$fips<-formatC(percounty$county_fips, width=5, flag="0")
percounty$abbr<-percounty$STATE

percounty$froudper100K<-(percounty$numberofcases/(41*percounty$population))*100000

percounty$froudper100K<- ifelse(percounty$froudper100K>100 , 100, percounty$froudper100K)


plot_usmap(regions = "counties",data = percounty,values = "froudper100K")+
  scale_fill_continuous(
    low = "white", high = "red", name = "No of Frauders per 100K pop.", label = scales::comma
  )+
  labs(title = "Office of Inspector General", subtitle = "Frauders Excluded from Federal and State Health Care Programs") + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"))



# Last 10 years of fraud reporting

all_data_withCountynames2<-all_data_withCountynames[which(all_data_withCountynames$EXCLDATE>20071231 & all_data_withCountynames$EXCLDATE<20181231),]

faudcases<- group_by(all_data_withCountynames2,STATE)

perstate<-summarise(faudcases,numberofcases=n())

perstate$state<-perstate$STATE

perstate<-merge(perstate, statepop,by.x='state',by.y='abbr')
#perstate$county<-paste("County", perstate$county_name, sep=" ")

perstate$abbr<-perstate$STATE

perstate$froudper100K<-((perstate$numberofcases/10.0)/(perstate$pop_2015))*100000

perstate$froudper100K<- ifelse(perstate$froudper100K>100 , 100, perstate$froudper100K)
plot_usmap(include = c("CA", "NV", "ID", "OR", "WA"),labels=TRUE) +
  labs(title = "Western States")

plot_usmap(regions = "states",data = perstate,values = "froudper100K",labels=TRUE)+
  scale_fill_continuous(
    low = "white", high = "red", name = "No of Cases per 100K pop.", label = scales::comma
  )+
  #labs(title = "Office of Inspector General", subtitle = "Fraud Excluded from Federal and State Health Care Programs") + 
  theme(panel.background = element_rect(colour = "transparent", fill = "lightblue"),legend.position = "right")


ggsave("Last10YearsPerState.svg", width=12, height=10)





### Vermont, Mississippi, Alaska, West Virginia
faudcases<- group_by(all_data_withCountynames, county_name,STATE,county_fips,population)

percounty<-summarise(faudcases,numberofcases=n())

percounty$state<-percounty$STATE
percounty$county<-paste("County", percounty$county_name, sep=" ")
percounty$fips<-formatC(percounty$county_fips, width=5, flag="0")
percounty$abbr<-percounty$STATE

percounty$froudper100K<-(percounty$numberofcases)

percounty$froudper100K<- ifelse(percounty$froudper100K>200 , 200, percounty$froudper100K)


plot_usmap(regions = "counties",data = percounty,values = "numberofcases",include = c("VT","MS","AK","WV"))+
  scale_fill_continuous(
    low = "white", high = "red", name = "No of Cases", label = scales::comma
  )+
  #labs(title = "Office of Inspector General", subtitle = "Frauders Excluded from Federal and State Health Care Programs") + 
  theme(panel.background = element_rect(colour = "transparent", fill = "lightblue"),legend.position = "right")


ggsave("States.svg", width=39, height=20)
percounty$STATE=="VT"


vermontdata<-percounty[percounty$STATE=="VT",]
mississippidata<-percounty[percounty$STATE=="MS",]
alaskadata<-percounty[percounty$STATE=="AK",]
westvirginiadata<-percounty[percounty$STATE=="WV",]
