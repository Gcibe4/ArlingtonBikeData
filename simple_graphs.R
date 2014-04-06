library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)
library(ggplot2)
library(lattice)

#noticed some bugs in the update, just saving this simple copy to generate the 
#graphs needed for some quick insights.. may do this in Python to save trouble
#now to import the CSV as data, please make sure this file is in the same folder as the CSV
#set working directory to same folder where data is stored to keep code tidy
 setwd("/Users/Josh/Desktop/Dropbox/Processed Counter Data & R code/")
# setwd("~/Dropbox/Processed Counter Data & R code/") # Dir on John's computer

#import big CSV 
Arl_MUT_Combined <- read.csv("Arl_MUT_Combined.csv")
#import CSV with holidays and other dates of concern
holidays <- read.csv("Holidays.csv")


Arl_MUT_Combined <- merge(Arl_MUT_Combined, holidays, by = "Date")

#now to set the dates to something understood as that in R, using Chron, lubridate (ugh) etc..
Arl_MUT_Combined$Date <- as.Date(Arl_MUT_Combined$Date,format="%d/%m/%Y")

#setting the time to actual time of day, including adding seconds.. cuz. we can?
#Arl_MUT_Combined$time <- times(paste(Arl_MUT_Combined$time,":00",sep=""),format="h:m:s")
Arl_MUT_Combined$time <- paste(Arl_MUT_Combined$time,":00",sep="")

#Arl_MUT_Combined$time <- strptime(Arl_MUT_Combined$time, "%H:%M:%S", tz = "")
#using awesome weekdays function to appropriately tag dates with days of the week and create a new column for that
Arl_MUT_Combined$Day <- weekdays(Arl_MUT_Combined$Date)

#categorizing days by testing for weekend or weekday through "if else" statement using sunday and saturday matching test
#if the day is Saturday or Sunday, then it gets a True entry in new weekend column
Arl_MUT_Combined$Weekend <- ifelse((Arl_MUT_Combined$Day == "Sunday") | (Arl_MUT_Combined$Day == "Saturday"),TRUE,FALSE)

#if the day is a weekend or a federal holiday then it is not a workday
Arl_MUT_Combined$Workday <- ifelse(Arl_MUT_Combined$Weekend | Arl_MUT_Combined$Holiday, FALSE, TRUE)


#melt in case we need a truly "vertical" data frame
Arl_MUT_Melt <- melt(Arl_MUT_Combined, measure.vars = c("PedIN","PedOUT","BikeIN","BikeOUT"),id.vars=c("Date","time","Location","Day","Holiday","Likely.abnormal","OPM.Closure","Other.events","Weekend","Workday"))
Arl_MUT_Melt$Mode <- ifelse(grepl("Ped",Arl_MUT_Melt$variable),"Ped","Bike")
Arl_MUT_Melt$Dir <- ifelse(grepl("IN",Arl_MUT_Melt$variable),"In","Out")

ddply(Arl_MUT_Combined, c("Date","Location"), summarise, grand_total = sum(Total, na.rm=TRUE))
