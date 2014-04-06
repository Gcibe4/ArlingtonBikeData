library(chron)
library(lubridate)
library(plyr)
library(zoo)
library(reshape2)

# I Ended up breaking this off into a separate file because several filenames were different. 
# Note the addition, after loading the files, of a section to delete the first day each counter is online,
# due to calibration etc., as recommended by David Patton -JCS

#set working directory to same folder where data is stored to keep code tidy
#setwd("~/Desktop/datascience/Arlington_MTU/Data2")
setwd("~/Dropbox/VT coursework/Capstone/Counter data") # Data dir on John's computer

#import base files prior to tidying data, names kept simple based on location, direction where applicable, and year
EFC10 <- read.csv("2010 WOD EFC 12.07-12.31 15 mins.csv")
EFC11 <- read.csv("2011 WOD EFC 01.01-12.31 15 mins.csv")
EFC12 <- read.csv("2012 WOD EFC 01.01-12.31 15 mins.csv")

TR_Island11 <- read.csv("2011 TR Island 10.14-12.31 15 mins.csv")
TR_Island12 <- read.csv("2012 TR Island 01.01-12.31 15 mins.csv")
TR_Island13 <- read.csv("2013 TR Island 01.01-12.31 15 mins.csv")

BonAir_WOD_East11 <- read.csv("2011 WOD BA East 01.21-12.31 15 mins.csv")
BonAir_WOD_East12 <- read.csv("2012 WOD BA East 01.01-12.31 15 mins.csv")
BonAir_WOD_East13 <- read.csv("2013 WOD BA East 01.01-08.31 15 min.csv")

BonAir_WOD_West12 <- read.csv("2012 WOD BA West 12.10-12.31 15 mins.csv")
BonAir_WOD_West13 <- read.csv("2013 WOD BA West 01.01-08.31 15 mins.csv")

ColPike11 <- read.csv("2011 WOD Col Pike 10.14-12.31 15 mins.csv")
ColPike12 <- read.csv("2012 WOD Col Pike 01.01-12.31 15 mins.csv")

CCityCon11 <- read.csv("2011 Crystal City Con 09.02-12.31 15 mins.csv")
CCityCon12 <- read.csv("2012 Crystal City Con 01.01-12.31 15 mins.csv")
CCityCon13 <- read.csv("2013 Crystal City Con 01.01-12.31 15 mins.csv")

BonAir_Custis10 <- read.csv("2010 Custis BA 12.08-12.31 15 mins.csv")
BonAir_Custis11 <- read.csv("2011 Custis BA 01.01-12.31 15 mins.csv")
BonAir_Custis12 <- read.csv("2012 Custis BA 01.01-12.31 15 mins.csv")
BonAir_Custis13 <- read.csv("2013 Custis BA 01.01-12.31 15 mins.csv")

Rosslyn09 <- read.csv("2009 Custis Rosslyn 10.15-12.31 15 mins.csv")
Rosslyn10 <- read.csv("2010 Custis Rosslyn 01.01-12.31 15 mins.csv")
Rosslyn11 <- read.csv("2011 Custis Rosslyn 01.01-12.31 15 mins.csv")
Rosslyn12 <- read.csv("2012 Custis Rosslyn 01.01-12.31 15 mins.csv")
Rosslyn13 <- read.csv("2013 Custis Rosslyn 01.01-12.31 15 mins.csv")

FourMile11 <- read.csv("2011 FMR Pyro 03.04-12.31 15 mins.csv")
FourMile12 <- read.csv("2012 FMR Pyro 01.01-12.31 15 mins.csv")
FourMile13 <- read.csv("2013 FMR Pyro 01.01-12.31 15 mins.csv")

KeyBridgeEast11 <- read.csv("2011 Key Bridge E 08.12-12.31 15 mins.csv")
KeyBridgeEast12 <- read.csv("2012 Key Bridge E 01.01-12.31 15 mins.csv")
KeyBridgeEast13 <- read.csv("2013 Key Bridge E 01.01-12.31 15 mins.csv")

KeyBridgeWest11 <- read.csv("2011 Key Bridge W 08.11-12.31 15 mins.csv")
KeyBridgeWest12 <- read.csv("2012 Key Bridge W 01.01-12.31 15 mins.csv")
KeyBridgeWest13 <- read.csv("2013 Key Bridge W 01.01-12.31 15 mins.csv")

Airport11 <- read.csv("2011 MVT Airport S 09.01-12.31 15 mins.csv")
Airport12 <- read.csv("2012 MVT Airport S 01.01-12.31 15 mins.csv")
Airport13 <- read.csv("2013 MVT Airport S 01.01-12.31 15 mins.csv")

Ballston12 <- read.csv("2012 Ballston Con 12.06-12.31 15 mins.csv")
Ballston13 <- read.csv("2013 Ballston Con 01.01-12.31 15 mins.csv")

Bluemont13 <- read.csv("2013 Bluemont Conn 11.22-12.31 15 mins.csv")

JoyceNB13 <- read.csv("2013 NB Joyce 06.20-12.31 15 mins.csv")

JoyceSB13 <- read.csv("2013 SB Joyce 06.20-12.31 15 mins.csv")

# remove first day of each counter's data to remove unusual activity due to setup/calibration 
EFC10 <- subset(EFC10, Date != "07/12/2010")
TR_Island11 <- subset(TR_Island11, Date != "14/10/2011")
BonAir_WOD_East11 <- subset(BonAir_WOD_East11, Date != "21/01/2011")
ColPike11 <- subset(ColPike11, Date != "14/10/2011")
CCityCon11 <- subset(CCityCon11, Date != "02/09/2011")
BonAir_Custis10 <- subset(BonAir_Custis10, Date != "08/12/2010")
Rosslyn09 <- subset(Rosslyn09, Date != "15/10/2009")
FourMile11 <- subset(FourMile11, Date != "04/03/2011")
KeyBridgeEast11 <- subset(KeyBridgeEast11, Date != "12/08/2011")
KeyBridgeWest11 <- subset(KeyBridgeWest11, Date != "11/08/2011")
Airport11 <- subset(Airport11, Date != "01/09/2011")
Ballston12 <- subset(Ballston12, Date != "06/12/2012")
Bluemont13 <- subset(Bluemont13, Date != "22/11/2013")
JoyceNB13 <- subset(JoyceNB13, Date != "20/06/2013")
JoyceSB13 <- subset(JoyceSB13, Date != "20/06/2013")

# renaming and cleaning data files with 7 variables
# splitting where needed based on direction
Airport11<- rename(Airport11, c("X"="time", "MVT.Airport.South"="Total","MVT.Airport.South.Ped.IN"="PedIN","MVT.Airport.South.Ped.OUT"="PedOUT","MVT.Airport.South.Bike.IN"="BikeIN","MVT.Airport.South.Bike.OUT"="BikeOUT"))
Airport12<- rename(Airport12, c("X"="time", "MVT.Airport.South"="Total","MVT.Airport.South.Ped.IN"="PedIN","MVT.Airport.South.Ped.OUT"="PedOUT","MVT.Airport.South.Bike.IN"="BikeIN","MVT.Airport.South.Bike.OUT"="BikeOUT"))
Airport13<- rename(Airport13, c("X"="time", "MVT.Airport.South"="Total","MVT.Airport.South.Ped.IN"="PedIN","MVT.Airport.South.Ped.OUT"="PedOUT","MVT.Airport.South.Bike.IN"="BikeIN","MVT.Airport.South.Bike.OUT"="BikeOUT"))
Ballston12 <- rename(Ballston12, c("X"="time", "Ballston.Connector"="Total","Ballston.Connector.Ped.IN"="PedIN", "Ballston.Connector.Ped.OUT"="PedOUT","Ballston.Connector.Bike.IN"="BikeIN", "Ballston.Connector.Bike.OUT"="BikeOUT"))
Ballston13 <- rename(Ballston13, c("X"="time", "Ballston.Connector"="Total","Ballston.Connector.Ped.IN"="PedIN", "Ballston.Connector.Ped.OUT"="PedOUT","Ballston.Connector.Bike.IN"="BikeIN", "Ballston.Connector.Bike.OUT"="BikeOUT"))
Bluemont13 <- rename(Bluemont13, c("X"="time", "Bluemont.Connector"="Total","Bluemont.Connector.Ped.IN"="PedIN", "Bluemont.Connector.Ped.OUT"="PedOUT","Bluemont.Connector.Bike.IN"="BikeIN", "Bluemont.Connector.Bike.OUT"="BikeOUT"))
BonAir_Custis10 <- rename(BonAir_Custis10, c("X"="time", "Custis.Bon.Air.II"="Total","Custis.Bon.Air.Ped.IN"="PedIN", "Custis.Bon.Air.Ped.OUT"="PedOUT","Custis.Bon.Air.Bike.IN"="BikeIN","Custis.Bon.Air.Bike.OUT"="BikeOUT"))
BonAir_Custis11 <- rename(BonAir_Custis11, c("X"="time", "Custis.Bon.Air.II"="Total","Custis.Bon.Air.Ped.IN"="PedIN", "Custis.Bon.Air.Ped.OUT"="PedOUT","Custis.Bon.Air.Bike.IN"="BikeIN","Custis.Bon.Air.Bike.OUT"="BikeOUT"))
BonAir_Custis12 <- rename(BonAir_Custis12, c("X"="time", "Custis.Bon.Air.II"="Total","Custis.Bon.Air.Ped.IN"="PedIN", "Custis.Bon.Air.Ped.OUT"="PedOUT","Custis.Bon.Air.Bike.IN"="BikeIN","Custis.Bon.Air.Bike.OUT"="BikeOUT"))
BonAir_Custis13 <- rename(BonAir_Custis13, c("X"="time", "Custis.Bon.Air"="Total","Custis.Bon.Air.Ped.IN"="PedIN", "Custis.Bon.Air.Ped.OUT"="PedOUT","Custis.Bon.Air.Bike.IN"="BikeIN","Custis.Bon.Air.Bike.OUT"="BikeOUT"))
BonAir_WOD_East11<-rename(BonAir_WOD_East11, c("X"="time", "WOD.Bon.Air.East"="Total","WOD.Bon.Air.East.Ped.IN"="PedIN","WOD.Bon.Air.East.Ped.OUT"="PedOUT","WOD.Bon.Air.East.Bike.IN"="BikeIN","WOD.Bon.Air.East.Bike.OUT"="BikeOUT"))
BonAir_WOD_East12 <- rename(BonAir_WOD_East12, c("X"="time", "WOD.Bon.Air.East"="Total","WOD.Bon.Air.East.Ped.IN"="PedIN","WOD.Bon.Air.East.Ped.OUT"="PedOUT","WOD.Bon.Air.East.Bike.IN"="BikeIN","WOD.Bon.Air.East.Bike.OUT"="BikeOUT"))
BonAir_WOD_East13 <- rename(BonAir_WOD_East13, c("X"="time", "WOD.Bon.Air.East"="Total","WOD.Bon.Air.East.Ped.IN"="PedIN","WOD.Bon.Air.East.Ped.OUT"="PedOUT","WOD.Bon.Air.East.Bike.IN"="BikeIN","WOD.Bon.Air.East.Bike.OUT"="BikeOUT"))
BonAir_WOD_West12 <- rename(BonAir_WOD_West12, c("X"="time", "WOD.Bon.Air.West"="Total","WOD.Bon.Air.West.Ped.IN"="PedIN","WOD.Bon.Air.West.Ped.OUT"="PedOUT","WOD.Bon.Air.West.Bike.IN"="BikeIN","WOD.Bon.Air.West.Bike.OUT"="BikeOUT"))
BonAir_WOD_West13 <- rename(BonAir_WOD_West13, c("X"="time", "WOD.Bon.Air.West"="Total","WOD.Bon.Air.West.Ped.IN"="PedIN","WOD.Bon.Air.West.Ped.OUT"="PedOUT","WOD.Bon.Air.West.Bike.IN"="BikeIN","WOD.Bon.Air.West.Bike.OUT"="BikeOUT"))
CCityCon11<- rename(CCityCon11, c("X"="time", "CC.Connector"="Total", "CC.Connector.Ped.IN"="PedIN","CC.Connector.Ped.OUT"="PedOUT", "CC.Connector.Bike.IN"="BikeIN", "CC.Connector.Bike.OUT"="BikeOUT"))
CCityCon12<- rename(CCityCon12, c("X"="time", "CC.Connector"="Total", "CC.Connector.Ped.IN"="PedIN","CC.Connector.Ped.OUT"="PedOUT", "CC.Connector.Bike.IN"="BikeIN", "CC.Connector.Bike.OUT"="BikeOUT"))
CCityCon13<- rename(CCityCon13, c("X"="time", "CC.Connector"="Total", "CC.Connector.Ped.IN"="PedIN","CC.Connector.Ped.OUT"="PedOUT", "CC.Connector.Bike.IN"="BikeIN", "CC.Connector.Bike.OUT"="BikeOUT"))
ColPike11<- rename(ColPike11, c("X"="time", "WOD.Columbia.Pike"="Total", "WOD.Colombia.Pike.Ped.IN"="PedIN", "WOD.Colombia.Pike.Ped.OUT"="PedOUT", "WOD.Colombia.Pike.Bike.IN"="BikeIN","WOD.Colombia.Pike.Bike.OUT"="BikeOUT"))
ColPike12<- rename(ColPike12, c("X"="time", "WOD.Columbia.Pike"="Total", "WOD.Colombia.Pike.Ped.IN"="PedIN", "WOD.Colombia.Pike.Ped.OUT"="PedOUT", "WOD.Colombia.Pike.Bike.IN"="BikeIN","WOD.Colombia.Pike.Bike.OUT"="BikeOUT"))
EFC10<- rename(EFC10, c("X"="time", "WOD.East.Falls.Church"="Total", "WOD.EFC.Ped.IN"="PedIN", "WOD.EFC.Ped.OUT"="PedOUT","WOD.EFC.Bike.IN"="BikeIN", "WOD.EFC.Bike.OUT"="BikeOUT"))
EFC11<- rename(EFC11, c("X"="time", "WOD.East.Falls.Church"="Total", "WOD.EFC.Ped.IN"="PedIN", "WOD.EFC.Ped.OUT"="PedOUT","WOD.EFC.Bike.IN"="BikeIN", "WOD.EFC.Bike.OUT"="BikeOUT"))
EFC12<- rename(EFC12, c("X"="time", "WOD.East.Falls.Church"="Total", "WOD.EFC.Ped.IN"="PedIN", "WOD.EFC.Ped.OUT"="PedOUT","WOD.EFC.Bike.IN"="BikeIN", "WOD.EFC.Bike.OUT"="BikeOUT"))
JoyceNB13<- rename(JoyceNB13, c("X"="time", "NB.Joyce.St"="Total", "NB.Joyce.St.Ped.IN"="PedIN","NB.Joyce.St.Ped.OUT"="PedOUT","NB.Joyce.St.Bike.IN"="BikeIN","NB.Joyce.St.Bike.OUT"="BikeOUT"))
JoyceSB13<- rename(JoyceSB13, c("X"="time", "SB.Joyce.St"="Total", "SB.Joyce.St.Ped.IN"="PedIN","SB.Joyce.St.Ped.OUT"="PedOUT","SB.Joyce.St.Bike.IN"="BikeIN","SB.Joyce.St.Bike.OUT"="BikeOUT"))
KeyBridgeEast11<- rename(KeyBridgeEast11, c("X"="time", "Key.Bridge.East"="Total", "Key.Bridge.East.Ped.IN"="PedIN","Key.Bridge.East.Ped.OUT"="PedOUT","Key.Bridge.East.Bike.IN"="BikeIN","Key.Bridge.East.Bike.OUT"="BikeOUT"))
KeyBridgeEast12<- rename(KeyBridgeEast12, c("X"="time", "Key.Bridge.East"="Total", "Key.Bridge.East.Ped.IN"="PedIN","Key.Bridge.East.Ped.OUT"="PedOUT","Key.Bridge.East.Bike.IN"="BikeIN","Key.Bridge.East.Bike.OUT"="BikeOUT"))
KeyBridgeEast13<- rename(KeyBridgeEast13, c("X"="time", "Key.Bridge.East"="Total", "Key.Bridge.East.Ped.IN"="PedIN","Key.Bridge.East.Ped.OUT"="PedOUT","Key.Bridge.East.Bike.IN"="BikeIN","Key.Bridge.East.Bike.OUT"="BikeOUT"))
KeyBridgeWest11<- rename(KeyBridgeWest11, c("X"="time", "Key.Bridge.West"="Total", "Key.Bridge.West.Ped.IN"="PedIN","Key.Bridge.West.Ped.OUT"="PedOUT","Key.Bridge.West.Bike.IN"="BikeIN","Key.Bridge.West.Bike.OUT"="BikeOUT"))
KeyBridgeWest12<- rename(KeyBridgeWest12, c("X"="time", "Key.Bridge.West"="Total", "Key.Bridge.West.Ped.IN"="PedIN","Key.Bridge.West.Ped.OUT"="PedOUT","Key.Bridge.West.Bike.IN"="BikeIN","Key.Bridge.West.Bike.OUT"="BikeOUT"))
KeyBridgeWest13<- rename(KeyBridgeWest13, c("X"="time", "Key.Bridge.West"="Total", "Key.Bridge.West.Ped.IN"="PedIN","Key.Bridge.West.Ped.OUT"="PedOUT","Key.Bridge.West.Bike.IN"="BikeIN","Key.Bridge.West.Bike.OUT"="BikeOUT"))
TR_Island11<- rename(TR_Island11, c("X"="time", "TR.Island"="Total", "TR.Island.Bridge.Ped.IN"="PedIN", "TR.Island.Bridge.Ped.OUT"="PedOUT", "TR.Island.Bridge.Bike.IN"="BikeIN", "TR.Island.Bridge.Bike.OUT"="BikeOUT"))
TR_Island12<- rename(TR_Island12, c("X"="time", "TR.Island"="Total", "TR.Island.Bridge.Ped.IN"="PedIN", "TR.Island.Bridge.Ped.OUT"="PedOUT", "TR.Island.Bridge.Bike.IN"="BikeIN", "TR.Island.Bridge.Bike.OUT"="BikeOUT"))
TR_Island13<- rename(TR_Island13, c("X"="time", "TR.Island"="Total", "TR.Island.Bridge.Ped.IN"="PedIN", "TR.Island.Bridge.Ped.OUT"="PedOUT", "TR.Island.Bridge.Bike.IN"="BikeIN", "TR.Island.Bridge.Bike.OUT"="BikeOUT"))


#Cleaning data for 5 variable files, then normalizing with 7 variables with null ped and bike columns
FourMile11<- rename(FourMile11, c("X"="time", "Four.Mile.Run.Pyro.04"="Total","Four.Mile.Run.Pyro.OUT"="TotalOUT","Four.Mile.Run.Pyro.IN"="TotalIN"))
FourMile11$PedIN<-"NA"
FourMile11$PedOUT<-"NA"
FourMile11$BikeIN<-"NA"
FourMile11$BikeOUT<-"NA"
FourMile11$PedTOTAL<-"NA"
FourMile11$BikeTOTAL<-"NA"
FourMile12<- rename(FourMile12, c("X"="time", "Four.Mile.Run.Pyro.04"="Total","Four.Mile.Run.Pyro.OUT"="TotalOUT","Four.Mile.Run.Pyro.IN"="TotalIN"))
FourMile12$PedIN<-"NA"
FourMile12$PedOUT<-"NA"
FourMile12$BikeIN<-"NA"
FourMile12$BikeOUT<-"NA"
FourMile12$PedTOTAL<-"NA"
FourMile12$BikeTOTAL<-"NA"
FourMile13<- rename(FourMile13, c("X"="time", "Four.Mile.Run.Pyro.04"="Total","Four.Mile.Run.Pyro.OUT"="TotalOUT","Four.Mile.Run.Pyro.IN"="TotalIN"))
FourMile13$PedIN<-"NA"
FourMile13$PedOUT<-"NA"
FourMile13$BikeIN<-"NA"
FourMile13$BikeOUT<-"NA"
FourMile13$PedTOTAL<-"NA"
FourMile13$BikeTOTAL<-"NA"
#Rosslyn is deficient in another way.. it has bike and pedestrian broken out, but no in or out.. it has bike and pedestrian tally's broken down, but only total counts.. to normalize I renamed and then added missing columns with null values
Rosslyn09<- rename(Rosslyn09, c("X"="time", "Custis.Rosslyn"="Total","Custis.Rosslyn.Ped"="PedTOTAL","Custis.Rosslyn.Bike"="BikeTOTAL"))
Rosslyn09$PedIN<-"NA"
Rosslyn09$PedOUT<-"NA"
Rosslyn09$BikeIN<-"NA"
Rosslyn09$BikeOUT<-"NA"
Rosslyn09$TotalIN<-"NA"
Rosslyn09$TotalOUT<-"NA"
Rosslyn10<- rename(Rosslyn10, c("X"="time", "Custis.Rosslyn"="Total","Custis.Rosslyn.Ped"="PedTOTAL","Custis.Rosslyn.Bike"="BikeTOTAL"))
Rosslyn10$PedIN<-"NA"
Rosslyn10$PedOUT<-"NA"
Rosslyn10$BikeIN<-"NA"
Rosslyn10$BikeOUT<-"NA"
Rosslyn10$TotalIN<-"NA"
Rosslyn10$TotalOUT<-"NA"
Rosslyn11<- rename(Rosslyn11, c("X"="time", "Custis.Rosslyn"="Total","Custis.Rosslyn.Ped"="PedTOTAL","Custis.Rosslyn.Bike"="BikeTOTAL"))
Rosslyn11$PedIN<-"NA"
Rosslyn11$PedOUT<-"NA"
Rosslyn11$BikeIN<-"NA"
Rosslyn11$BikeOUT<-"NA"
Rosslyn11$TotalIN<-"NA"
Rosslyn11$TotalOUT<-"NA"
Rosslyn12<- rename(Rosslyn12, c("X"="time", "Custis.Rosslyn"="Total","Custis.Rosslyn.Ped"="PedTOTAL","Custis.Rosslyn.Bike"="BikeTOTAL"))
Rosslyn12$PedIN<-"NA"
Rosslyn12$PedOUT<-"NA"
Rosslyn12$BikeIN<-"NA"
Rosslyn12$BikeOUT<-"NA"
Rosslyn12$TotalIN<-"NA"
Rosslyn12$TotalOUT<-"NA"
Rosslyn13<- rename(Rosslyn13, c("X"="time", "Custis.Rosslyn"="Total","Custis.Rosslyn.Ped"="PedTOTAL","Custis.Rosslyn.Bike"="BikeTOTAL"))
Rosslyn13$PedIN<-"NA"
Rosslyn13$PedOUT<-"NA"
Rosslyn13$BikeIN<-"NA"
Rosslyn13$BikeOUT<-"NA"
Rosslyn13$TotalIN<-"NA"
Rosslyn13$TotalOUT<-"NA"

#Adding Location Tag Columns to all, and calculating other variables
Airport11$Location <- "MVT_Airport_S"
Airport11$CounterID <- 10
Airport11$PedTOTAL <- Airport11$PedIN + Airport11$PedOUT
Airport11$BikeTOTAL<-Airport11$BikeIN + Airport11$BikeOUT
Airport11$TotalOUT<-Airport11$PedOUT + Airport11$BikeOUT
Airport11$TotalIN<-Airport11$PedIN + Airport11$BikeIN
Airport12$Location <- "MVT_Airport_S"
Airport12$CounterID <- 10
Airport12$PedTOTAL <- Airport12$PedIN + Airport12$PedOUT
Airport12$BikeTOTAL<-Airport12$BikeIN + Airport12$BikeOUT
Airport12$TotalOUT<-Airport12$PedOUT + Airport12$BikeOUT
Airport12$TotalIN<-Airport12$PedIN + Airport12$BikeIN
Airport13$Location <- "MVT_Airport_S"
Airport13$CounterID <- 10
Airport13$PedTOTAL <- Airport13$PedIN + Airport13$PedOUT
Airport13$BikeTOTAL<-Airport13$BikeIN + Airport13$BikeOUT
Airport13$TotalOUT<-Airport13$PedOUT + Airport13$BikeOUT
Airport13$TotalIN<-Airport13$PedIN + Airport13$BikeIN
Ballston12$Location<-"Ballston"
Ballston12$CounterID <- 26
Ballston12$PedTOTAL <- Ballston12$PedIN + Ballston12$PedOUT
Ballston12$BikeTOTAL<-Ballston12$BikeIN + Ballston12$BikeOUT
Ballston12$TotalOUT<-Ballston12$PedOUT + Ballston12$BikeOUT
Ballston12$TotalIN<-Ballston12$PedIN + Ballston12$BikeIN
Ballston13$Location<-"Ballston"
Ballston13$CounterID <- 26
Ballston13$PedTOTAL <- Ballston13$PedIN + Ballston13$PedOUT
Ballston13$BikeTOTAL<-Ballston13$BikeIN + Ballston13$BikeOUT
Ballston13$TotalOUT<-Ballston13$PedOUT + Ballston13$BikeOUT
Ballston13$TotalIN<-Ballston13$PedIN + Ballston13$BikeIN
Bluemont13$Location<-"Bluemont"
Bluemont13$CounterID <- 25
Bluemont13$PedTOTAL <- Bluemont13$PedIN + Bluemont13$PedOUT
Bluemont13$BikeTOTAL<-Bluemont13$BikeIN + Bluemont13$BikeOUT
Bluemont13$TotalOUT<-Bluemont13$PedOUT + Bluemont13$BikeOUT
Bluemont13$TotalIN<-Bluemont13$PedIN + Bluemont13$BikeIN
BonAir_Custis10$Location<-"BonAir_Custis"
BonAir_Custis10$CounterID <- 6
BonAir_Custis10$PedTOTAL <- BonAir_Custis10$PedIN + BonAir_Custis10$PedOUT
BonAir_Custis10$BikeTOTAL<-BonAir_Custis10$BikeIN + BonAir_Custis10$BikeOUT
BonAir_Custis10$TotalOUT<-BonAir_Custis10$PedOUT + BonAir_Custis10$BikeOUT
BonAir_Custis10$TotalIN<-BonAir_Custis10$PedIN + BonAir_Custis10$BikeIN
BonAir_Custis11$Location<-"BonAir_Custis"
BonAir_Custis11$CounterID <- 6
BonAir_Custis11$PedTOTAL <- BonAir_Custis11$PedIN + BonAir_Custis11$PedOUT
BonAir_Custis11$BikeTOTAL<-BonAir_Custis11$BikeIN + BonAir_Custis11$BikeOUT
BonAir_Custis11$TotalOUT<-BonAir_Custis11$PedOUT + BonAir_Custis11$BikeOUT
BonAir_Custis11$TotalIN<-BonAir_Custis11$PedIN + BonAir_Custis11$BikeIN
BonAir_Custis12$Location<-"BonAir_Custis"
BonAir_Custis12$CounterID <- 6
BonAir_Custis12$PedTOTAL <- BonAir_Custis12$PedIN + BonAir_Custis12$PedOUT
BonAir_Custis12$BikeTOTAL<-BonAir_Custis12$BikeIN + BonAir_Custis12$BikeOUT
BonAir_Custis12$TotalOUT<-BonAir_Custis12$PedOUT + BonAir_Custis12$BikeOUT
BonAir_Custis12$TotalIN<-BonAir_Custis12$PedIN + BonAir_Custis12$BikeIN
BonAir_Custis13$Location<-"BonAir_Custis"
BonAir_Custis13$CounterID <- 6
BonAir_Custis13$PedTOTAL <- BonAir_Custis13$PedIN + BonAir_Custis13$PedOUT
BonAir_Custis13$BikeTOTAL<-BonAir_Custis13$BikeIN + BonAir_Custis13$BikeOUT
BonAir_Custis13$TotalOUT<-BonAir_Custis13$PedOUT + BonAir_Custis13$BikeOUT
BonAir_Custis13$TotalIN<-BonAir_Custis13$PedIN + BonAir_Custis13$BikeIN
BonAir_WOD_East11$Location<- "BonAir_East"
BonAir_WOD_East11$CounterID <- 7
BonAir_WOD_East11$PedTOTAL <- BonAir_WOD_East11$PedIN + BonAir_WOD_East11$PedOUT
BonAir_WOD_East11$BikeTOTAL<-BonAir_WOD_East11$BikeIN + BonAir_WOD_East11$BikeOUT
BonAir_WOD_East11$TotalOUT<-BonAir_WOD_East11$PedOUT + BonAir_WOD_East11$BikeOUT
BonAir_WOD_East11$TotalIN<-BonAir_WOD_East11$PedIN + BonAir_WOD_East11$BikeIN
BonAir_WOD_East12$Location<- "BonAir_East"
BonAir_WOD_East12$CounterID <- 7
BonAir_WOD_East12$PedTOTAL <- BonAir_WOD_East12$PedIN + BonAir_WOD_East12$PedOUT
BonAir_WOD_East12$BikeTOTAL<-BonAir_WOD_East12$BikeIN + BonAir_WOD_East12$BikeOUT
BonAir_WOD_East12$TotalOUT<-BonAir_WOD_East12$PedOUT + BonAir_WOD_East12$BikeOUT
BonAir_WOD_East12$TotalIN<-BonAir_WOD_East12$PedIN + BonAir_WOD_East12$BikeIN
BonAir_WOD_East13$Location<- "BonAir_East"
BonAir_WOD_East13$CounterID <- 7
BonAir_WOD_East13$PedTOTAL <- BonAir_WOD_East13$PedIN + BonAir_WOD_East13$PedOUT
BonAir_WOD_East13$BikeTOTAL<-BonAir_WOD_East13$BikeIN + BonAir_WOD_East13$BikeOUT
BonAir_WOD_East13$TotalOUT<-BonAir_WOD_East13$PedOUT + BonAir_WOD_East13$BikeOUT
BonAir_WOD_East13$TotalIN<-BonAir_WOD_East13$PedIN + BonAir_WOD_East13$BikeIN
BonAir_WOD_West12$Location<- "BonAir_West"
BonAir_WOD_West12$CounterID <- 27
BonAir_WOD_West12$PedTOTAL <- BonAir_WOD_West12$PedIN + BonAir_WOD_West12$PedOUT
BonAir_WOD_West12$BikeTOTAL<- BonAir_WOD_West12$BikeIN + BonAir_WOD_West12$BikeOUT
BonAir_WOD_West12$TotalOUT<- BonAir_WOD_West12$PedOUT + BonAir_WOD_West12$BikeOUT
BonAir_WOD_West12$TotalIN<- BonAir_WOD_West12$PedIN + BonAir_WOD_West12$BikeIN
BonAir_WOD_West13$Location<- "BonAir_West"
BonAir_WOD_West13$CounterID <- 27
BonAir_WOD_West13$PedTOTAL <- BonAir_WOD_West13$PedIN + BonAir_WOD_West13$PedOUT
BonAir_WOD_West13$BikeTOTAL<- BonAir_WOD_West13$BikeIN + BonAir_WOD_West13$BikeOUT
BonAir_WOD_West13$TotalOUT<- BonAir_WOD_West13$PedOUT + BonAir_WOD_West13$BikeOUT
BonAir_WOD_West13$TotalIN<- BonAir_WOD_West13$PedIN + BonAir_WOD_West13$BikeIN
CCityCon11$Location<- "Crystal_City_Connector"
CCityCon11$CounterID <- 11
CCityCon11$PedTOTAL <- CCityCon11$PedIN + CCityCon11$PedOUT
CCityCon11$BikeTOTAL<-CCityCon11$BikeIN + CCityCon11$BikeOUT
CCityCon11$TotalOUT<-CCityCon11$PedOUT + CCityCon11$BikeOUT
CCityCon11$TotalIN<-CCityCon11$PedIN + CCityCon11$BikeIN
CCityCon12$Location<- "Crystal_City_Connector"
CCityCon12$CounterID <- 11
CCityCon12$PedTOTAL <- CCityCon12$PedIN + CCityCon12$PedOUT
CCityCon12$BikeTOTAL<-CCityCon12$BikeIN + CCityCon12$BikeOUT
CCityCon12$TotalOUT<-CCityCon12$PedOUT + CCityCon12$BikeOUT
CCityCon12$TotalIN<-CCityCon12$PedIN + CCityCon12$BikeIN
CCityCon13$Location<- "Crystal_City_Connector"
CCityCon13$CounterID <- 11
CCityCon13$PedTOTAL <- CCityCon13$PedIN + CCityCon13$PedOUT
CCityCon13$BikeTOTAL<-CCityCon13$BikeIN + CCityCon13$BikeOUT
CCityCon13$TotalOUT<-CCityCon13$PedOUT + CCityCon13$BikeOUT
CCityCon13$TotalIN<-CCityCon13$PedIN + CCityCon13$BikeIN
ColPike11$Location<- "Columbia_Pike"
ColPike11$CounterID <- 14
ColPike11$PedTOTAL <- ColPike11$PedIN + ColPike11$PedOUT
ColPike11$BikeTOTAL<-ColPike11$BikeIN + ColPike11$BikeOUT
ColPike11$TotalOUT<-ColPike11$PedOUT + ColPike11$BikeOUT
ColPike11$TotalIN<-ColPike11$PedIN + ColPike11$BikeIN
ColPike12$Location<- "Columbia_Pike"
ColPike12$CounterID <- 14
ColPike12$PedTOTAL <- ColPike12$PedIN + ColPike12$PedOUT
ColPike12$BikeTOTAL<-ColPike12$BikeIN + ColPike12$BikeOUT
ColPike12$TotalOUT<-ColPike12$PedOUT + ColPike12$BikeOUT
ColPike12$TotalIN<-ColPike12$PedIN + ColPike12$BikeIN
EFC10$Location<- "East_Falls_Church"
EFC10$CounterID <- 5
EFC10$PedTOTAL <- EFC10$PedIN + EFC10$PedOUT
EFC10$BikeTOTAL<-EFC10$BikeIN + EFC10$BikeOUT
EFC10$TotalOUT<-EFC10$PedOUT + EFC10$BikeOUT
EFC10$TotalIN<-EFC10$PedIN + EFC10$BikeIN
EFC11$Location<- "East_Falls_Church"
EFC11$CounterID <- 5
EFC11$PedTOTAL <- EFC11$PedIN + EFC11$PedOUT
EFC11$BikeTOTAL<-EFC11$BikeIN + EFC11$BikeOUT
EFC11$TotalOUT<-EFC11$PedOUT + EFC11$BikeOUT
EFC11$TotalIN<-EFC11$PedIN + EFC11$BikeIN
EFC12$Location<- "East_Falls_Church"
EFC12$CounterID <- 5
EFC12$PedTOTAL <- EFC12$PedIN + EFC12$PedOUT
EFC12$BikeTOTAL<-EFC12$BikeIN + EFC12$BikeOUT
EFC12$TotalOUT<-EFC12$PedOUT + EFC12$BikeOUT
EFC12$TotalIN<-EFC12$PedIN + EFC12$BikeIN
#I don't need to add new variables to Four_Mile_Run at this point
FourMile11$Location<- "Four_Mile_Run"
FourMile11$CounterID <- 2
FourMile12$Location<- "Four_Mile_Run"
FourMile12$CounterID <- 2
FourMile13$Location<- "Four_Mile_Run"
FourMile13$CounterID <- 2
JoyceNB13$Location<- "Joyce_St_Northbound"
JoyceNB13$CounterID <- 29
JoyceNB13$PedTOTAL <- JoyceNB13$PedIN + JoyceNB13$PedOUT
JoyceNB13$BikeTOTAL<-JoyceNB13$BikeIN + JoyceNB13$BikeOUT
JoyceNB13$TotalOUT<-JoyceNB13$PedOUT + JoyceNB13$BikeOUT
JoyceNB13$TotalIN<-JoyceNB13$PedIN + JoyceNB13$BikeIN
JoyceSB13$Location<- "Joyce_St_Southbound"
JoyceSB13$CounterID <- 28
JoyceSB13$PedTOTAL <- JoyceSB13$PedIN + JoyceSB13$PedOUT
JoyceSB13$BikeTOTAL<-JoyceSB13$BikeIN + JoyceSB13$BikeOUT
JoyceSB13$TotalOUT<-JoyceSB13$PedOUT + JoyceSB13$BikeOUT
JoyceSB13$TotalIN<-JoyceSB13$PedIN + JoyceSB13$BikeIN
KeyBridgeEast11$Location<- "Key_Bridge_East"
KeyBridgeEast11$CounterID <- 9
KeyBridgeEast11$PedTOTAL <- KeyBridgeEast11$PedIN + KeyBridgeEast11$PedOUT
KeyBridgeEast11$BikeTOTAL<-KeyBridgeEast11$BikeIN + KeyBridgeEast11$BikeOUT
KeyBridgeEast11$TotalOUT<-KeyBridgeEast11$PedOUT + KeyBridgeEast11$BikeOUT
KeyBridgeEast11$TotalIN<-KeyBridgeEast11$PedIN + KeyBridgeEast11$BikeIN
KeyBridgeEast12$Location<- "Key_Bridge_East"
KeyBridgeEast12$CounterID <- 9
KeyBridgeEast12$PedTOTAL <- KeyBridgeEast12$PedIN + KeyBridgeEast12$PedOUT
KeyBridgeEast12$BikeTOTAL<-KeyBridgeEast12$BikeIN + KeyBridgeEast12$BikeOUT
KeyBridgeEast12$TotalOUT<-KeyBridgeEast12$PedOUT + KeyBridgeEast12$BikeOUT
KeyBridgeEast12$TotalIN<-KeyBridgeEast12$PedIN + KeyBridgeEast12$BikeIN
KeyBridgeEast13$Location<- "Key_Bridge_East"
KeyBridgeEast13$CounterID <- 9
KeyBridgeEast13$PedTOTAL <- KeyBridgeEast13$PedIN + KeyBridgeEast13$PedOUT
KeyBridgeEast13$BikeTOTAL<-KeyBridgeEast13$BikeIN + KeyBridgeEast13$BikeOUT
KeyBridgeEast13$TotalOUT<-KeyBridgeEast13$PedOUT + KeyBridgeEast13$BikeOUT
KeyBridgeEast13$TotalIN<-KeyBridgeEast13$PedIN + KeyBridgeEast13$BikeIN
KeyBridgeWest11$Location<- "Key_Bridge_West"
KeyBridgeWest11$CounterID <- 8
KeyBridgeWest11$PedTOTAL <- KeyBridgeWest11$PedIN + KeyBridgeWest11$PedOUT
KeyBridgeWest11$BikeTOTAL<-KeyBridgeWest11$BikeIN + KeyBridgeWest11$BikeOUT
KeyBridgeWest11$TotalOUT<-KeyBridgeWest11$PedOUT + KeyBridgeWest11$BikeOUT
KeyBridgeWest11$TotalIN<-KeyBridgeWest11$PedIN + KeyBridgeWest11$BikeIN
KeyBridgeWest12$Location<- "Key_Bridge_West"
KeyBridgeWest12$CounterID <- 8
KeyBridgeWest12$PedTOTAL <- KeyBridgeWest12$PedIN + KeyBridgeWest12$PedOUT
KeyBridgeWest12$BikeTOTAL<-KeyBridgeWest12$BikeIN + KeyBridgeWest12$BikeOUT
KeyBridgeWest12$TotalOUT<-KeyBridgeWest12$PedOUT + KeyBridgeWest12$BikeOUT
KeyBridgeWest12$TotalIN<-KeyBridgeWest12$PedIN + KeyBridgeWest12$BikeIN
KeyBridgeWest13$Location<- "Key_Bridge_West"
KeyBridgeWest13$CounterID <- 8
KeyBridgeWest13$PedTOTAL <- KeyBridgeWest13$PedIN + KeyBridgeWest13$PedOUT
KeyBridgeWest13$BikeTOTAL<-KeyBridgeWest13$BikeIN + KeyBridgeWest13$BikeOUT
KeyBridgeWest13$TotalOUT<-KeyBridgeWest13$PedOUT + KeyBridgeWest13$BikeOUT
KeyBridgeWest13$TotalIN<-KeyBridgeWest13$PedIN + KeyBridgeWest13$BikeIN

Rosslyn09$Location<- "Custis_Rosslyn"
Rosslyn09$CounterID <- 1
Rosslyn10$Location<- "Custis_Rosslyn"
Rosslyn10$CounterID <- 1
Rosslyn11$Location<- "Custis_Rosslyn"
Rosslyn11$CounterID <- 1
Rosslyn12$Location<- "Custis_Rosslyn"
Rosslyn12$CounterID <- 1
Rosslyn13$Location<- "Custis_Rosslyn"
Rosslyn13$CounterID <- 1

TR_Island11$Location<- "TR_Island_Bridge"
TR_Island11$CounterID <- 12
TR_Island11$PedTOTAL <- TR_Island11$PedIN + TR_Island11$PedOUT
TR_Island11$BikeTOTAL<-TR_Island11$BikeIN + TR_Island11$BikeOUT
TR_Island11$TotalOUT<-TR_Island11$PedOUT + TR_Island11$BikeOUT
TR_Island11$TotalIN<-TR_Island11$PedIN + TR_Island11$BikeIN
TR_Island12$Location<- "TR_Island_Bridge"
TR_Island12$CounterID <- 12
TR_Island12$PedTOTAL <- TR_Island12$PedIN + TR_Island12$PedOUT
TR_Island12$BikeTOTAL<-TR_Island12$BikeIN + TR_Island12$BikeOUT
TR_Island12$TotalOUT<-TR_Island12$PedOUT + TR_Island12$BikeOUT
TR_Island12$TotalIN<-TR_Island12$PedIN + TR_Island12$BikeIN
TR_Island13$Location<- "TR_Island_Bridge"
TR_Island13$CounterID <- 12
TR_Island13$PedTOTAL <- TR_Island13$PedIN + TR_Island13$PedOUT
TR_Island13$BikeTOTAL<-TR_Island13$BikeIN + TR_Island13$BikeOUT
TR_Island13$TotalOUT<-TR_Island13$PedOUT + TR_Island13$BikeOUT
TR_Island13$TotalIN<-TR_Island13$PedIN + TR_Island13$BikeIN

#one function to rule them all, one function to find them
#one function to bring them all and in the darkness bind them

#Applying Rbind to bind all together into one CSV
Arl_MUT_Combined <- rbind(Airport11,Airport12,Airport13,Ballston12,Ballston13,
                          Bluemont13,BonAir_Custis10,BonAir_Custis11,BonAir_Custis12,
                          BonAir_Custis13,BonAir_WOD_East11,BonAir_WOD_East12,BonAir_WOD_East13,
                          BonAir_WOD_West12,BonAir_WOD_West13,CCityCon11,CCityCon12,CCityCon13,
                          ColPike11,ColPike12, EFC10, EFC11, EFC12, FourMile11, FourMile12,
                          FourMile13,JoyceNB13, JoyceSB13,KeyBridgeEast11, KeyBridgeEast12, 
                          KeyBridgeEast13, KeyBridgeWest11, KeyBridgeWest12, KeyBridgeWest13, 
                          Rosslyn09,Rosslyn10,Rosslyn11,Rosslyn12,Rosslyn13,
                          TR_Island11,TR_Island12,TR_Island13)

Arl_MUT_Combined$CounterID <- factor(Arl_MUT_Combined$CounterID)
nrow(Arl_MUT_Combined)
# setwd("~/Dropbox/Processed Counter Data & R code/") # Dir on John's computer
# write.csv(Arl_MUT_Combined, "Arl_MUT_Combined.csv")


