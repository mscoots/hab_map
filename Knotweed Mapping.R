HABS <- read.csv("garlandbrook_streamsurveyCSV.csv", header=TRUE)
BT <- read.csv("BrookTroutGB_2021_CSV.csv", header=TRUE)
str(HABS)
str(BT)

HABS$Site_Number <- as.numeric(substr(HABS$Site,4,6)) #turn sites from text to number
BT$BT_num <- as.numeric(substr(BT$BT_id,7,9))

windows() #create a larger window to print plot

#plot the longitude and latitude (null graph)
plot(x=HABS$Longitude, y=HABS$Latitude, xlab = "Longitude", ylab = "Latitude",type="n", cex=0.7)
#cex used to alter size of text

#plot locations of knotweed
kw.habs <- HABS[which(HABS$Knotweed_Num>0),]
points(x=kw.habs$Longitude, y=kw.habs$Latitude, cex=sqrt(kw.habs$total_knotweed_area_m2/5),
       col="green")
#cex used to make size of plot point correspondent to total area of knotweed

#plot where brook trout was collected
points(x=BT$Lon,y=BT$Lat, col="red", pch=16)

#plot where macroinvertebrates were collected
points(x=macro.habs$Longitude,y=macro.habs$Latitude, col="#FF9900", pch=16)

#make plot points habitat numbers
text(x=HABS$Longitude, y=HABS$Latitude,labels = HABS$Site_Number,cex=0.7)

#use xlim/ylim to zoom in on areas of the graph
plot(x=HABS$Longitude, y=HABS$Latitude, xlab = "Longitude", ylab = "Latitude",type="n", cex=0.7,
     xlim=c(-71.486,-71.483), ylim=c(44.471,44.473))

cbind(HABS$Site_Number,HABS$total_knotweed_area_m2)



# merge datasets ----------------------------------------------------------
library(dplyr)
macros <- read.csv("total_macro_inventory_2021.csv")
str(macros)
macros$Site <- macros$Sample.Site
flow <- read.csv("GarlandBrook_MacroInvertSampling2.csv")
str(flow)

flow$SiteDate <- paste(flow$Date,flow$Site,sep="_")
#right join between HABS and macros
macro.habs <- right_join(HABS,macros,by="Site")
macro.sample <- right_join(flow,macro.habs,by="Site")

#extract unique habitats that were sampled by surber/drift nets
#assume that the object "macros" is your data frame

bugs.habs <- data.frame(Sample=unique(macro.habs$Sample.ID))
bugs.habs$site <- substr(bugs.habs$Sample,regexpr("HAB",bugs.habs$Sample)[1],(regexpr("HAB",bugs.habs$Sample)[1]+5))

samples <- unique(macros$Sample.ID)
samples

sampling <- data.frame(Sample.ID=samples)
sampling

macros$Sample.ID

sampling2 <- right_join(macro.habs, sampling, by="Sample.ID")
sampling2


# make date columns across dataframes -------------------------------------

#date should be macros, flow, and eventually, our merged data
install.packages("lubridate")
library(lubridate)

head(macros$Date)
macros$DateL <- mdy(macros$Date)
macros$DateL
macros$Date

head(flow$Date)
flow$DateL <- mdy(flow$Date)
flow$DateL

all.dates <- unique(c(flow$DateL,macros$DateL))
all.dates


# start new data frame to bring together ----------------------------------

sites
gears <- as.factor(c("Drift1","Drift2","Surber"))
gears
help <- expand.grid(DateL=all.dates, Site=sites, Gear=gears)
str(help)

library(dplyr)

install.packages("stringr")
library(stringr)

macros$chr2 <- str_sub(macros$Sample.ID,-2,-1)
macros$chr2
macros$chr1 <- str_sub(macros$Sample.ID,-1,-1)

macros$Gear <- ifelse(macros$chr2=="d1", "Drift1",ifelse(macros$chr2=="d2","Drift2",
                      ifelse(macros$chr1=="s","Surber", NA)))
table(is.na(macros$Gear))

bad.gears <- which(is.na(macros$Gear))
bad.gears
table(is.na(macros$Gear))
table(macros$Gear)

macros$Gear[bad.gears] <- c("Drift1",rep("Drift2",9),"Surber")
macros$Gear <- as.factor(macros$Gear)


help <- left_join(help,flow,by=c("DateL","Site"))
head(help)

try <- macros %>% group_by(Sample.ID) %>% count(Order)
try


try2 <- macros %>% group_by(Site, DateL, Gear) %>% count(Order)
try2
str(try2)
View(try2)

#let's try to join flow with try2

please <- left_join(try2,flow,by=c("DateL","Site"))
head(please) #thank whatever deity!
#please is a "tibble" (special type of dataframe)

please.df <- as.data.frame(please)
head(please.df) #please.df is a normal dataframe

install.packages("tidyr")
library(tidyr)
please.df2 <- spread(please.df,Order,n)
head(please.df2)
dim(please.df)
dim(please.df2)

#fix issues in "macros" "Order" column
colnames(please.df2)
macros$Order[which(macros$Order=="Plectoptera" | macros$Order=="Pleoptera " | 
                     macros$Order == "Plecoptera ")] <- "Plecoptera"
macros$Order[which(macros$Order=="Ephemerotpera"|macros$Order=="Ephemerptera"|
                     macros$Order=="ephemeroptera"|macros$Order=="Ephemeroptera "|
                     macros$Order=="Ephemoptera"|macros$Order==
                     "Ephemroptera ")]<-"Ephemeroptera"
macros$Order[which(macros$Order=="Trichoptera ")]<-"Trichoptera"
macros$Order[which(macros$Order=="diptera"|macros$Order=="diptera "|macros$Order
                   =="Diptera "|macros$Order=="Diptera (larva) "|macros$Order==
                     "Diptera?")]<-"Diptera"
macros$Order[which(macros$Order=="arachnida"|macros$Order=="Arachnida")
             ]<-"Araneae"
macros$Order[which(macros$Order=="Coleoptera ")]<-"Coleoptera"
macros$Order[which(macros$Order=="Hemiptera "|macros$Order=="Hemiptera?")
             ]<-"Hemiptera"
macros$Order[which(macros$Order=="hymenoptera"|macros$Order=="Hymenoptera ")
             ]<-"Hymenoptera"
macros$Order[which(macros$Order=="Hydracarina "|macros$Order=="Hydracarnia")
             ]<-"Hydracarina"
macros$Order[which(macros$Order=="Lepidoptera ")]<-"Lepidoptera"
macros$Order[which(macros$Order=="Megaloptera ")]<-"Megaloptera"
macros$Order[which(macros$Order=="Odonata ")]<-"Odonata"
macros$Order[which(macros$Order=="unsure "|macros$Order=="unknown"
                   |macros$Order=="Unsure ")]<-"Unknown"

#cement changes
try2 <- macros %>% group_by(Site, DateL, Gear) %>% count(Order)
please <- left_join(try2,flow,by=c("DateL","Site"))
please.df <- as.data.frame(please)
please.df2 <- spread(please.df,Order,n)
colnames(please.df2)

drift1.sub <- subset(please.df2,Gear=="Drift1")
table(drift1.sub$Gear)
drift2.sub <- subset(please.df2,Gear=="Drift2")
table(drift2.sub$Gear)
surber.sub <- subset(please.df2,Gear=="Surber")
table(surber.sub$Gear)
alldrift.sub <- subset(please.df2,Gear=="Drift1"|Gear=="Drift2") 
View(alldrift.sub)
