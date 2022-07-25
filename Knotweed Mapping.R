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

#right join between HABS and macros
macro.habs <- right_join(HABS,macros,by="Site")


#extract unique habitats that were sampled by surber/drift nets
#assume that the object "macros" is your data frame



bugs.habs <- data.frame(Sample=unique(macro.habs$Sample.ID))
bugs.habs$site <- substr(bugs.habs$Sample,regexpr("HAB",bugs.habs$Sample)[1],(regexpr("HAB",bugs.habs$Sample)[1]+5))
bugs.habs

samples <- unique(macros$Sample.ID)
samples

sampling <- data.frame(Sample.ID=samples)
sampling

macros$Sample.ID

sampling2 <- right_join(macro.habs, sampling, by="Sample.ID")
sampling2
