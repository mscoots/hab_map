HABS <- read.csv("garlandbrook_streamsurveyCSV.csv", header=TRUE)
str(HABS)

HABS$Site_Number <- as.numeric(substr(HABS$Site,4,6)) #turn sites from text to number

windows() #create a larger window to print plot

#plot the longitude and latitude (null graph)
plot(x=HABS$Longitude, y=HABS$Latitude, xlab = "Longitude", ylab = "Latitude",type="n", cex=0.7)
#cex used to alter size of text

#plot locations of knotweed
points(x=HABS$Longitude[which(HABS$Knotweed_Num>0)], y=HABS$Latitude[which(HABS$Knotweed_Num>0)], col = "red", pch = 16,
       cex =sqrt(HABS$total_knotweed_area_m2)/5)
#cex used to make size of plot point correspondent to total area of knotweed

text(x=HABS$Longitude, y=HABS$Latitude,labels = HABS$Site_Number,cex=0.7) #make plot points habitat numbers

#use xlim/ylim to zoom in on areas of the graph
#plot(x=HABS$Longitude, y=HABS$Latitude, xlab = "Longitude", ylab = "Latitude",type="n", cex=0.7,
     #xlim=c(-71.490,-71.480), ylim=c(44.47,44.48))
