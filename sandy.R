library(maps)
library(maptools)
library(rgdal)

# need this for handling "paste" extra spaces
trim.trailing <- function (x) sub("\\s+$", "", x)

# get track data from Unisys
wx = read.table(file="http://weather.unisys.com/hurricane/atlantic/2012/SANDY/track.dat", skip=3,fill=TRUE)

# join last two columns (type of storm) that read.table didn't parse as one
# and give the data frame real column names
wx$V7 = trim.trailing(paste(wx$V7,wx$V8," "))
wx$V8 = NULL
colnames(wx) = c("Advisory","Latitude","Longitude","Time","WindSpeed","Pressure","Status")

# annotate the name with forecast (+12/+24/etc) projections
wx$Advisory = unlist(strsplit(toString(wx$Advisory),", "))
wx$a = ""
wx$a[grep("\\+",wx$Advisory)] = wx$Advisory[grep("\\+",wx$Advisory)]
wx$Status = trim.trailing(paste(wx$Status,wx$a," "))

# cheap way to make past plots one color and forecast plots another
wx$color = "red"
wx$color[grep("\\+",wx$Advisory)] = "orange"

# only want part of the map
xlim=c(-90,-60)

# plot the map itself
map("state", interior = FALSE, xlim=xlim)
map("state", boundary = FALSE, col="gray", add = TRUE,xlim=xlim)

# plot the track with triangles and colors
lines(x=wx$Longitude,y=wx$Latitude,col="black",cex=0.75)
points(x=wx$Longitude,y=wx$Latitude,col=wx$color,pch=17,cex=0.75)

# annotate it with the current & projects strength status + forecast
text(x=wx$Longitude,y=wx$Latitude,col='blue',labels=wx$Status,adj=c(-.15),cex=0.33)

# get the forecast "cone" from the KML that google's crisis map provides
# NOTE: you need curl & unzip (with funzip) on your system
# NOTE: Google didn't uniquely name the cone they provide, so this will break
#       post-Sandy. I suggest saving the last KML & track files out out if you
#       want to save this for posterity

# this gets the cone and makes it into a string that getKMLcoordinates can process
coneKML = paste(unlist(system("curl -s -o - http://mw1.google.com/mw-weather/maps_hurricanes/three_day_cone.kmz | funzip", intern = TRUE)),collapse="")

# process the coneKML and make a data frame out of it
coords = getKMLcoordinates(textConnection(coneKML))
coords = data.frame(SpatialPoints(coords, CRS('+proj=longlat')))

# this plots the cone and leaves the track visible
polygon(coords$X1,coords$X2, pch=16, border="red", col='#FF000033',cex=0.25)

# no one puts Sandy in a box (but me)
box()
