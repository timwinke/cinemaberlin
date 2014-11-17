require(RCurl)
require(XML)
require(ggmap)
require(dplyr)
Sys.setlocales("LC_ALL", "en_US.UTF-8")

url <- "http://www.kinokompendium.de"
urlroot <- paste(url, "kino_berlin_az.htm", sep="/")
SOURCE <-  getURL(urlroot,encoding="UTF-8") #Download the page
#this is a very very long line. Let's not print it. Instead:
substring (SOURCE,1,200)

PARSED <- htmlParse(SOURCE, encoding="UTF-8") #Format the html code d

links <- xpathSApply(PARSED, "//a[@class='textbezirke']/@href")
cinemanames <- xpathSApply(PARSED, "//a[@class='textbezirke']", xmlValue)


cinemas <- data.frame(name=cinemanames, room=NA, seats=NA, address=NA, lon=NA, lat=NA)
cinemasbind <- data.frame(name=NA, room=NA, seats=NA, address=NA, lon=NA, lat=NA)

#something wrong: cinemanames[27] "CineStar\nCUBIX Alexanderplatz"
# CineStar Kino in der\n  Kulturbrauerei
#"CUBIX CineStar Alexanderplatz" [37]
#cinemanames[38]
#51 "Haus der Berliner Festspiele"
#52 Haus der Kulturen der Welt"
#55 "Kant\nKino"
#Kino in der Kulturbrauerei
#74 "Regenbogenkino
# Toni & Tonino
# Kulturbrauerei
# urania 88
# "Zoo Palast 93
# jörk 

for (i in 94:95){
 urlcinema <- paste(url, links[i], sep="/")
 cinemapage <-  getURL(urlcinema) #Download the page
 cinemapage <- htmlParse(cinemapage, encoding="UTF-8") #Format the html code d
 cinemainfo <- xpathSApply(cinemapage, "//td[@valign='top']", xmlValue)
 cinemainfo <- gsub("\n", "  ",cinemainfo)
 
 cinemaaddress <- xpathSApply(cinemapage, "//td[@width='193']", xmlValue)[1]
 cinemaaddress <- unlist(strsplit(cinemaaddress, split="\n"))
 cinemaaddress <- paste(cinemaaddress[1],cinemaaddress[2],cinemaaddress[3])
 if(substr(cinemaaddress,1,1) == " ") cinemaaddress <- substr(cinemaaddress, 2, 99999)
 cinemaaddress <- gsub("  ", " ",cinemaaddress)
 
 cinemalonlat <- geocode(cinemaaddress)
 
 seats <- cinemainfo[grep("Plätze", cinemainfo)]
 seats <- as.numeric(substr(seats,9,11))
 seats <- seats[!is.na(seats)]
 cinemastemp <- data.frame(name=cinemanames[i], room=1:length(seats), 
                           seats=seats, address=cinemaaddress, lon=cinemalonlat[1], lat=cinemalonlat[2])
 cinemasbind <- rbind(cinemasbind,cinemastemp)
}

cinemasbind <- cinemasbind[2:nrow(cinemasbind),]
write.csv2(cinemasbind, file="cinemas_nov15.csv", fileEncoding ="latin1")
# minlon <- min(cinemasbind$lon, na.rm) - 1
# minlat <- min(cinemasbind$lat, na.rm) - 1
# geocode("Berlin")


#
berlin <- get_map(location = c(13.40495, 52.52001), zoom=11, maptype="roadmap") #Berlin
p <- ggmap(berlin)
p + geom_point(data=cinemasbind, aes(x=lon, y=lat, size=seats, alpha=.20), color="yellow") + 
  geom_text(data=cinemasbind, aes(x=lon, y=lat, label=name, size=1))




install.packages("plotGoogleMaps")
require("plotGoogleMaps")
require("stringr")
require("tidyr")


# Data preparation
# Point data
cinemasbind <- read.csv2(file="cinemas_nov15b.csv")
cinemasbind <- as.data.frame(cinemasbind)

cin <- cinemasbind %>%
  select(name, room, seats) %>%
  spread(room,seats) 

cinemas <- cinemasbind %>% 
  group_by(name) %>%
  summarise(
  seats = max(seats)
  ) %>%
  merge(cinemasbind, by=c("name", "seats")) %>%
  select(name, seats, room, lon,lat) %>%
  mutate(name = as.character(name),
         #name = substr(name,1,1),
         name = str_replace_all(name, "[^[:alnum:]]", " ")) %>%
  merge(cin, by=c("name")) 
cinemas <- cinemas[,1:13]
cinemas[is.na(cinemas)] <- " "
coordinates(cinemas)<- ~lon+lat # convert to SPDF
prjtgmaps <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +epsg=3857" #google Maps prjt
proj4string(cinemas) <- CRS(prjtgmaps)

# adding Coordinate Referent Sys.
# Create web map of Point data
#plot(cinemas)
m<-plotGoogleMaps(cinemas,filename='myMap1.htm',zcol='seats',mapTypeId='ROADMAP')

# zinc labels

m<-plotGoogleMaps(cinemasbind, zcol='seats',filename='myMap_z2.htm',
                  iconMarker=ic, mapTypeId='ROADMAP',layerName = 'MEUSE POINTS')



data(meuse)
coordinates(meuse)<-~x+y # convert to SPDF
proj4string(meuse) <- CRS('+init=epsg:28992')
# adding Coordinate Referent Sys.
# Create web map of Point data
m<-plotGoogleMaps(meuse,filename='myMap1.htm')


m<-bubbleGoogleMaps(meuse,zcol='zinc',max.radius = 80,filename='myMap3.htm')
# remove outlines
m<-bubbleGoogleMaps(meuse,zcol='zinc',max.radius = 80,
                    filename='myMap3.htm',strokeOpacity=0)

