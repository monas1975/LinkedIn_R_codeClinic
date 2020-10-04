

# install.packages("rjson")
# install.packages("mapdata")

library(rjson)
library(maps)
library(mapdata)


myIPaddress <- readLines("http://ipv4bot.whatismyipaddress.com/", warn = FALSE)
getGeoString <- paste0("https://ipinfo.io/", myIPaddress,"/json")

latlong <- strsplit(myLocation$loc, ",")

myLongitude <- as.numeric(latlong[[1]][2])
myLatitude <- as.numeric(unlist(latlong)[1])

maprange = 7

map("worldHires", 
    xlim = c(myLongitude-maprange, myLongitude+maprange),
    ylim = c(myLatitude-maprange, myLatitude+maprange),
    col = "gray80", fill = TRUE,
    mar = c(1.1, 1.1, par("mar")[3], 2))
points(myLongitude, myLatitude, col="red", pch = 8, cex = 1)
title(paste(myLocation$city, ", ", myLocation$region, " - ", myLocation$country))
map.cities( label = TRUE, minpop = 500000)