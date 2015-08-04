#making maps of indiana field sites

library(RgoogleMaps)

fs<-read.csv("fieldSites.csv")

center = c(mean(fs$lat), mean(fs$lon)) 


fs$size <- "small"  #create a column indicating size of marker
fs$col <- "red"   #create a column indicating color of marker
fs$char <- ""   #normal Google Maps pinpoints will be drawn
fs$label <- fs$name
mymarkers <- cbind.data.frame(fs$lat, fs$lon, fs$label, fs$size, 
                              fs$col, fs$char)   #create the data frame by binding my data columns of GPS coordinates with the newly created columns
names(mymarkers) <- c("lat", "lon", "label", "size", "col", "char")  #assign column headings

Indi <- GetMap.bbox(lonR= range(lon), latR= range(lat),
                             center= center,
                             destfile= "sitesClose.png",
                             markers= mymarkers, zoom=10, 
                             maptype="hybrid")

Indi <- GetMap.bbox(lonR= range(lon), latR= range(lat),
                    center= center,
                    destfile= "sitesWayOut.png",
                    markers= mymarkers, zoom=5, 
                    maptype="hybrid")

Indi <- GetMap.bbox(lonR= range(lon), latR= range(lat),
                    center= center,
                    destfile= "sitesRoad.png",
                    markers= mymarkers, zoom=10, 
                    maptype="terrain")

