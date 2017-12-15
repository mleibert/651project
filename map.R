library(RColorBrewer)
library(rgeos)
library(rgdal)
library(maptools)
library(scales)
library(dplyr)
library(ggmap)
library(dplyr)
library(Cairo)
library(RColorBrewer)



aaaa<-sample(0:1,100,T,c(.8,.2))
bbbb<-sample(0:1,100,T,c(.8,.2))

aaaa<-data.frame(aaaa,ifelse( aaaa == 1, T,F),ifelse( bbbb == 1, T,F) )

sum(apply( , 1, function(x) T %in% x))
nrow(dat)

aaaa<-which(apply( (influence.measures(fit)[[2]]), 1, function(x) T %in% x))
 
head(datleaf)
datleafs<-datleaf[,1:4]
head(datleafs)

ppu <- paste0("<b>","Wait Time: ","</b>", datleafs$wait, "<br>",
              "<b>", "Block: ", "</b>", datleafs$block, "<br>")



bd<- readOGR(dsn = ".", layer = 
	"District_Boundary_as_defined_by_boundary_Stones")
bd<- fortify(bd, region = "OBJECTID")
bd<-as.data.frame(bd)



ward <- readOGR(dsn = ".", layer = "Ward_-_2012")
ward <- fortify(ward , region = "WARD")
ward <-as.data.frame(ward )
ward <-ward [,c(2,1,3:ncol(ward ))]
head(ward );wards<-as.numeric(unique(ward$id))


m <- leaflet() 
m <- m %>% addTiles() %>% addProviderTiles("CartoDB.Positron")

colorz<-c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3',
	'#fdb462','#b3de69','#fccde5')

for (i in 1:8) {m<-addPolygons(m,lng=ward[which(ward$id == i),]$long, 
				lat=ward[which(ward$id == i),]$lat, color = "blue",
			fillColor = colorz[i],weight=2,
		   highlightOptions = highlightOptions(color =
			 "white", weight = 2,     bringToFront = TRUE))}

m <- leaflet(datleafs) 
m <- m %>% addTiles() %>% addProviderTiles("CartoDB.Positron") %>% 
		addMarkers( clusterOptions = markerClusterOptions(), 
	label = lapply(ppu , HTML)) %>% 
		 addProviderTiles(providers$CartoDB.Positron)


colorz<-c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3',
	'#fdb462','#b3de69','#fccde5')

for (i in 1:8) {m<-addPolygons(m,lng=ward[which(ward$id == i),]$long, 
				lat=ward[which(ward$id == i),]$lat, color = "blue",
			fillColor = colorz[i],weight=2,
		   highlightOptions = highlightOptions(color =
			 "white", weight = 2,     bringToFront = TRUE))}
m



leaflet() %>% addTiles() 





