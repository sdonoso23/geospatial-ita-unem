

library(rgeos)
library(rgdal)
library(ggplot2)
library(tidyverse)
library(ggrepel)

#read the shape file and load data
shapefile<-readOGR("shapefiles/Reg2016_ED50.shp",
                   layer ="Reg2016_ED50", 
                   verbose = T, stringsAsFactors = FALSE)
centroids<-data.frame(long=coordinates(shapefile)[,1],
                      lat=coordinates(shapefile)[,2])
data<-read.csv("output/itadata2004.csv")


#get information of coordinates of limits of regions from the shapefile
italiamap <- fortify(shapefile, region = "REGIONE")


#join with data
map <- left_join(italiamap, data, by=c("id"="Territorio"))

#join centroids with ids for names 
centroids<- data%>%
    select(Territorio) %>%
    cbind.data.frame(.,centroids)
    

#plot
mapunem<-ggplot(map, aes(x = long, y = lat, group = group)) + 
    geom_polygon(aes(fill = UNEM)) + 
    coord_equal(ratio=1) + 
    geom_text_repel(data=centroids,
                    aes(label=Territorio,x=long,y=lat, 
                        group=Territorio))+
    theme(legend.position="right",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank())+ 
    scale_fill_gradient(low = "yellow2", high="firebrick3")
    


