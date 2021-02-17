library(ggmap)
library(googleway)


key<-"input your google key"
register_google(key = key)
set_key(key, api = "map")

#data import
stn<-read.csv('C:/Users/USER/Desktop/기상청 ASOS 지점.csv')
colnames(stn)<-c("no","stn","name","lat",'lon')


# first option=toner-lite
p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="toner-lite",  source = "stamen")
ggmap(p)


# ggmap with ASOS points
ggmap(p)+ geom_point(data=stn, aes(x=lon, y=lat),size=1, color="red", alpha=0.7)

#second option=terrain-background
p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="terrain-background")
ggmap(p)+ geom_point(data=stn, aes(x=lon, y=lat),size=1.5, color="blue", alpha=0.7)


#third option=terrain
p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="terrain")
ggmap(p)+ geom_point(data=stn, aes(x=lon, y=lat),size=1.5, color="blue", alpha=0.7)

#fourth option=satellite
p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="satellite")
ggmap(p)+ geom_point(data=stn, aes(x=lon, y=lat),size=1.5, color="blue", alpha=0.7)

#fifth option=roadmap
p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="roadmap")
ggmap(p)+ geom_point(data=stn, aes(x=lon, y=lat),size=1.5, color="blue", alpha=0.7)


#sixth option=hybrid
p<-get_map(location = c(127.5, 35.8), zoom = 7, maptype="hybrid")
ggmap(p)+ geom_point(data=stn, aes(x=lon, y=lat),size=1.5, color="blue", alpha=0.7)



# etc options
southKrLocation <- c(125.04, 33.06, 131.52, 38.27) 
krMap <- get_map(location=southKrLocation, source = "stamen", maptype = "watercolor", crop = FALSE)
mymap <- ggmap(krMap, extent = 'device')
mymap+ geom_point(data=stn, aes(x=lon, y=lat),size=1, color="black", alpha=0.7)


#roadmap, terrain, toner, watercolor mymap <- ggmap(krMap, extent = 'device') mymap


m <-get_googlemap (zoom = 12, maptype = "terrain", source = "google")
ggmap(p)+ geom_point(data=air.data1, aes(x=lon, y=lat),size=1, color="red", alpha=0.7)

ggmap:::register_google(AIzaSyCveN7cUl727nRzzbSNSTXB5RCbj61TSJU)
