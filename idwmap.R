library(sp)
library(gstat)
library(raster)
library(maps)
library(mapdata)
library(ggmap)


gg2<-data
coordinates(gg2)<-c("lon","lat")
grd<-as.data.frame(spsample(gg2, "regular",n=50000))#The higher the number of n, the higher the resolution
names(grd)<-c("lon","lat")
coordinates(grd)<-c("lon","lat")
gridded(grd)=TRUE
fullgrid(grd)<-TRUE

mse.idw <-gstat::idw(mean.mtx~1, gg2, newdata=grd, idp=2)
r<- raster(mse.idw)
plot(r)

