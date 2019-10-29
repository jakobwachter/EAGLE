### spatial exercise
### 22.10.19

## setwd

setwd("E:\\Uni\\1. Semester EAGLE\\Programming and Geostatistics")

## install packages

install.packages("raster")

require(raster)

# get outline of GER

germany<- getData("GADM", country="DEU", level=2)
plot(germany)

# get precipitation data

prec<- getData("worldclim", var="prec", res=.5, lon=10, lat= 51)
plot(prec)

prec_ger1<-crop(prec,germany)
spplot(prec_ger1)

prec_ger2<- mask(prec_ger1, germany)
spplot(prec_ger2)

#Test to skip crop

prec_ger3<- mask(prec, germany)
spplot(prec_ger3)

prec_ger4<-crop(prec_ger3, germany)
spplot(prec_ger4)

# extract statistics

prec_avg<- cellStats(prec_ger2, stat="mean")
plot(prec_avg,cex=1.5,pch=19,col="orange")

ccodes()
fiji<- getData("GADM", country="FJI", level=2)
plot(fiji)

