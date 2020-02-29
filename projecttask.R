### analyzing scPDSI trends for different sites, bioclimatic divisions and Land Cover Classes in bavaria
### project work for MB1 - Programming and Geostatistics
### Jakob Wachter
### 18th February 2020

## loading all required packages and setting working directory

require(ggplot2)
require(sp)
require(rgdal)
require(raster)

setwd("E:\\Uni\\1.Semester_EAGLE\\Programming_and_Geostatistics\\project_task")

## loading input data

# previously calculated scPDSI values in 100 fishnet polygons over germany - provided by Marius Philipp, added missing months of 2019 personally

scPDSI_stack<-brick("/Raster/DWD/scPDSI/scPDSI_raster.tif")

# Landklif project quadrants shapefile

landklif_quadrants<-readOGR("Final60Quadrants_epsg25832.shp")

# bioclimatic regions of bavaria shapefile

bioclim_regions<-readOGR("NatGlied_BAY_Meynen-Schmithuesen_UTM.shp")

# Corine Land Cover Classes for germany

CLC_germany<-readOGR("CLC2012.shp")

## check the data for consistency

plot(scPDSI_stack)
head(scPDSI_stack)
tail(scPDSI_stack)





## check for projections 

defaultproj<- proj4string()




