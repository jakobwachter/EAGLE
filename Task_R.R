### Introduction to programming and geoanalysis
### task of 22nd october
### Jakob Wachter

## loading a previously created Excel-file into R

# set working directory

setwd("F:\\Uni\\1. Semester EAGLE\\Programming and Geostatistics\\Task _22.10")

# load excel file 

NDVIValues<- read.table("NDVIValues.csv",header = T,sep="")
# or
NDVIValues<- read.csv("NDVIValues.csv",header = T, sep="")

# check data

head(NDVIValues)
summary(NDVIValues)
plot(NDVIValues)

# save as csv file

write.csv(NDVIValues, "NDVIValues_R")
