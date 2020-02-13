#####################################################################
#                                                                   #
# Description: R script for computing scPDSI data based on          #
#              precipitation and potential evapotranspiration data  #
# Author: Marius Philipp                                            #
# Date: 2019-11-06                                                  #
#                                                                   #
# Edit: Jakob wachter 2020-11-02                                    #
# Purpose: Add missing months of 2019 to calculated scPDSI          #                                                                  #
#####################################################################

library(raster)
library(ggplot2)
library(sp)
library(tidyverse)
library(scPDSI)
library(zoo)
library(lubridate)
library(plotly)
library(SpatialDataToolbox)

setwd("E:\\Uni\\1.Semester_EAGLE\\Programming_and_Geostatistics\\project_task")


# Define directories to precipitation and to potential evopotranspiration data
prec_path <- paste0(getwd(), "/Raster/DWD/Sum_Prec/Download/")
evap_path <- paste0(getwd(), "/Raster/DWD/Sum_Evap/Download/")

# List all prec and evap files
prec_filenames = list.files(path = prec_path, pattern="*.gz$", full.names = T)
evap_filenames = list.files(path = evap_path, pattern="*.gz$", full.names = T)

# Subset precipitation data to match evapotranspiration data (only available since 1991)
prec_filenames <- prec_filenames[85:(length(prec_filenames))]
evap_filenames <- evap_filenames[1:(length(evap_filenames))]


###########################################
### Create a vector with the date names ###
###########################################

# ... for years ...
my_years <- as.character(seq(1991, 2019, by=1))

# ... months ...
my_months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# ... and combine them into one vector with dates ...
dates <- as.vector(outer(my_years, my_months, FUN=paste))
dates <- gsub(" ", "-", dates)
dates <- sort(dates, decreasing = F)

# Add months for 2019 manually
# dates <- c(dates, "2019-01", "2019-02","2019-03", "2019-04", "2019-05", "2019-06", "2019-07")

# Convert the dates into "Date"-type objects
dates <- as.yearmon(dates)
dates <- as.Date(dates)

# Add 15 days to have a each date in the middle of the month
dates <- dates + 15

# get only summer months
dates_months <- lubridate::month(dates)
summer_month_positions <- which(dates_months %in% c(5:10))
dates_summer <- dates[summer_month_positions]


############################
### Read ASCII-Grid-File ###
############################

# Function for reading all ascii files and converting them into a raster stack
convert_fun <- function(my_filenames){
  for (i in 1:length(my_filenames)){
    print(paste0("Converting file ", i, " of ", length(my_filenames)))
    if (i == 1){
      # for the first run define our final raster file ...
      current_ascii <- read.asciigrid(my_filenames[i])
      my_raster <- raster(current_ascii)
    } else {
      # ... and fill it with each additional run with another layer
      current_ascii <- read.asciigrid(my_filenames[i])
      current_raster <- raster(current_ascii)
      my_raster <- stack(my_raster, current_raster)
    }
  }
  return(my_raster)
}

# Execute function
prec_data <- convert_fun(my_filenames = prec_filenames)
evap_data <- convert_fun(my_filenames = evap_filenames)

# Add 3-degree Gauss-Kruger zone 3, Ellipsoid Bessel, Datum Potsdam (central point Rauenberg), EPSG:31467
# coordinate reference system as mentioned in the description:
# ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/precipitation/
my_crs <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7"
prec_data@crs <- sp::CRS(my_crs)
evap_data@crs <- sp::CRS(my_crs)

# Divide evapotranspiration raster by 10 to get mm as mentioned in the description:
# ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/evapo_p/
evap_data <- evap_data/10

# Change names of the raster files based on dates vector
names(prec_data) <- dates
names(evap_data) <- dates


######################################################
### Subset extent of rasterstack into 100 polygons ###
######################################################

# Assign extent of raster stack as a polygon
my_poly <- as(extent(prec_data), 'SpatialPolygons')
proj4string(my_poly) <- CRS(as.character(crs(prec_data)))

# Define function for creating a fishnet polygon out of the extent
fishnet_fun <- function(my_poly, diff_factor){
  # Define the extent
  my_ext <- extent(my_poly)
  # Caclulate horizontal and vertical difference
  x_diff <- my_ext[2] - my_ext[1]
  y_diff <- my_ext[4] - my_ext[3]
  # Divide x_diff and y_diff by 10 to get distance of 10 chunks in each direction
  x_diff_chunk <- x_diff/diff_factor
  y_diff_chunk <- y_diff/diff_factor
  # Start with for-loop to create a polygon for out of each x-y chunk
  for (i in 1:10){
    # First define y start and end
    # If i == 1 use the min y as a start and add one y_diff_chunk as end
    if (i == 1){
      y_start <- my_ext[3]
      y_end <- my_ext[3] + y_diff_chunk
      # If i =! 1 use the min y - (i-1) * the y_diff_chunk as a start and add i*y_diff_chunk as end
    } else {
      y_start <- my_ext[3] + (i-1)*y_diff_chunk
      y_end <- my_ext[3] + i*y_diff_chunk
    }
    # Now define x start and end
    for (j in 1:10){
      # If j == 1 use the x min as a start and add one x_diff_chunk as end
      if (j == 1){
        x_start <- my_ext[1]
        x_end <- my_ext[1] + x_diff_chunk
        # If j =! 1 use the min x - (j-1) * the x_diff_chunk as a start and add j*x_diff_chunk as end
      } else {
        x_start <- my_ext[1] + (j-1)*x_diff_chunk
        x_end <- my_ext[1] + j*x_diff_chunk
      }
      # Create extent from coordinates
      ext <- extent(x_start,x_end,y_start,y_end)
      # Convert coordinates to a SpatialPolygons object
      sp <- as(ext, 'SpatialPolygons')
      # Create SpatialPolygonsDataFrame from SpatialPolygons object
      # Define the number of polygon
      if (i == 1){
        current_number <- j
      } else {
        current_number <- (i-1)*10 + j
      }
      data = data.frame(name=current_number)
      spdf = SpatialPolygonsDataFrame(sp,data)
      # Combine single polygons into a "row"
      if (j == 1){
        final_row <- spdf
      } else {
        final_row <- rbind(final_row, spdf)
      }
    }
    # Combine all "rows" to have the final polygon
    if (i == 1){
      final_polygon <- final_row
    } else {
      final_polygon <- rbind(final_polygon, final_row)
    }
  }
  # Assign original projection to polygon
  proj4string(final_polygon) <- CRS(as.character(crs(my_poly)))
  return(final_polygon)
}

# Execute finshnet function
fishnet_poly <- fishnet_fun(my_poly = my_poly, diff_factor = 10)


####################################
### Pixelwise scPDSI calculation ###
####################################

# Function which calculates the scPDSI for every pixel of a raster stack.
# I use the fishnet polygon to subset the raster into smaller chunks which
# is more efficient to calculate than one big raster file
scPDSI_raster_function <- function(my_prec, my_evap, my_poly, my_start){
  # Start for loop for each feature within the polygon
  for (i in 1:nrow(my_poly)){
    print(paste0("Working on subset ", i, " of ", nrow(my_poly)))
    # Define current polygon
    current_poly <- my_poly[i,]
    # Crop precipitation and evapotranspiration data to current polygon
    prec_crop <- crop(my_prec, current_poly)
    evap_crop <- crop(my_evap, current_poly)
    # Define the prec data as a new raster wich will be filled with the scPDSI values
    new_raster <- prec_crop
    # Start for loop to calculate scPDSI for every pixel
    for(j in 1:ncell(prec_crop)){
      # Define prec and evap values of one pixel from all layers as a vector
      current_prec_values <- as.vector(prec_crop[j])
      current_evap_values <- as.vector(evap_crop[j])
      # If there are no NA values, calculate the scPDSI
      # If there are NAs it will not do anything and therefore keep the original NA value for that pixel
      if (any(is.na(current_prec_values)) == F) {
        current_sc_pdsi <- pdsi(current_prec_values, current_evap_values, start = my_start, sc = T)
        current_sc_pdsi_values <- as.vector(current_sc_pdsi$X)
        new_raster[j] <- current_sc_pdsi_values
      }
    }
    # Create mosaic of individual raster files
    if (i == 1){
      final_mosaic <- new_raster
    } else {
      final_mosaic <- mosaic(final_mosaic, new_raster, fun = mean)
    }
  }
  return(final_mosaic)
}

# Execute function
scPDSI_raster <- scPDSI_raster_function(my_prec = prec_data, my_evap = evap_data, 
                                        my_poly = fishnet_poly, my_start = 1991)

plot(scPDSI_raster[[1]])

# Write scPDSI_raster file to disk
writeRaster(scPDSI_raster, filename = paste0(getwd(), "/Raster/DWD/scPDSI/scPDSI_raster.tif"), 
            format = "GTiff")


#######################################
### Plot scPDSI for the Steigerwald ###
#######################################

# Define scPDSI raster
scPDSI_raster <- brick(paste0(getwd(), "/Raster/DWD/scPDSI/scPDSI_raster.tif"))

# Create polygon which covers the Steigerwald
steigerald_extent <- extent(3597158, 3635735, 5507158, 5539873)
steigerwald_poly <- as(steigerald_extent, 'SpatialPolygons')
proj4string(steigerwald_poly) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7" )

# Crop scPDSI_raster to the Steigerwald aoi
scPDSI_raster_crop <- crop(scPDSI_raster, steigerwald_poly)

# Get mean of values from each raster layer
for (i in 1:nlayers(scPDSI_raster_crop)){
  current_layer <- scPDSI_raster_crop[[i]]
  current_mean <- mean(getValues(current_layer), na.rm=T)
  if (i == 1){
    steigerald_timeseries <- current_mean
  } else {
    steigerald_timeseries <- c(steigerald_timeseries, current_mean)
  }
}

# Create data.frame out of dates and scPDSI values
steigerwald_timeseries_df <- data.frame(dates = dates, scPDSI = steigerald_timeseries)

# for export
png(filename="./Zwischenergebnisse/scPDSI_Steigerwald/scPDSI_Steigerwald.png", 
    width = 1000, height = 750, pointsize = 52)
ggplot(steigerwald_timeseries_df, aes(x=dates, y=scPDSI))+
  annotate("rect", xmin=steigerwald_timeseries_df$dates[1],
           xmax=steigerwald_timeseries_df$dates[nrow(steigerwald_timeseries_df)],
           ymin=0, ymax=7, alpha=0.1, fill="blue", colour = "blue")+
  annotate("rect", xmin=steigerwald_timeseries_df$dates[1],
           xmax=steigerwald_timeseries_df$dates[nrow(steigerwald_timeseries_df)],
           ymin=-7, ymax=0, alpha=0.1, fill="red", colour = "red")+
  geom_point(size=2)+
  geom_line(size = 1.001)+
  annotate("text", x = as.Date("1995-01-01"), y = 5.5, label = "wet period", colour="blue", size = 8)+
  annotate("text", x = as.Date("1995-01-01"), y = -5.5, label = "dry period", colour="red", size = 8)+
  geom_hline(yintercept = 0, color="black", size=1.1)+
  labs(title="Mean scPDSI time-series over the Steigerwald", 
       x="Date", y="Mean scPDSI") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size=24),
        axis.text=element_text(size=22),
        axis.title=element_text(size=24),
        axis.ticks = element_line(color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        legend.title=element_text(size=24, face="bold"), 
        legend.text=element_text(size=24)) +
  ylim(-7,7)
dev.off()


###############################
### Reproject scPDSI to UTM ###
###############################

# Define scPDSI raster
scPDSI_raster <- brick(paste0(getwd(), "/Raster/DWD/scPDSI/scPDSI_raster.tif"))

# Reproject to UTM
scPDSI_reproj <- projectRaster(scPDSI_raster, method="bilinear", crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Write scPDSI_raster file to disk
writeRaster(scPDSI_reproj, filename = paste0(getwd(), "/Raster/DWD/scPDSI/scPDSI_raster_utm.tif"), 
            format = "GTiff")
