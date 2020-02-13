#####################################################################
#                                                                   #
# Description: R script for downloading meteorological data         #
# Author: Marius Philipp                                            #
# Date: 2019-11-06                                                  #
#                                                                   #
# Edit: Jakob wachter 2020-11-02                                    #
# Purpose: Add missing months of 2019 to downloaded data            #
#                                                                   #
#####################################################################

library(raster)
library(ggplot2)
library(sp)
library(RCurl)
library(tidyverse)
library(zoo)
library(lubridate)

setwd("E:\\Uni\\1.Semester_EAGLE\\Programming_and_Geostatistics\\project_task")


################################
### Download from ftp server ###
################################

# Define http and use the getURL from the RCurl package to list the data
http_temp_mean <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/air_temperature_mean/"
http_temp_max <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/air_temperature_max/"
http_evap <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/evapo_p/"
http_prec <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/precipitation/"
http_drought <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/drought_index/"
http_moist <- "ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/soil_moist/"

# List resulting datasets of given url
# Temperature, Precipitation and Drought index is listed within folders for every month
# Evapotranspiration and soil moisture data directly lists all files
result_temp_prec_drought <- getURL(http_temp_mean, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)
result_evap <- getURL(http_evap, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)
result_moist <- getURL(http_moist, verbose=TRUE, ftp.use.epsv=TRUE, dirlistonly = TRUE)

# Split string into pieces by identifying certain pattern that seperates the individual filenames
result_temp_prec_drought_sorted <- str_split(result_temp_prec_drought, "\r\n")
result_evap_sorted <- str_split(result_evap, "\r\n")
result_moist_sorted <- str_split(result_moist, "\r\n")

# Turn list into vector
result_temp_prec_drought_sorted <- result_temp_prec_drought_sorted[[1]]
result_evap_sorted <- result_evap_sorted[[1]]
result_moist_sorted <- result_moist_sorted[[1]]

# Delete one empty entry because of the previously applied pattern
# and the handbooks in german and english
# For precipitation, temperature and drought index:
result_temp_prec_drought_tidy <- result_temp_prec_drought_sorted[1:(length(result_temp_prec_drought_sorted)-3)]

# For evapotranspiration and soil moisture look for .asc files:
relevant_entries <- grep(".asc.gz", result_evap_sorted)
result_evap_tidy <- result_evap_sorted[relevant_entries]
result_moist_tidy <- result_moist_sorted[relevant_entries]

# Define output directory of downloads
out_dir_evap <- paste0(getwd(), "/Raster/DWD/Sum_Evap/Download/")
out_dir_prec <- paste0(getwd(), "/Raster/DWD/Sum_Prec/Download/")
out_dir_temp_mean <- paste0(getwd(), "/Raster/DWD/Mean_Temp/Download/")
out_dir_temp_max <- paste0(getwd(), "/Raster/DWD/Max_Temp/Download/")
out_dir_drought <- paste0(getwd(), "/Raster/DWD/Drought_Index/Download/")
out_dir_moist <- paste0(getwd(), "/Raster/DWD/Soil_Moisture/Download/")

# Function for downloading temperature, precipitation and drought index
download_temp_prec_drought <- function(my_http, my_months, my_outdir, start_year){
  for (i in 1:length(my_months)){
    # print progress
    print(paste0("Processing files for month ", i, " of ", length(my_months)))
    # Define http for current month
    current_http <- paste0(my_http, my_months[i], "/")
    # List resulting datasets of given url
    current_result <- getURL(current_http, verbose=F, ftp.use.epsv=TRUE, dirlistonly = TRUE)
    # Split string into pieces by identifying certain pattern that seperates the individual filenames
    current_result_tidy <- str_split(current_result, "\r\n")
    # Convert resulting list from str_plit() back to vector
    current_result_tidy <- current_result_tidy[[1]]
    # Reorder vector to alphabetically decreasing file names
    current_result_tidy <- sort(current_result_tidy, decreasing = F)
    # Delete first entry which is empty because of the previously applied pattern
    current_result_tidy <- current_result_tidy[2:length(current_result_tidy)]
    # Data can already be subsetted to desired years e.g. starting from 1984
    start_position <- grep(start_year, current_result_tidy)
    current_result_tidy <- current_result_tidy[c(seq(start_position,length(current_result_tidy), by=1))]
    # For loop for downloading all files listed in the ftp-server
    for (j in 1:length(current_result_tidy)) {
      download.file(paste0(current_http, current_result_tidy[j]), paste0(my_outdir, current_result_tidy[j]), quiet = T)
    }
  }
}

# Function for downloading evapotranspiration and soil moisture
download_evap_moist <- function(my_http, my_files, my_outdir){
  # For loop for downloading all files listed in the ftp-server
  for (i in 1:length(my_files)) {
    print(paste0("Downloading file ", i, " of ", length(my_files)))
    download.file(paste0(my_http, my_files[i]), paste0(my_outdir, my_files[i]), quiet = T)
  }
}

# Execute functions
download_temp_prec_drought(my_http = http_drought, my_months = result_temp_prec_drought_tidy, my_outdir = out_dir_drought, start_year = "1984")
download_temp_prec_drought(my_http = http_temp_max, my_months = result_temp_prec_drought_tidy, my_outdir = out_dir_temp_max, start_year = "1984")
download_temp_prec_drought(my_http = http_temp_mean, my_months = result_temp_prec_drought_tidy, my_outdir = out_dir_temp_mean, start_year = "1984")
download_temp_prec_drought(my_http = http_prec, my_months = result_temp_prec_drought_tidy, my_outdir = out_dir_prec, start_year = "1984")
download_evap_moist(my_http = http_evap, my_files = result_evap_tidy, my_outdir = out_dir_evap)
download_evap_moist(my_http = http_moist, my_files = result_moist_tidy, my_outdir = out_dir_moist)


############################
### Read ASCII-Grid-File ###
############################

# Define file names and directory
mypath <- out_dir_prec
temp = list.files(path = mypath, pattern="*.gz$")
filenames <- paste0(mypath, temp)

# read all ascii files and convert them into a raster stack
for (i in 1:length(filenames)){
  print(paste0("Converting file ", i, " of ", length(filenames)))
  if (i == 1){
    # for the first run define our final raster file ...
    current_ascii <- read.asciigrid(filenames[i])
    my_raster <- raster(current_ascii)
  } else {
    # ... and fill it with each additional run with another layer
    current_ascii <- read.asciigrid(filenames[i])
    current_raster <- raster(current_ascii)
    my_raster <- stack(my_raster, current_raster)
    # Delete all variables except for the raster stack "my_raster"
    rm(i, current_ascii, current_raster)
  }
}

# Create a vector with the date names...
# ... for years ...
my_years <- as.character(seq(1984, 2019, by=1))

# ... months ...
my_months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# ... and combine them into one vector with dates ...
dates <- as.vector(outer(my_years, my_months, FUN=paste))
dates <- gsub(" ", "-", dates)
dates <- sort(dates, decreasing = F)

# ... and add the remaining months of 2019
#dates <- c(dates, "2019-01", "2019-02", "2019-03", "2019-04", "2019-05", "2019-06", "2019-07")

# Convert the dates into "Date"-type objects
dates <- as.yearmon(dates)
dates <- as.Date(dates)

# Add 15 days to have a each date in the middle of the month
dates <- dates + 15

# Change names of the raster file based on dates vector
names(my_raster) <- dates


#################################
### Create a time Series plot ###
#################################

# Add 3-degree Gauss-Kruger zone 3, Ellipsoid Bessel, Datum Potsdam (central point Rauenberg), EPSG:31467
# coordinate reference system as mentioned in the description:
# ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/precipitation/
my_crs <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +datum=potsdam +units=m +no_defs +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7" 
my_raster@crs <- sp::CRS(my_crs)

# Define dataframe and fill it with the dates
my_mat <- matrix(data = NA, nrow = length(dates), ncol = 2)
my_df <- data.frame(my_mat)
names(my_df) <- c("Dates", "Sum_Prec")
my_df[,1] <- dates

# For-loop calculating mean of each raster and save it in data.frame
for (i in 1:length(dates)){
  print(paste0("Processing layer ", i, " of ", length(dates)))
  current_layer <- my_raster[[i]]
  current_mean <- mean(getValues(current_layer), na.rm=T)
  my_df[i,2] <- current_mean
  rm(current_layer, current_mean, i)
}

# Plot resulting dataframe and perform a regression analysis to display a trend line
ggplot(my_df, aes(x=Dates, y=Sum_Prec))+
  geom_point(size=2)+
  geom_line()+
  geom_smooth(method="lm", se=TRUE, formula= y ~ x)+
  labs(title="Time Series of Mean Precipitation Across Germany", 
       x="Year", y="Mean Precipitation in mm") +
  theme(plot.title = element_text(hjust = 0.5))

# Get mean precipitation for a specific month
June <- which(month(my_df$Dates) %in% 6)
my_df_subset <- my_df[June,]
mean_prec_june <- mean(my_df_subset$Sum_Prec)
