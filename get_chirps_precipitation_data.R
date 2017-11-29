# https://www7.ncdc.noaa.gov/CDO/cdoselect.cmd?datasetabbv=GSOD&resolution=40

# Packages
library(tidyverse)
library(sp)
library(raster)

# Define function to replace NAs (coded in noaa as 99.9 by NOAA)
detect_noaa_na <- function(x){
  x <- as.character(x)
  y <- gsub('.', '', x, fixed = TRUE)
  oks <- y == x
  out <- unlist(lapply(strsplit(y, ''), function(x){all(x == '9')}))
  out[oks] <- FALSE
  out
}

# Define functions for converting from farenheit to celcius
f_to_c <- function(x){
  x <- x - 32
  x <- x * (5/9)
  x
}

# Define function for converting from inches to milimeters
i_to_m <- function(x){
  x <- x * 25.4
  x
}

# Define function to calculate distance from each district centroid to the weather stations
get_distance <- function (lon1, lat1, lon2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

# Define function to get weather for certain location
get_weather_for_location <- function(noaa,
                                     lng, 
                                     lat){
  out <- noaa
  y <- lat
  x <- lng
  
  # Get distance to all locations
  out$distance <- NA
  for (i in 1:nrow(out)){
    out$distance[i] <-
      get_distance(lon1 = lng,
                   lat1 = lat,
                   lon2 = out$lon[i],
                   lat2 = out$lat[i])
  }
  
  # Create a weight column
  out$weight <- 1 / out$distance 
  out$weight[is.infinite(out$weight)] <- 1
  
  # Group by date and get weighted averages
  out <- 
    out %>%
    group_by(date) %>%
    summarise(lat = y,
              lon = x,
              temp = weighted.mean(temp, w = weight, na.rm = TRUE),
              dewp = weighted.mean(dewp, w = weight, na.rm = TRUE),
              wdsp = weighted.mean(wdsp, w = weight, na.rm = TRUE),
              mxspd = weighted.mean(mxspd, w = weight, na.rm = TRUE),
              max = weighted.mean(max, w = weight, na.rm = TRUE),
              min = weighted.mean(min, w = weight, na.rm = TRUE),
              prcp = weighted.mean(prcp, w = weight, na.rm = TRUE))
  return(out)
}

# Fetch weather data for CHIRPS
# ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/

# Define function for creating link to data
create_url <- function(date = '2010-01-01'){
  date <- as.Date(date)
  url <- paste0("ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/",
                format(date, '%Y'),
                "/chirps-v2.0.",
                format(date, '%Y'),
                ".",
                format(date, '%m'),
                ".",
                format(date, "%d"),
                ".tif.gz")
  return(url)
}

# If no weather_data dir, create on
if(!dir.exists('chirps_raw')){
  dir.create('chirps_raw')
}

fetch_weather <- function(date){
  this_date <- date
  start_time <- Sys.time()
  try({
    # Define a file name
    file_name <- paste0('chirps_raw/', this_date, '.tif')
    # Skip if the data is already there
    if(!file.exists(file_name)){
      this_url <- create_url(this_date)
      # Download file
      download.file(url = this_url,
                    destfile = 'temp.tif.gz')
      # Extract
      R.utils::gunzip('temp.tif.gz')
      # Move
      file.copy(from = 'temp.tif',
                to = file_name)
      message('---------------------------------')
      end_time <- Sys.time()
      message(paste0(date, ' done. That took ',
                     round(as.numeric(end_time - start_time), digits = 2),
                     ' seconds.'))
      # Remove the old stuff
      suppressWarnings(file.remove('temp.tif'))
      suppressWarnings(file.remove('temp.tif.gz'))
    }
  })
}

dates <- seq(as.Date('2010-01-01'),
             as.Date('2010-01-05'),
             by = 1)


for (i in 1:length(dates)){
  fetch_weather(date = dates[i])
}

# Read each of the files
files <- dir('chirps_raw/')
files <- files[grepl('.tif', files, fixed = TRUE)]

centroids <- expand.grid(lon = -180:180,
                         lat = -90:90)

# Read in each file and combine
results <- list()
for (i in 1:length(files)){
  this_file <- files[i]
  this_date <- as.Date(gsub('.tif', '', this_file, fixed = TRUE))
  r <- raster(paste0('chirps_raw/', this_file))
  
  # Extract the values
  x <- raster::extract(r, centroids)
  
  # Create a dataframe output
  output <- centroids 
  output$date <- this_date
  output$precipitation <- x
  results[[i]] <- output
  message(this_date)
}

# Add together all the results
precipitation <- bind_rows(results)
