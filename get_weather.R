library(tidyverse)
library(RCurl)

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

 
get_data <- function(directory = 'noaa_raw',
                     year = 2017){
  url_base <- "ftp://ftp.ncdc.noaa.gov/pub/data/gsod/"
  url <- paste0(url_base, year, '/')
  # userpwd <- "yourUser:yourPass"
  filenames <- getURL(url,
                      # userpwd = userpwd,
                      ftp.use.epsv = FALSE,
                      dirlistonly = TRUE)
  filenames <- unlist(strsplit(filenames, '\n'))
  for (i in 1:length(filenames)){
    this_file <- filenames[i]
    new_name <- paste0(directory, '/', gsub('.gz', '', this_file, fixed = TRUE))
    if(!file.exists(new_name)){
      try({
        message('Retrieiving ', this_file, ' (', i, ' of ', length(filenames), ' for year ', year, ')')
        # Delete before downloading
        if(file.exists('temp.op.gz')){
          file.remove('temp.op.gz')
        }
        if(file.exists('temp.op')){
          file.remove('temp.op')
        }
        # Download
        download.file(url = paste0(url, this_file),
                      destfile = 'temp.op.gz')
        # Extract
        R.utils::gunzip('temp.op.gz')
        # Move
        file.copy(from = 'temp.op',
                  to = new_name)
      })
    }
  }
}

for(i in 2010:2017){
  get_data(year = i)
}

# 
# 
# Read in all data
raw_data <- dir('noaa_raw/')
data_list <- list()
for (i in 1:length(raw_data)){
  message(i, ' of ', length(raw_data))
  this_file <- raw_data[i]
  suppressMessages(this_table <- read_table(paste0('noaa_raw/', this_file)))
  this_table <-
    this_table %>%
    dplyr::select(`STN---`, YEARMODA, TEMP, DEWP, SLP, STP, VISIB, WDSP, MXSPD, GUST, MAX, MIN, PRCP, SNDP, FRSHTT)
  names(this_table)[1] <- 'USAF'
  this_table$USAF <- as.character(this_table$USAF)
  this_table <- data.frame(this_table)
  for(j in 1:ncol(this_table)){
    this_table[,j] <- as.character(this_table[,j])
  }
  data_list[[i]] <- this_table
}
a <- bind_rows(data_list)


# Join to station information
# Read in station info
b <- read_table('noaa_data/isd-history_may_2017.txt', skip = 20)
b <- b %>% dplyr::select(USAF, `STATION NAME`, CTRY, LAT, LON, `ELEV(M)`)
b <- b %>% rename(station_name = `STATION NAME`, country = CTRY, lat = LAT, lon = LON, elevation = `ELEV(M)`)
# b$USAF <- as.numeric(b$USAF)
b <- b %>%
  filter(!duplicated(USAF))

# Join
noaa <- left_join(a, b,
                  by = 'USAF')

# Make date column
noaa$date <- as.Date(paste0(substr(noaa$YEARMODA,start = 1, stop = 4),
                            '-',
                            substr(noaa$YEARMODA,start = 5, stop = 6),
                            '-',
                            substr(noaa$YEARMODA,start = 7, stop = 8)))

# Make lowercase column names
names(noaa) <- tolower(names(noaa))

# Keep only columns of interest
noaa <- 
  noaa %>%
  dplyr::select(country,
                station_name,
                usaf,
                date,
                temp,
                dewp,
                slp,
                stp,
                visib,
                wdsp,
                mxspd,
                gust,
                max,
                min,
                prcp,
                sndp,
                frshtt,
                lat,
                lon,
                elevation)

# Clean up NAs
noaa <- data.frame(noaa)
for (j in 5:ncol(noaa)){
  noaa[,j] <- ifelse(detect_noaa_na(noaa[,j]),
                     NA,
                     noaa[,j])
}

# Convert to number
convert_to_number <-
  function(x){
    x <- regmatches(x, gregexpr("[[:digit:]]+", x))
    if(length(unlist(x)) == 0){
      y <- NA
    } else {
      y <- lapply(x, function(z){
        if(length(z) == 2){
          out <- as.numeric(paste0(z[1], '.', z[2]))
        } else {
          out <- unlist(z)[1]
        }
        return(out)
      })
    }
    return(as.numeric(unlist(y)))
  }


# Clean up column types
noaa <-
  noaa %>%
  mutate(temp = convert_to_number(`temp`),
         max = convert_to_number(`max`),
         min = convert_to_number(`min`),
         prcp = convert_to_number(`prcp`),
         wdsp = convert_to_number(`wdsp`),
         visib = convert_to_number(`visib`),
         stp = convert_to_number(`stp`),
         sndp = convert_to_number(`sndp`),
         dewp = convert_to_number(`dewp`),
         slp = convert_to_number(`slp`),
         mxspd = convert_to_number(`mxspd`),
         gust = convert_to_number(gust))

# Since noaa has some missing days, interpolate
left <- expand.grid(station_name = sort(unique(noaa$station_name)),
                    date = sort(unique(noaa$date)))
noaa <- left_join(left,
                  noaa,
                  by = c('station_name', 'date'))
# Flag estimations
noaa$estimated <- ifelse(is.na(noaa$lat), TRUE, FALSE)
# Performance interpolation
x <-
  noaa %>%
  arrange(date) %>%
  group_by(station_name) %>%
  mutate(temp = zoo::na.approx(object = temp,
                               x = date,
                               na.rm = FALSE),
         dewp = zoo::na.approx(object = dewp,
                               x = date,
                               na.rm = FALSE),
         wdsp = zoo::na.approx(object = wdsp,
                               x = date,
                               na.rm = FALSE),
         mxspd = zoo::na.approx(object = mxspd,
                                x = date,
                                na.rm = FALSE),
         max = zoo::na.approx(object = max,
                              x = date,
                              na.rm = FALSE),
         min = zoo::na.approx(object = min,
                              x = date,
                              na.rm = FALSE),
         prcp = zoo::na.approx(object = prcp,
                               x = date,
                               na.rm = FALSE),
         visib = zoo::na.approx(object = visib,
                               x = date,
                               na.rm = FALSE),
         slp = zoo::na.approx(object = slp,
                               x = date,
                               na.rm = FALSE),
         stp = zoo::na.approx(object = stp,
                               x = date,
                               na.rm = FALSE),
         gust = zoo::na.approx(object = gust,
                               x = date,
                               na.rm = FALSE),
         sndp = zoo::na.approx(object = sndp,
                               x = date,
                               na.rm = FALSE),
         elevation = zoo::na.approx(object = elevation,
                               x = date,
                               na.rm = FALSE))

# Fix missing lat/lons
ll <- noaa %>%
  group_by(station_name) %>%
  summarise(lat = dplyr::first(lat[!is.na(lat)]),
            lon = dplyr::first(lon[!is.na(lon)]))

noaa <- x %>% ungroup %>%
  dplyr::select(-lat,
                -lon) %>%
  left_join(ll,
            by = 'station_name')

