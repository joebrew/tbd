# https://neo.sci.gsfc.nasa.gov/view.php?datasetId=MODAL2_M_CLD_FR&date=2017-10-01
library(tidyverse)

if('cloud_cover.RData' %in% dir('cloud_cover')){
  load('cloud_cover/cloud_cover.RData')
} else {
  
  # Define function for making url 
  make_url <- function(url_number = 2034923){
    paste0('https://neo.sci.gsfc.nasa.gov/servlet/RenderData?si=',
           url_number,
           '&cs=gs&format=JPEG&width=3600&height=1800')
  }
  
  # Manually enter the date numbers for the images we want (retrieved from the above website)
  df <- data_frame(year = 2017,
                   month = 1:12,
                   url_number = c(1712121,
                                  1713286,
                                  1714206,
                                  1715131,
                                  1715907,
                                  1716888,
                                  1722874,
                                  1738562,
                                  1740028, #9
                                  1740887,
                                  NA,
                                  NA)) %>%
    bind_rows(
      data_frame(year = 2016,
                 month = 1:12,
                 url_number = c(1699638,
                                1701462,
                                1701588,
                                1702911,
                                1704011,
                                1705081,
                                1706123,
                                1707262,
                                1708575,
                                1709456,
                                1710300,
                                1711547)) 
    ) %>%
    bind_rows(
      data_frame(year = 2015,
                 month = 1:12,
                 url_number = c(1618432,
                                1618435,
                                1618438,
                                1618441,
                                1618408,
                                1656298,
                                1690398,
                                1692039,
                                1694723,
                                1696013,
                                1697204,
                                1698490))
    ) %>%
    bind_rows(
      data_frame(year = 2014,
                 month = 1:12,
                 url_number = c(1618437,1618439,1618440,1618442,
                                1618443,1618411,1618414,1618417,
                                1618420,1618423,1618426,1618429))
    ) %>%
    bind_rows(
      data_frame(year = 2013,
                 month = 1:12,
                 url_number = c(1618419, 1618421, 1618422, 1618424, 1618425, 1618427, 1618428, 1618430, 1618431, 1618433, 1618434, 1618436))
    ) %>%
    bind_rows(
      data_frame(year = 2012,
                 month = 1:12,
                 url_number = c(1618396, 1618399, 1618402, 1618405, 1618372, 1618409, 1618410, 1618412, 1618413, 1618415, 1618416, 1618418))
    ) %>%
    bind_rows(
      data_frame(year = 2011,
                 month = 1:12,
                 url_number = c(1618218, 1618220, 1618221, 1618223, 1618224, 1618226, 1618227, 1618195, 1618198, 1618201, 1618204, 1618393))
    ) %>%
    bind_rows(
      data_frame(year = 2010,
                 month = 1:12,
                 url_number = c(1618200, 1618202, 1618203, 1618205, 1618206, 1618208, 1618209, 1618211, 1618212, 1618214, 1618215, 1618217))
    ) %>%
    bind_rows(
      data_frame(year = 2009,
                 month = 1:12,
                 url_number = c(1618174, 1618177, 1618180, 1618183, 1618186, 1618189, 1618156, 1618193, 1618194, 1618196, 1618197, 1618199))) %>%
    bind_rows(
      data_frame(year = 2008,
                 month = 1:12,
                 url_number = c(1617998, 1617999, 1618001, 1618002, 1618004, 1618005, 1618007, 1618008, 1618010, 1618011, 1617979, 1617982))) %>%
    bind_rows(
      data_frame(year = 2007,
                 month = 1:12,
                 url_number = c(1617980, 1617981, 1617983, 1617984, 1617986, 1617987, 1617989, 1617990, 1617992, 1617993, 1617995, 1617996))) %>%
    bind_rows(
      data_frame(year = 2006,
                 month = 1:12,
                 url_number = c(1617949, 1617952, 1617955, 1617958, 1617961, 1617964, 1617967, 1617970, 1617973, 1617940, 1617977, 1617978))) %>%
    bind_rows(
      data_frame(year = 2005,
                 month = 1:12,
                 url_number = c(1617802, 1617805, 1617808, 1617811, 1617814, 1617817, 1617820, 1617823, 1617826, 1617829, 1617796, 1617946))) %>%
    bind_rows(
      data_frame(year = 2004,
                 month = 1:12,
                 url_number = c(1617816, 1617818, 1617819, 1617821, 1617822, 1617824, 1617825, 1617827, 1617828, 1617830, 1617831, 1617799))) %>%
    bind_rows(
      data_frame(year = 2003,
                 month = 1:12,
                 url_number = c(1617658, 1617661, 1617664, 1617667, 1617670, 1617673, 1617676, 1617679, 1617682, 1617685, 1617652, 1617815))) %>%
    bind_rows(
      data_frame(year = 2002,
                 month = 1:12,
                 url_number = c(1617672, 1617674, 1617675, 1617677, 1617678, 1617680, 1617681, 1617683, 1617684, 1617686, 1617687, 1617655))) %>%
    bind_rows(
      data_frame(year = 2001,
                 month = 1:12,
                 url_number = c(1617558, 1617560, 1617561, 1617563, 1617564, 1617566, 1617567, 1617569, 1617570, 1617572, 1617573, 1617671))) %>%
    bind_rows(
      data_frame(year = 2000,
                 month = 2:12,
                 url_number = c(1617512, 1617523, 1617524, 1617546, 1617548, 1617549, 1617551, 1617552, 1617554, 1617555, 1617557))) %>%
    mutate(url = make_url(url_number)) %>%
    arrange(desc(year), desc(month)) %>%
    filter(!is.na(month), !is.na(url_number))
  
  # Download all files
  for (i in 1:nrow(df)){
    this_file <- df$url[i]
    save_file <- paste0(df$year[i], df$month[i], '.jpg')
    if(!save_file %in% dir('cloud_cover')){
      download.file(df$url[i], destfile = paste0('cloud_cover/', save_file))
    }
  }
  
  # Read in all files
  jpgs <- dir('cloud_cover')
  jpgs <- jpgs[grepl('.jpg', jpgs, fixed = TRUE)]
  
  out_list <- list()
  for (i in 1:length(jpgs)){
    message(i, ' of ', length(jpgs))
    this_jpg <- jpgs[i]
    this_year <- substr(this_jpg, 1, 4)
    this_month <- substr(this_jpg, 5, nchar(this_jpg))
    this_month <- gsub('.jpg', '', this_month, fixed=TRUE)
    this_raster <- raster(paste0('cloud_cover/', this_jpg))
    geo <- expand.grid(lon = (-180:180),
                       lat = -90:90)
    geo$x <- (geo$lon + 180) * 10
    geo$y <- (geo$lat + 90) * 10
    geo$value <- raster::extract(this_raster, geo[,c('x', 'y')])
    geo$year <- this_year
    geo$month <- this_month
    out_list[[i]] <- geo
  }
  cloud_cover <- bind_rows(out_list)
  
  save(cloud_cover, file = 'cloud_cover/cloud_cover.RData')
}
