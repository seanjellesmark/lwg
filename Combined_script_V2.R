# Combined final script for the analysis by Jellesmark et al 
# Chapter 2 "The effect of conservation actions on the abundance of breeding birds within reserves in the UK"
library(INLA)
library(INLAutils)
library(inlatools)
library(brinla)
library(patchwork)
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(ggpubr)
library(openxlsx)
library(tidyverse)
library(readxl)
library(splines)
library(splines2) # Not sure whether we will use this one but included in case we decide on use these splines and forget to load it in next session
library(mgcv) # Not really necessary.
library(data.table)
library(rgdal)
library(stringr)
library(gstat)
library(sf)
library(gt)
library(ggregplot)

# Not necessary atm # library(ggsn) # Using development version from: devtools::install_github('oswaldosantos/ggsn') 

# Part 1: Climate ----
# Climatic data script. Creates monthly climate data on 1x1 km grid level for the UK and summarises into winter and spring variables which are
# joined with counts and conservation data later in the script. "rain" is used to refer to some of the climatic variables. 
# This does not have any actual effect and the correct variable is extracted, but it is sloppy and I will fix it when
# I have finished this script

#### Monthly -------

# rainfall ----
monthly <- function(data_name, variable_name, climate_year, climate_unit){
  
  #set path and filename  
  
  ncpath <- "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/climate/rainfall/"
  
  ncname <- data_name
  
  ncfname <- paste (ncpath, ncname, ".nc", sep ="")
  
  dname <- variable_name
  
  # open a netCDF file
  
  ncin <- nc_open(ncfname)
  
  print(ncin)
  
  # Get coordinates and time variables
  
  lon <-ncvar_get(ncin, "projection_x_coordinate")
  nlon <-dim(lon)
  head(lon)
  
  lat <-ncvar_get(ncin, "projection_y_coordinate")
  nlat <-dim(lat)
  head(lat)
  
  print(c(nlon, nlat))
  
  # get time
  
  time <- ncvar_get(ncin, "time")
  time
  
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(time)
  nt
  
  tunits
  
  # get rainfall data
  
  rainfall_array <- ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(rainfall_array)
  
  # get global attributes
  
  title <- ncatt_get(ncin,0,"title")
  institution <- ncatt_get(ncin,0,"institution")
  datasource <- ncatt_get(ncin,0,"source")
  references <- ncatt_get(ncin,0,"references")
  history <- ncatt_get(ncin,0,"history")
  Conventions <- ncatt_get(ncin,0,"Conventions")
  
  # check workspace
  
  ls()
  
  # Reshaping from raster to rectangular
  
  # Convert the time variable
  
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  chron(time,origin=c(tmonth, tday, tyear))
  
  # reshape the array into vector
  rain_vec_long <- as.vector(rainfall_array)
  length(rain_vec_long)
  
  # reshape the vector into a matrix
  rain_mat <- matrix(rain_vec_long, nrow=nlon*nlat, ncol=nt)
  dim(rain_mat)
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  rain_df02 <- data.frame(cbind(lonlat,rain_mat))
  names(rain_df02) <- c("lon","lat","january","february","march","april","may", "june", "july", "august", "september", "october", "november", "december")
  # options(width=96)
  head(na.omit(rain_df02, 20))
  
  # Correct for sea values - not sure whether this is necessary - Update: It's not necessary unless I want to plot the values spatially. 
  # We therefore exclude this part and exchange with a single filter to get rid of the large values
  
  #  rain_df03<-rain_df02 %>% 
  #    mutate(january_cor = if_else(january > 3000, 0, january),
  #           february_cor = if_else(february > 3000, 0, february),
  #           march_cor = if_else(march > 3000, 0, march),
  #           april_cor = if_else(april > 3000, 0, april),
  #           may_cor = if_else(may > 3000, 0, may),
  #           june_cor = if_else(june > 3000, 0, june),
  #           july_cor = if_else(july > 3000, 0, july),
  #           august_cor = if_else(august > 3000, 0, august),
  #           september_cor = if_else(september > 3000, 0, september),
  #           october_cor = if_else(october > 3000, 0, october),
  #           november_cor = if_else(november > 3000, 0, november),
  #           december_cor = if_else(december > 3000, 0, december),
  #           year = climate_year,
  #           unit = climate_unit )
  
  rain_df03 <- rain_df02 %>% 
    filter(january < 5000) %>% 
    mutate(year = climate_year,
           unit = climate_unit)
  # Add variable type to the month so that for example january becomes january_rain. variable name has to be a part of month columns as we
  # end up with multiple climatic variables in each year*month combination
  
  rain_df04 <- rain_df03%>% rename_at(vars(january:december) ,function(x){paste0("rain_", x)})
}
precip_2018<-monthly(data_name = "rainfall18", variable_name = "rainfall", climate_year = 2018, climate_unit = "monthly_rainfall")
precip_2017<-monthly(data_name = "rainfall17", variable_name = "rainfall", climate_year = 2017, climate_unit = "monthly_rainfall")
precip_2016<-monthly(data_name = "rainfall16", variable_name = "rainfall", climate_year = 2016, climate_unit = "monthly_rainfall")
precip_2015<-monthly(data_name = "rainfall15", variable_name = "rainfall", climate_year = 2015, climate_unit = "monthly_rainfall")
precip_2014<-monthly(data_name = "rainfall14", variable_name = "rainfall", climate_year = 2014, climate_unit = "monthly_rainfall")
precip_2013<-monthly(data_name = "rainfall13", variable_name = "rainfall", climate_year = 2013, climate_unit = "monthly_rainfall")
precip_2012<-monthly(data_name = "rainfall12", variable_name = "rainfall", climate_year = 2012, climate_unit = "monthly_rainfall")
precip_2011<-monthly(data_name = "rainfall11", variable_name = "rainfall", climate_year = 2011, climate_unit = "monthly_rainfall")
precip_2010<-monthly(data_name = "rainfall10", variable_name = "rainfall", climate_year = 2010, climate_unit = "monthly_rainfall")
precip_2009<-monthly(data_name = "rainfall09", variable_name = "rainfall", climate_year = 2009, climate_unit = "monthly_rainfall")
precip_2008<-monthly(data_name = "rainfall08", variable_name = "rainfall", climate_year = 2008, climate_unit = "monthly_rainfall")
precip_2007<-monthly(data_name = "rainfall07", variable_name = "rainfall", climate_year = 2007, climate_unit = "monthly_rainfall")
precip_2006<-monthly(data_name = "rainfall06", variable_name = "rainfall", climate_year = 2006, climate_unit = "monthly_rainfall")
precip_2005<-monthly(data_name = "rainfall05", variable_name = "rainfall", climate_year = 2005, climate_unit = "monthly_rainfall")
precip_2004<-monthly(data_name = "rainfall04", variable_name = "rainfall", climate_year = 2004, climate_unit = "monthly_rainfall")
precip_2003<-monthly(data_name = "rainfall03", variable_name = "rainfall", climate_year = 2003, climate_unit = "monthly_rainfall")
precip_2002<-monthly(data_name = "rainfall02", variable_name = "rainfall", climate_year = 2002, climate_unit = "monthly_rainfall")
precip_2001<-monthly(data_name = "rainfall01", variable_name = "rainfall", climate_year = 2001, climate_unit = "monthly_rainfall")
precip_2000<-monthly(data_name = "rainfall00", variable_name = "rainfall", climate_year = 2000, climate_unit = "monthly_rainfall")
precip_1999<-monthly(data_name = "rainfall99", variable_name = "rainfall", climate_year = 1999, climate_unit = "monthly_rainfall")
precip_1998<-monthly(data_name = "rainfall98", variable_name = "rainfall", climate_year = 1998, climate_unit = "monthly_rainfall")
precip_1997<-monthly(data_name = "rainfall97", variable_name = "rainfall", climate_year = 1997, climate_unit = "monthly_rainfall")
precip_1996<-monthly(data_name = "rainfall96", variable_name = "rainfall", climate_year = 1996, climate_unit = "monthly_rainfall")
precip_1995<-monthly(data_name = "rainfall95", variable_name = "rainfall", climate_year = 1995, climate_unit = "monthly_rainfall")
precip_1994<-monthly(data_name = "rainfall94", variable_name = "rainfall", climate_year = 1994, climate_unit = "monthly_rainfall")
precip_1993<-monthly(data_name = "rainfall93", variable_name = "rainfall", climate_year = 1993, climate_unit = "monthly_rainfall")
precip_1992<-monthly(data_name = "rainfall92", variable_name = "rainfall", climate_year = 1992, climate_unit = "monthly_rainfall")

monthly_precip<-bind_rows(precip_1992, precip_1993, precip_1994, precip_1995, precip_1996, precip_1997, precip_1998, precip_1999, precip_2000, precip_2001, 
                          precip_2002,precip_2003, precip_2004, precip_2005, precip_2006, precip_2007, precip_2008, precip_2009, precip_2010, 
                          precip_2011, precip_2012, precip_2013, precip_2014, precip_2015, precip_2016, precip_2017, precip_2018)

monthly_precip <- monthly_precip %>% 
  rename(easting_clim = lon,
         northing_clim = lat)



# Mean temperature ---- 

# The same function is used but with minor changes
monthly <- function(data_name, variable_name, climate_year, climate_unit){
  
  #set path and filename  
  
  ncpath <- "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/climate/tasmean/"
  
  ncname <- data_name
  
  ncfname <- paste (ncpath, ncname, ".nc", sep ="")
  
  dname <- variable_name
  
  # open a netCDF file
  
  ncin <- nc_open(ncfname)
  
  print(ncin)
  
  # Get coordinates and time variables
  
  lon <-ncvar_get(ncin, "projection_x_coordinate")
  nlon <-dim(lon)
  head(lon)
  
  lat <-ncvar_get(ncin, "projection_y_coordinate")
  nlat <-dim(lat)
  head(lat)
  
  print(c(nlon, nlat))
  
  # get time
  
  time <- ncvar_get(ncin, "time")
  time
  
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(time)
  nt
  
  tunits
  
  # get temp data
  
  temp_array <- ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(temp_array)
  
  # get global attributes
  
  title <- ncatt_get(ncin,0,"title")
  institution <- ncatt_get(ncin,0,"institution")
  datasource <- ncatt_get(ncin,0,"source")
  references <- ncatt_get(ncin,0,"references")
  history <- ncatt_get(ncin,0,"history")
  Conventions <- ncatt_get(ncin,0,"Conventions")
  
  # check workspace
  
  ls()
  
  # Reshaping from raster to rectangular
  
  # Convert the time variable
  
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  chron(time,origin=c(tmonth, tday, tyear))
  
  
  
  # reshape the array into vector
  temp_vec_long <- as.vector(temp_array)
  length(temp_vec_long)
  
  # reshape the vector into a matrix
  temp_mat <- matrix(temp_vec_long, nrow=nlon*nlat, ncol=nt)
  dim(temp_mat)
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  temp_df02 <- data.frame(cbind(lonlat,temp_mat))
  names(temp_df02) <- c("lon","lat","january","february","march","april","may", "june", "july", "august", "september", "october", "november", "december")
  # options(width=96)
  head(na.omit(temp_df02, 20))
  
  # Correct for sea values - not sure whether this is necessary - Update: It's not necessary unless I want to plot the values spatially. 
  # We therefore exclude this part and exchange with a single filter to get rid of the large values
  
  #  rain_df03<-rain_df02 %>% 
  #    mutate(january_cor = if_else(january > 3000, 0, january),
  #           february_cor = if_else(february > 3000, 0, february),
  #           march_cor = if_else(march > 3000, 0, march),
  #           april_cor = if_else(april > 3000, 0, april),
  #           may_cor = if_else(may > 3000, 0, may),
  #           june_cor = if_else(june > 3000, 0, june),
  #           july_cor = if_else(july > 3000, 0, july),
  #           august_cor = if_else(august > 3000, 0, august),
  #           september_cor = if_else(september > 3000, 0, september),
  #           october_cor = if_else(october > 3000, 0, october),
  #           november_cor = if_else(november > 3000, 0, november),
  #           december_cor = if_else(december > 3000, 0, december),
  #           year = climate_year,
  #           unit = climate_unit )
  
  temp_df03 <- temp_df02 %>% 
    filter(january < 5000)%>% 
    mutate(year = climate_year,
           unit = climate_unit)
  
  # Add variable type to the month so that for example january becomes january_rain. variable name has to be a part of month columns as we
  # end up with multiple climatic variables in each year*month combination
  
  temp_df04 <- temp_df03%>% rename_at(vars(january:december) ,function(x){paste0("temp_", x)})
}
temp_2018<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201801-201812", variable_name = "tas", climate_year = 2018, climate_unit = "mean_temp")
temp_2017<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201701-201712", variable_name = "tas", climate_year = 2017, climate_unit = "mean_temp")
temp_2016<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201601-201612", variable_name = "tas", climate_year = 2016, climate_unit = "mean_temp")
temp_2015<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201501-201512", variable_name = "tas", climate_year = 2015, climate_unit = "mean_temp")
temp_2014<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201401-201412", variable_name = "tas", climate_year = 2014, climate_unit = "mean_temp")
temp_2013<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201301-201312", variable_name = "tas", climate_year = 2013, climate_unit = "mean_temp")
temp_2012<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201201-201212", variable_name = "tas", climate_year = 2012, climate_unit = "mean_temp")
temp_2011<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201101-201112", variable_name = "tas", climate_year = 2011, climate_unit = "mean_temp")
temp_2010<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_201001-201012", variable_name = "tas", climate_year = 2010, climate_unit = "mean_temp")
temp_2009<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200901-200912", variable_name = "tas", climate_year = 2009, climate_unit = "mean_temp")
temp_2008<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200801-200812", variable_name = "tas", climate_year = 2008, climate_unit = "mean_temp")
temp_2007<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200701-200712", variable_name = "tas", climate_year = 2007, climate_unit = "mean_temp")
temp_2006<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200601-200612", variable_name = "tas", climate_year = 2006, climate_unit = "mean_temp")
temp_2005<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200501-200512", variable_name = "tas", climate_year = 2005, climate_unit = "mean_temp")
temp_2004<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200401-200412", variable_name = "tas", climate_year = 2004, climate_unit = "mean_temp")
temp_2003<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200301-200312", variable_name = "tas", climate_year = 2003, climate_unit = "mean_temp")
temp_2002<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200201-200212", variable_name = "tas", climate_year = 2002, climate_unit = "mean_temp")
temp_2001<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200101-200112", variable_name = "tas", climate_year = 2001, climate_unit = "mean_temp")
temp_2000<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_200001-200012", variable_name = "tas", climate_year = 2000, climate_unit = "mean_temp")
temp_1999<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_199901-199912", variable_name = "tas", climate_year = 1999, climate_unit = "mean_temp")
temp_1998<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_199801-199812", variable_name = "tas", climate_year = 1998, climate_unit = "mean_temp")
temp_1997<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_199701-199712", variable_name = "tas", climate_year = 1997, climate_unit = "mean_temp")
temp_1996<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_199601-199612", variable_name = "tas", climate_year = 1996, climate_unit = "mean_temp")
temp_1995<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_199501-199512", variable_name = "tas", climate_year = 1995, climate_unit = "mean_temp")
temp_1994<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_199401-199412", variable_name = "tas", climate_year = 1994, climate_unit = "mean_temp")
temp_1993<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_199301-199312", variable_name = "tas", climate_year = 1993, climate_unit = "mean_temp")
temp_1992<-monthly(data_name = "tas_hadukgrid_uk_1km_mon_199201-199212", variable_name = "tas", climate_year = 1992, climate_unit = "mean_temp")

monthly_temp<-bind_rows(temp_1992 ,temp_1993, temp_1994, temp_1995, temp_1996, temp_1997, temp_1998, temp_1999, temp_2000, temp_2001, 
                        temp_2002,temp_2003, temp_2004, temp_2005, temp_2006, temp_2007, temp_2008, temp_2009, temp_2010, 
                        temp_2011, temp_2012, temp_2013, temp_2014, temp_2015, temp_2016, temp_2017, temp_2018)

monthly_temp <- monthly_temp %>% 
  rename(easting_clim = lon,
         northing_clim = lat)

## Monthly max temperature ----

monthly <- function(data_name, variable_name, climate_year, climate_unit){
  
  #set path and filename  
  
  ncpath <- "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/climate/tasmax/"
  
  ncname <- data_name
  
  ncfname <- paste (ncpath, ncname, ".nc", sep ="")
  
  dname <- variable_name
  
  # open a netCDF file
  
  ncin <- nc_open(ncfname)
  
  print(ncin)
  
  # Get coordinates and time variables
  
  lon <-ncvar_get(ncin, "projection_x_coordinate")
  nlon <-dim(lon)
  head(lon)
  
  lat <-ncvar_get(ncin, "projection_y_coordinate")
  nlat <-dim(lat)
  head(lat)
  
  print(c(nlon, nlat))
  
  # get time
  
  time <- ncvar_get(ncin, "time")
  time
  
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(time)
  nt
  
  tunits
  
  # get rainfall data
  
  rainfall_array <- ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(rainfall_array)
  
  # get global attributes
  
  title <- ncatt_get(ncin,0,"title")
  institution <- ncatt_get(ncin,0,"institution")
  datasource <- ncatt_get(ncin,0,"source")
  references <- ncatt_get(ncin,0,"references")
  history <- ncatt_get(ncin,0,"history")
  Conventions <- ncatt_get(ncin,0,"Conventions")
  
  # check workspace
  
  ls()
  
  # Reshaping from raster to rectangular
  
  # Convert the time variable
  
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  chron(time,origin=c(tmonth, tday, tyear))
  
  
  
  # reshape the array into vector
  rain_vec_long <- as.vector(rainfall_array)
  length(rain_vec_long)
  
  # reshape the vector into a matrix
  rain_mat <- matrix(rain_vec_long, nrow=nlon*nlat, ncol=nt)
  dim(rain_mat)
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  rain_df02 <- data.frame(cbind(lonlat,rain_mat))
  names(rain_df02) <- c("lon","lat","january","february","march","april","may", "june", "july", "august", "september", "october", "november", "december")
  # options(width=96)
  head(na.omit(rain_df02, 20))
  
  # Correct for sea values - not sure whether this is necessary - Update: It's not necessary unless I want to plot the values spatially. 
  # We therefore exclude this part and exchange with a single filter to get rid of the large values
  
  #  rain_df03<-rain_df02 %>% 
  #    mutate(january_cor = if_else(january > 3000, 0, january),
  #           february_cor = if_else(february > 3000, 0, february),
  #           march_cor = if_else(march > 3000, 0, march),
  #           april_cor = if_else(april > 3000, 0, april),
  #           may_cor = if_else(may > 3000, 0, may),
  #           june_cor = if_else(june > 3000, 0, june),
  #           july_cor = if_else(july > 3000, 0, july),
  #           august_cor = if_else(august > 3000, 0, august),
  #           september_cor = if_else(september > 3000, 0, september),
  #           october_cor = if_else(october > 3000, 0, october),
  #           november_cor = if_else(november > 3000, 0, november),
  #           december_cor = if_else(december > 3000, 0, december),
  #           year = climate_year,
  #           unit = climate_unit )
  
  rain_df03 <- rain_df02 %>% 
    filter(january < 5000)%>% 
    mutate(year = climate_year,
           unit = climate_unit)
  
  # Add variable type to the month so that for example january becomes january_rain. variable name has to be a part of month columns as we
  # end up with multiple climatic variables in each year*month combination
  
  rain_df04 <- rain_df03%>% rename_at(vars(january:december) ,function(x){paste0("maxtemp_", x)})
}
maxtemp_2018<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201801-201812", variable_name = "tasmax", climate_year = 2018, climate_unit = "max_temp")
maxtemp_2017<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201701-201712", variable_name = "tasmax", climate_year = 2017, climate_unit = "max_temp")
maxtemp_2016<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201601-201612", variable_name = "tasmax", climate_year = 2016, climate_unit = "max_temp")
maxtemp_2015<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201501-201512", variable_name = "tasmax", climate_year = 2015, climate_unit = "max_temp")
maxtemp_2014<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201401-201412", variable_name = "tasmax", climate_year = 2014, climate_unit = "max_temp")
maxtemp_2013<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201301-201312", variable_name = "tasmax", climate_year = 2013, climate_unit = "max_temp")
maxtemp_2012<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201201-201212", variable_name = "tasmax", climate_year = 2012, climate_unit = "max_temp")
maxtemp_2011<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201101-201112", variable_name = "tasmax", climate_year = 2011, climate_unit = "max_temp")
maxtemp_2010<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_201001-201012", variable_name = "tasmax", climate_year = 2010, climate_unit = "max_temp")
maxtemp_2009<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200901-200912", variable_name = "tasmax", climate_year = 2009, climate_unit = "max_temp")
maxtemp_2008<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200801-200812", variable_name = "tasmax", climate_year = 2008, climate_unit = "max_temp")
maxtemp_2007<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200701-200712", variable_name = "tasmax", climate_year = 2007, climate_unit = "max_temp")
maxtemp_2006<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200601-200612", variable_name = "tasmax", climate_year = 2006, climate_unit = "max_temp")
maxtemp_2005<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200501-200512", variable_name = "tasmax", climate_year = 2005, climate_unit = "max_temp")
maxtemp_2004<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200401-200412", variable_name = "tasmax", climate_year = 2004, climate_unit = "max_temp")
maxtemp_2003<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200301-200312", variable_name = "tasmax", climate_year = 2003, climate_unit = "max_temp")
maxtemp_2002<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200201-200212", variable_name = "tasmax", climate_year = 2002, climate_unit = "max_temp")
maxtemp_2001<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200101-200112", variable_name = "tasmax", climate_year = 2001, climate_unit = "max_temp")
maxtemp_2000<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_200001-200012", variable_name = "tasmax", climate_year = 2000, climate_unit = "max_temp")
maxtemp_1999<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_199901-199912", variable_name = "tasmax", climate_year = 1999, climate_unit = "max_temp")
maxtemp_1998<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_199801-199812", variable_name = "tasmax", climate_year = 1998, climate_unit = "max_temp")
maxtemp_1997<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_199701-199712", variable_name = "tasmax", climate_year = 1997, climate_unit = "max_temp")
maxtemp_1996<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_199601-199612", variable_name = "tasmax", climate_year = 1996, climate_unit = "max_temp")
maxtemp_1995<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_199501-199512", variable_name = "tasmax", climate_year = 1995, climate_unit = "max_temp")
maxtemp_1994<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_199401-199412", variable_name = "tasmax", climate_year = 1994, climate_unit = "max_temp")
maxtemp_1993<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_199301-199312", variable_name = "tasmax", climate_year = 1993, climate_unit = "max_temp")
maxtemp_1992<-monthly(data_name = "tasmax_hadukgrid_uk_1km_mon_199201-199212", variable_name = "tasmax", climate_year = 1992, climate_unit = "max_temp")

monthly_maxtemp<-bind_rows(maxtemp_1992 ,maxtemp_1993, maxtemp_1994, maxtemp_1995, maxtemp_1996, maxtemp_1997, maxtemp_1998, maxtemp_1999, maxtemp_2000, maxtemp_2001, 
                           maxtemp_2002,maxtemp_2003, maxtemp_2004, maxtemp_2005, maxtemp_2006, maxtemp_2007, maxtemp_2008, maxtemp_2009, maxtemp_2010, 
                           maxtemp_2011, maxtemp_2012, maxtemp_2013, maxtemp_2014, maxtemp_2015, maxtemp_2016, maxtemp_2017, maxtemp_2018)

monthly_maxtemp <- monthly_maxtemp %>% 
  rename(easting_clim = lon,
         northing_clim = lat)
## Monthly min temperature ----


monthly <- function(data_name, variable_name, climate_year, climate_unit){
  
  #set path and filename  
  
  ncpath <- "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/climate/tasmin/"
  
  ncname <- data_name
  
  ncfname <- paste (ncpath, ncname, ".nc", sep ="")
  
  dname <- variable_name
  
  # open a netCDF file
  
  ncin <- nc_open(ncfname)
  
  print(ncin)
  
  # Get coordinates and time variables
  
  lon <-ncvar_get(ncin, "projection_x_coordinate")
  nlon <-dim(lon)
  head(lon)
  
  lat <-ncvar_get(ncin, "projection_y_coordinate")
  nlat <-dim(lat)
  head(lat)
  
  print(c(nlon, nlat))
  
  # get time
  
  time <- ncvar_get(ncin, "time")
  time
  
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(time)
  nt
  
  tunits
  
  # get rainfall data
  
  rainfall_array <- ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(rainfall_array)
  
  # get global attributes
  
  title <- ncatt_get(ncin,0,"title")
  institution <- ncatt_get(ncin,0,"institution")
  datasource <- ncatt_get(ncin,0,"source")
  references <- ncatt_get(ncin,0,"references")
  history <- ncatt_get(ncin,0,"history")
  Conventions <- ncatt_get(ncin,0,"Conventions")
  
  # check workspace
  
  ls()
  
  # Reshaping from raster to rectangular
  
  # Convert the time variable
  
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  chron(time,origin=c(tmonth, tday, tyear))
  
  
  
  # reshape the array into vector
  rain_vec_long <- as.vector(rainfall_array)
  length(rain_vec_long)
  
  # reshape the vector into a matrix
  rain_mat <- matrix(rain_vec_long, nrow=nlon*nlat, ncol=nt)
  dim(rain_mat)
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  rain_df02 <- data.frame(cbind(lonlat,rain_mat))
  names(rain_df02) <- c("lon","lat","january","february","march","april","may", "june", "july", "august", "september", "october", "november", "december")
  # options(width=96)
  head(na.omit(rain_df02, 20))
  
  # Correct for sea values - not sure whether this is necessary - Update: It's not necessary unless I want to plot the values spatially. 
  # We therefore exclude this part and exchange with a single filter to get rid of the large values
  
  #  rain_df03<-rain_df02 %>% 
  #    mutate(january_cor = if_else(january > 3000, 0, january),
  #           february_cor = if_else(february > 3000, 0, february),
  #           march_cor = if_else(march > 3000, 0, march),
  #           april_cor = if_else(april > 3000, 0, april),
  #           may_cor = if_else(may > 3000, 0, may),
  #           june_cor = if_else(june > 3000, 0, june),
  #           july_cor = if_else(july > 3000, 0, july),
  #           august_cor = if_else(august > 3000, 0, august),
  #           september_cor = if_else(september > 3000, 0, september),
  #           october_cor = if_else(october > 3000, 0, october),
  #           november_cor = if_else(november > 3000, 0, november),
  #           december_cor = if_else(december > 3000, 0, december),
  #           year = climate_year,
  #           unit = climate_unit )
  
  rain_df03 <- rain_df02 %>% 
    filter(january < 5000) %>% 
    mutate(year = climate_year,
           unit = climate_unit)
  
  # Add variable type to the month so that for example january becomes january_rain. variable name has to be a part of month columns as we
  # end up with multiple climatic variables in each year*month combination
  
  rain_df04 <- rain_df03%>% rename_at(vars(january:december) ,function(x){paste0("mintemp_", x)})
}
mintemp_2018<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201801-201812", variable_name = "tasmin", climate_year = 2018, climate_unit = "min_temp")
mintemp_2017<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201701-201712", variable_name = "tasmin", climate_year = 2017, climate_unit = "min_temp")
mintemp_2016<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201601-201612", variable_name = "tasmin", climate_year = 2016, climate_unit = "min_temp")
mintemp_2015<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201501-201512", variable_name = "tasmin", climate_year = 2015, climate_unit = "min_temp")
mintemp_2014<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201401-201412", variable_name = "tasmin", climate_year = 2014, climate_unit = "min_temp")
mintemp_2013<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201301-201312", variable_name = "tasmin", climate_year = 2013, climate_unit = "min_temp")
mintemp_2012<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201201-201212", variable_name = "tasmin", climate_year = 2012, climate_unit = "min_temp")
mintemp_2011<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201101-201112", variable_name = "tasmin", climate_year = 2011, climate_unit = "min_temp")
mintemp_2010<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_201001-201012", variable_name = "tasmin", climate_year = 2010, climate_unit = "min_temp")
mintemp_2009<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200901-200912", variable_name = "tasmin", climate_year = 2009, climate_unit = "min_temp")
mintemp_2008<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200801-200812", variable_name = "tasmin", climate_year = 2008, climate_unit = "min_temp")
mintemp_2007<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200701-200712", variable_name = "tasmin", climate_year = 2007, climate_unit = "min_temp")
mintemp_2006<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200601-200612", variable_name = "tasmin", climate_year = 2006, climate_unit = "min_temp")
mintemp_2005<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200501-200512", variable_name = "tasmin", climate_year = 2005, climate_unit = "min_temp")
mintemp_2004<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200401-200412", variable_name = "tasmin", climate_year = 2004, climate_unit = "min_temp")
mintemp_2003<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200301-200312", variable_name = "tasmin", climate_year = 2003, climate_unit = "min_temp")
mintemp_2002<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200201-200212", variable_name = "tasmin", climate_year = 2002, climate_unit = "min_temp")
mintemp_2001<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200101-200112", variable_name = "tasmin", climate_year = 2001, climate_unit = "min_temp")
mintemp_2000<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_200001-200012", variable_name = "tasmin", climate_year = 2000, climate_unit = "min_temp")
mintemp_1999<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_199901-199912", variable_name = "tasmin", climate_year = 1999, climate_unit = "min_temp")
mintemp_1998<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_199801-199812", variable_name = "tasmin", climate_year = 1998, climate_unit = "min_temp")
mintemp_1997<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_199701-199712", variable_name = "tasmin", climate_year = 1997, climate_unit = "min_temp")
mintemp_1996<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_199601-199612", variable_name = "tasmin", climate_year = 1996, climate_unit = "min_temp")
mintemp_1995<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_199501-199512", variable_name = "tasmin", climate_year = 1995, climate_unit = "min_temp")
mintemp_1994<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_199401-199412", variable_name = "tasmin", climate_year = 1994, climate_unit = "min_temp")
mintemp_1993<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_199301-199312", variable_name = "tasmin", climate_year = 1993, climate_unit = "min_temp")
mintemp_1992<-monthly(data_name = "tasmin_hadukgrid_uk_1km_mon_199201-199212", variable_name = "tasmin", climate_year = 1992, climate_unit = "min_temp")

monthly_mintemp<-bind_rows(mintemp_1992 ,mintemp_1993, mintemp_1994, mintemp_1995, mintemp_1996, mintemp_1997, mintemp_1998, mintemp_1999, mintemp_2000, mintemp_2001, 
                           mintemp_2002,mintemp_2003, mintemp_2004, mintemp_2005, mintemp_2006, mintemp_2007, mintemp_2008, mintemp_2009, mintemp_2010, 
                           mintemp_2011, mintemp_2012, mintemp_2013, mintemp_2014, mintemp_2015, mintemp_2016, mintemp_2017, mintemp_2018)

monthly_mintemp <- monthly_mintemp %>% 
  rename(easting_clim = lon,
         northing_clim = lat)

#  Monthly number of days with groundfrost----

monthly <- function(data_name, variable_name, climate_year, climate_unit){
  
  #set path and filename  
  
  ncpath <- "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/climate/groundfrost/"
  
  ncname <- data_name
  
  ncfname <- paste (ncpath, ncname, ".nc", sep ="")
  
  dname <- variable_name
  
  # open a netCDF file
  
  ncin <- nc_open(ncfname)
  
  print(ncin)
  
  # Get coordinates and time variables
  
  lon <-ncvar_get(ncin, "projection_x_coordinate")
  nlon <-dim(lon)
  head(lon)
  
  lat <-ncvar_get(ncin, "projection_y_coordinate")
  nlat <-dim(lat)
  head(lat)
  
  print(c(nlon, nlat))
  
  # get time
  
  time <- ncvar_get(ncin, "time")
  time
  
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(time)
  nt
  
  tunits
  
  # get rainfall data
  
  rainfall_array <- ncvar_get(ncin,dname)
  dlname <- ncatt_get(ncin,dname,"long_name")
  dunits <- ncatt_get(ncin,dname,"units")
  fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  dim(rainfall_array)
  
  # get global attributes
  
  title <- ncatt_get(ncin,0,"title")
  institution <- ncatt_get(ncin,0,"institution")
  datasource <- ncatt_get(ncin,0,"source")
  references <- ncatt_get(ncin,0,"references")
  history <- ncatt_get(ncin,0,"history")
  Conventions <- ncatt_get(ncin,0,"Conventions")
  
  # check workspace
  
  ls()
  
  # Reshaping from raster to rectangular
  
  # Convert the time variable
  
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  chron(time,origin=c(tmonth, tday, tyear))
  
  
  
  # reshape the array into vector
  rain_vec_long <- as.vector(rainfall_array)
  length(rain_vec_long)
  
  # reshape the vector into a matrix
  rain_mat <- matrix(rain_vec_long, nrow=nlon*nlat, ncol=nt)
  dim(rain_mat)
  
  # create a dataframe
  lonlat <- as.matrix(expand.grid(lon,lat))
  rain_df02 <- data.frame(cbind(lonlat,rain_mat))
  names(rain_df02) <- c("lon","lat","january","february","march","april","may", "june", "july", "august", "september", "october", "november", "december")
  # options(width=96)
  head(na.omit(rain_df02, 20))
  
  # Correct for sea values - not sure whether this is necessary - Update: It's not necessary unless I want to plot the values spatially. 
  # We therefore exclude this part and exchange with a single filter to get rid of the large values
  
  #  rain_df03<-rain_df02 %>% 
  #    mutate(january_cor = if_else(january > 3000, 0, january),
  #           february_cor = if_else(february > 3000, 0, february),
  #           march_cor = if_else(march > 3000, 0, march),
  #           april_cor = if_else(april > 3000, 0, april),
  #           may_cor = if_else(may > 3000, 0, may),
  #           june_cor = if_else(june > 3000, 0, june),
  #           july_cor = if_else(july > 3000, 0, july),
  #           august_cor = if_else(august > 3000, 0, august),
  #           september_cor = if_else(september > 3000, 0, september),
  #           october_cor = if_else(october > 3000, 0, october),
  #           november_cor = if_else(november > 3000, 0, november),
  #           december_cor = if_else(december > 3000, 0, december),
  #           year = climate_year,
  #           unit = climate_unit )
  
  rain_df03 <- rain_df02 %>% 
    filter(january < 5000) %>% 
    mutate(year = climate_year,
           unit = climate_unit)
  
  # Add variable type to the month so that for example january becomes january_rain. variable name has to be a part of month columns as we
  # end up with multiple climatic variables in each year*month combination
  
  rain_df04 <- rain_df03%>% rename_at(vars(january:december) ,function(x){paste0("groundfrost_", x)})
}
groundfrost_2018<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201801-201812", variable_name = "groundfrost", climate_year = 2018, climate_unit = "groundfrost")
groundfrost_2017<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201701-201712", variable_name = "groundfrost", climate_year = 2017, climate_unit = "groundfrost")
groundfrost_2016<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201601-201612", variable_name = "groundfrost", climate_year = 2016, climate_unit = "groundfrost")
groundfrost_2015<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201501-201512", variable_name = "groundfrost", climate_year = 2015, climate_unit = "groundfrost")
groundfrost_2014<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201401-201412", variable_name = "groundfrost", climate_year = 2014, climate_unit = "groundfrost")
groundfrost_2013<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201301-201312", variable_name = "groundfrost", climate_year = 2013, climate_unit = "groundfrost")
groundfrost_2012<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201201-201212", variable_name = "groundfrost", climate_year = 2012, climate_unit = "groundfrost")
groundfrost_2011<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201101-201112", variable_name = "groundfrost", climate_year = 2011, climate_unit = "groundfrost")
groundfrost_2010<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_201001-201012", variable_name = "groundfrost", climate_year = 2010, climate_unit = "groundfrost")
groundfrost_2009<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200901-200912", variable_name = "groundfrost", climate_year = 2009, climate_unit = "groundfrost")
groundfrost_2008<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200801-200812", variable_name = "groundfrost", climate_year = 2008, climate_unit = "groundfrost")
groundfrost_2007<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200701-200712", variable_name = "groundfrost", climate_year = 2007, climate_unit = "groundfrost")
groundfrost_2006<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200601-200612", variable_name = "groundfrost", climate_year = 2006, climate_unit = "groundfrost")
groundfrost_2005<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200501-200512", variable_name = "groundfrost", climate_year = 2005, climate_unit = "groundfrost")
groundfrost_2004<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200401-200412", variable_name = "groundfrost", climate_year = 2004, climate_unit = "groundfrost")
groundfrost_2003<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200301-200312", variable_name = "groundfrost", climate_year = 2003, climate_unit = "groundfrost")
groundfrost_2002<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200201-200212", variable_name = "groundfrost", climate_year = 2002, climate_unit = "groundfrost")
groundfrost_2001<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200101-200112", variable_name = "groundfrost", climate_year = 2001, climate_unit = "groundfrost")
groundfrost_2000<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_200001-200012", variable_name = "groundfrost", climate_year = 2000, climate_unit = "groundfrost")
groundfrost_1999<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_199901-199912", variable_name = "groundfrost", climate_year = 1999, climate_unit = "groundfrost")
groundfrost_1998<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_199801-199812", variable_name = "groundfrost", climate_year = 1998, climate_unit = "groundfrost")
groundfrost_1997<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_199701-199712", variable_name = "groundfrost", climate_year = 1997, climate_unit = "groundfrost")
groundfrost_1996<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_199601-199612", variable_name = "groundfrost", climate_year = 1996, climate_unit = "groundfrost")
groundfrost_1995<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_199501-199512", variable_name = "groundfrost", climate_year = 1995, climate_unit = "groundfrost")
groundfrost_1994<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_199401-199412", variable_name = "groundfrost", climate_year = 1994, climate_unit = "groundfrost")
groundfrost_1993<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_199301-199312", variable_name = "groundfrost", climate_year = 1993, climate_unit = "groundfrost")
groundfrost_1992<-monthly(data_name = "groundfrost_hadukgrid_uk_1km_mon_199201-199212", variable_name = "groundfrost", climate_year = 1992, climate_unit = "groundfrost")

monthly_groundfrost<-bind_rows(groundfrost_1992 ,groundfrost_1993, groundfrost_1994, groundfrost_1995, groundfrost_1996, groundfrost_1997, groundfrost_1998, groundfrost_1999, groundfrost_2000, groundfrost_2001, 
                               groundfrost_2002,groundfrost_2003, groundfrost_2004, groundfrost_2005, groundfrost_2006, groundfrost_2007, groundfrost_2008, groundfrost_2009, groundfrost_2010, 
                               groundfrost_2011, groundfrost_2012, groundfrost_2013, groundfrost_2014, groundfrost_2015, groundfrost_2016, groundfrost_2017, groundfrost_2018)

monthly_groundfrost <- monthly_groundfrost %>% 
  rename(easting_clim = lon,
         northing_clim = lat)

# bind together in one df. EDIT: bind_cols doesn't work as monthly max_temp is missing 20 obs. Doesn't affect further analysis but odd. Check why  

# Restrict to site easting and northing

site_eastnorth <- read.csv("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/climate_coord.csv")

climate <- left_join(site_eastnorth, monthly_precip, by = c("easting_clim", "northing_clim"))
climate <- left_join(climate, monthly_maxtemp, by = c("easting_clim", "northing_clim", "year"))
climate <- left_join(climate, monthly_temp, by = c("easting_clim", "northing_clim", "year"))
climate <- left_join(climate, monthly_mintemp, by = c("easting_clim", "northing_clim", "year"))
climate <- left_join(climate, monthly_groundfrost, by = c("easting_clim", "northing_clim", "year"))

rm(list=setdiff(ls(), "climate"))

# Create winter climate variables ( October - Feb both months included) and spring/summer climate variables (March, April and May)
# UPDATE - It takes a few minutes to run the code before the data has been reduced, as we have >6 mill obs. Should probably wait to calculate the 
# variables  till after climate data has been joined to sites.UPDATE - I have now restricted climate obs to those from the reserves exclusively
# (excluding Boyton marches 2015 as location is missing). Should reduce running time with ~ 10 min

climate1 <- climate %>% 
  group_by(easting_clim, northing_clim) %>% 
  mutate(rain_winter = (rain_january + rain_february + rain_march +  
                          lag(c(rain_october + rain_november + rain_december), n = 1, order_by = year)),
         rain_spring = rain_april + rain_may + rain_june,
         groundfrost_winter = (groundfrost_january + groundfrost_february + groundfrost_march + 
                                 lag(c(groundfrost_october + groundfrost_november + groundfrost_december), n = 1, order_by = year)),
         groundfrost_spring = groundfrost_april + groundfrost_may + groundfrost_june,
         temp_winter = temp_january + temp_february + temp_march + 
           lag(c(temp_october + temp_november + temp_december), n = 1, order_by = year),
         temp_spring = temp_april + temp_may + temp_june,
         maxtemp_winter = (maxtemp_january + maxtemp_february + maxtemp_march + 
                             lag(c(maxtemp_october + maxtemp_november + maxtemp_december), n = 1, order_by = year)),
         maxtemp_spring = maxtemp_april + maxtemp_may + maxtemp_june,
         mintemp_winter = (mintemp_january + mintemp_february + mintemp_march + 
                             lag(c(mintemp_october + mintemp_november + mintemp_december), n = 1, order_by = year)),
         mintemp_spring = mintemp_april + mintemp_may + mintemp_june)

# Create alternative winter and spring variables 
climate1 <- climate1 %>% 
  group_by(easting_clim, northing_clim) %>% 
  mutate(rain_winter_short = (rain_january + rain_february +  
                                lag(rain_december, n = 1, order_by = year)),
         rain_spring_late = rain_april + rain_may + rain_june,
         groundfrost_winter_short = (groundfrost_january + groundfrost_february +  
                                       lag(groundfrost_december, n = 1, order_by = year)),
         groundfrost_spring_late = groundfrost_april + groundfrost_may + groundfrost_june,
         temp_winter_short = temp_january + temp_february +  
           lag(temp_december, n = 1, order_by = year),
         temp_spring_late = temp_april + temp_may + temp_june,
         maxtemp_winter_short = (maxtemp_january + maxtemp_february +  
                                   lag(maxtemp_december, n = 1, order_by = year)),
         maxtemp_spring_late = maxtemp_april + maxtemp_may + maxtemp_june,
         mintemp_winter_short = (mintemp_january + mintemp_february +  
                                   lag(mintemp_december, n = 1, order_by = year)),
         mintemp_spring_late = mintemp_april + mintemp_may + mintemp_june)

# Get rid of monthly observation, divide temps so that they are means and not sums and 
# use the transformed winter and spring only. For some reason the mean arg doesn't work in this so doing it manually

climate_v2 <- climate1 %>% 
  select(northing_clim, easting_clim, year, ends_with(c('winter','spring', 'short', 'late'))) %>% 
  mutate(temp_winter = temp_winter/6,
         temp_spring = temp_spring/3,
         maxtemp_winter = maxtemp_winter/6,
         maxtemp_spring = maxtemp_spring/3,
         mintemp_winter = mintemp_winter/6,
         mintemp_spring = mintemp_spring/3,
         temp_winter_short = temp_winter_short/3,
         temp_spring_late = temp_spring_late/3,
         maxtemp_winter_short = maxtemp_winter_short/3,
         maxtemp_spring_late = maxtemp_spring_late/3,
         mintemp_winter_short = mintemp_winter_short/3,
         mintemp_spring_late = mintemp_spring_late/3
  )

# Create lagged climate variables for the winter and spring climate

climate_v2 <- climate_v2 %>%
  group_by(northing_clim, easting_clim) %>% 
  mutate(rain_spring_lag = lag(rain_spring, n = 1, order_by = year),
         rain_winter_lag = lag(rain_winter, n = 1, order_by = year),
         groundfrost_spring_lag = lag(groundfrost_spring, n = 1, order_by = year),
         groundfrost_winter_lag = lag(groundfrost_winter, n = 1, order_by = year),
         temp_winter_lag = lag(temp_winter, n = 1, order_by = year),
         temp_spring_lag = lag(temp_spring, n = 1, order_by = year),
         maxtemp_winter_lag = lag(maxtemp_winter, n = 1, order_by = year),
         maxtemp_spring_lag = lag(maxtemp_spring, n = 1, order_by = year),
         mintemp_winter_lag = lag(mintemp_winter, n = 1, order_by = year),
         mintemp_spring_lag = lag(mintemp_spring, n = 1, order_by = year),
         rain_spring_late_lag = lag(rain_spring_late, n = 1, order_by = year),
         rain_winter_short_lag = lag(rain_winter_short, n = 1, order_by = year),
         groundfrost_spring_late_lag = lag(groundfrost_spring_late, n = 1, order_by = year),
         groundfrost_winter_short_lag = lag(groundfrost_winter_short, n = 1, order_by = year),
         temp_winter_short_lag = lag(temp_winter_short, n = 1, order_by = year),
         temp_spring_late_lag = lag(temp_spring_late, n = 1, order_by = year),
         maxtemp_winter_short_lag = lag(maxtemp_winter_short, n = 1, order_by = year),
         maxtemp_spring_late_lag = lag(maxtemp_spring_late, n = 1, order_by = year),
         mintemp_winter_short_lag = lag(mintemp_winter_short, n = 1, order_by = year),
         mintemp_spring_late_lag = lag(mintemp_spring_late, n = 1, order_by = year))


# Part 2: Data preparation ----
## Script for preparing data for analysis 
# # Dont load raster as it messes up dplyr's select command  #  library(raster)
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(tidyverse)
library(readxl)
library(ggpubr)
library(openxlsx)
library(INLA)
library(data.table)
library(gstat)
library(rnaturalearth)
# Load in the species count data and clean it ----
remove_list <- paste(c("Number", "NB I", "Area acquired", "Long-term", "Management agreement", "On lowland wet", "Total"), collapse = '|') # list containing sub_site names to remove

# Clean the raw data and transform from wide to long format

cleaner<-function(x) {
  df<-read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/reserve_breeding_pairs.xlsx", sheet = x)
  df<-df[rowSums(is.na(df)) != ncol(df),]
  df<-df%>%
    rename("sub_site"=1)%>%
    filter(!grepl(remove_list, sub_site))
  df<-gather(df, year, count, -sub_site)
  df<-df%>%
    mutate(species=x)
}
slavonian<-cleaner("Slavonian grebe")
shoveler<-cleaner("Shoveler")
black_grebe<-cleaner("Black-necked grebe")
bittern<-cleaner("Bittern")
little_bittern<-cleaner("Little bittern")
little_egret<-cleaner("Little egret")
great<-cleaner("Great white egret")
garganey<-cleaner("Garganey")
common<-cleaner("Common scoter")
marsh<-cleaner("Marsh harrier")
hen<-cleaner("Hen harrier")
black_grouse<-cleaner("Black grouse")
capercaillie<-cleaner("Capercaillie")
spotted<-cleaner("Spotted crake")
corncrake<-cleaner("Corncrake")
crane<-cleaner("Crane")
black_stilt<-cleaner("Black-winged stilt")
avocet<-cleaner("Avocet")
stone<-cleaner("Stone-curlew")
ringed<-cleaner("Ringed plover")
lapwing<-cleaner("Lapwing")
snipe<-cleaner("Snipe")
wood<-cleaner("Wood sandpiper")
black_limosa<-cleaner("Black-t godwit (limosa)")
black_islandica<-cleaner("Black-t godwit (islandica)")
curlew<-cleaner("Curlew")
redshank<-cleaner("Redshank")
red<-cleaner("Red-necked phalarope")
mediterranean<-cleaner("Mediterranean gull")
little_tern<-cleaner("Little tern")
sandwich<-cleaner("Sandwich tern")
common_tern<-cleaner("Common tern")
roseate<-cleaner("Roseate tern")
nightjar<-cleaner("Nightjar")
woodlark<-cleaner("Woodlark")
nightingale<-cleaner("Nightingale")
ring<-cleaner("Ring ouzel")
cetti<-cleaner("Cetti's warbler")
savi<-cleaner("Savi's warbler")
dartford<-cleaner("Dartford warbler")
bearded<-cleaner("Bearded tit")
golden<-cleaner("Golden oriole")
chough<-cleaner("Chough")
cirl<-cleaner("Cirl bunting")
yellow_wagtail<-cleaner("Yellow wagtail")

# Rbind to transform into one dataset and delete all observations containing NA's.

reserve_species<-rbind(slavonian, shoveler, black_grebe, bittern, little_bittern,
                       little_egret, great, garganey, common, marsh, hen, black_grouse,
                       capercaillie, spotted, corncrake, crane, black_stilt, avocet,
                       stone, ringed, lapwing, snipe, wood, black_limosa, black_islandica,
                       curlew, redshank, red, mediterranean, little_tern, sandwich,
                       common_tern, roseate, nightjar, woodlark, nightingale, ring,
                       cetti, savi, dartford, bearded, golden, chough, cirl, yellow_wagtail)

reserve_species<-drop_na(reserve_species)

# Restrict to wetland species

lwg_species <- c("Curlew", "Lapwing", "Snipe", "Redshank")

lwg_reserve_species<-reserve_species%>%
  filter(species %in% lwg_species)

# Homogenize main and sub site information along area and habitat before acquisition
reserve_information<-read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/RSPB work sheet.xlsx", sheet = "reserve_information")

# Homogenise habitat type prior to conversion (so that "Grass" is renamed "Grassland" and so on)
reserve_information <- reserve_information %>% 
  mutate(habitat_when_acquired = case_when(habitat_when_acquired == "Grass" ~ "Grassland",
                                           habitat_when_acquired == "Mixed" ~ "Grassland & arable",
                                           TRUE ~ habitat_when_acquired))

lwg_reserve_species<-left_join(lwg_reserve_species, reserve_information, by = c("sub_site"))

# Drop observations without proper main and sub site - remember to look these through again with Malcolm for some of the main sites which lack categorisation 
lwg_reserve_species<-lwg_reserve_species[complete.cases(lwg_reserve_species[ , 5]),]

# rearrange columns and delete the old sub_site variable to keep only cleaned main site and sub site variable
lwg_reserve_species<-lwg_reserve_species%>%
  select(-c(sub_site))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, everything())

# set counts to integer
lwg_reserve_species$count<-as.integer(lwg_reserve_species$count)
lwg_reserve_species$year<-as.integer(lwg_reserve_species$year)


#### Create a presence/absence variable  ----
# First step is creating a full site x year dataframe which contains all combinations, as 
# reserve counts are only included for the sites where a species is observed at least once

# Create a year of acquisition as numeric and where the "before 1994" is transformed to 1994 - UPDATE not necessary but haven't 
# checked whether exclusion messes up code - UPDATE fixed by assigning reserve_information directly to pres_abs. 
pres_abs <- reserve_information 

# Create a full presence / absence site x year combination sheet

year = seq(from = 1994, to = 2018, by = 1)

presence_absence <- crossing(pres_abs$sub_site_uniform, year) %>% 
  rename(sub_site_uniform = 1)

# Join the pres_abs and the presence absence sheet
p_a <- left_join(presence_absence, pres_abs, by = "sub_site_uniform")

# get rid of the sub_site variable which isn't homogenized, reorganise and rename the remaining main_site and sub_site so it looks proper
# get rid of wrong sub_site

p_a <- p_a %>% 
  select(everything(), -c(sub_site))

# rename

p_a <- p_a %>% 
  rename(main_site = main_site_uniform,
         sub_site = sub_site_uniform)
# reorder  

p_a <- p_a %>% 
  select(main_site, sub_site, everything())

# Get rid of obs which are from before reserve creation NOTE that a few reserves are losing observations here as they have pre-reserve counts 


# Select sites and year and left join lwg data

p_a <- p_a %>% 
  select(main_site, sub_site, year)

# add species

p_done <- crossing(p_a, lwg_species) %>% 
  rename(species = lwg_species)

# Note that a full join includes all observation whereas a left join only takes obs starting from year of acquisition (full = 14088 obs, left = 13968 obs)

pandabs <- left_join(p_done, lwg_reserve_species, by = c("main_site", "sub_site", "species", 'year')) 

# Fill out the missing covariate values generated by the left join

presence <- pandabs %>% 
  select(main_site, sub_site, year, species, count)

reserve_info <- reserve_information %>% 
  select(everything(), -c(sub_site)) %>% 
  rename(sub_site = sub_site_uniform,
         main_site = main_site_uniform) %>% 
  distinct()

presence <- left_join(presence, reserve_info, by = c("main_site", "sub_site"))

# Now create the presence/absence variable - Missing counts are categorised as absence here but are accounted for later 

presence <- presence %>%
  mutate(presence_absence = case_when(is.na(count) ~ 0,
                                      count > 0 ~ 1,
                                      count == 0 ~ 0))

# Load the sheets with missing years using a modified cleaner function.

cleaner_modified<-function(x) {
  df<-read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/true_NAs.xlsx", sheet = x)
  df<-df[rowSums(is.na(df)) != ncol(df),]
  df<-df%>%
    rename("sub_site"=1)%>%
    filter(!grepl(remove_list, sub_site))
  df<-gather(df, year, count, -sub_site)
  df<-df%>%
    mutate(species=x)
}

lapwing_NA<-cleaner_modified("Lapwing")
snipe_NA<-cleaner_modified("Snipe")
curlew_NA<-cleaner_modified("Curlew")
redshank_NA<-cleaner_modified("Redshank")


# Rbind to transform into one data set and delete all observations containing NA's.

reserve_species_NA<-rbind(lapwing_NA, snipe_NA, curlew_NA, redshank_NA)

# Rename count to absence (1 means absence (NA) in breeding pair data)

reserve_species_NA <- reserve_species_NA %>% 
  rename(absence = count)

reserve_species_NA <- left_join(reserve_species_NA, reserve_information, by = "sub_site") %>% 
  select(main_site_uniform, sub_site_uniform, species, year, absence) %>% 
  rename(main_site = main_site_uniform,
         sub_site = sub_site_uniform)


## Examine which are true NAs. The point here is to verify whether its the same sub_sites in the same years that have true NAs or whether it differs 
## between species. It seems to differ a bit so we will stick to excluding only obs which are true NAs according to reserve breeding pair data.
## This si fixed later on.

diff_in_NAs <- reserve_species_NA %>% group_by(species, sub_site, year) %>% summarise(n_absent = sum(as.numeric(absence), na.rm = TRUE))
diff_in_NAs$n_absent <- as.numeric(diff_in_NAs$n_absent)
diff_in_NAs <- diff_in_NAs %>% pivot_wider(names_from = "year",
                                           values_from ="n_absent")
diff_in_NAs

# Replacing current false NA presence/absence and counts with 0 while making sure that true NAs stay

reserve_species_NA$year <- as.numeric(reserve_species_NA$year)

presence <- left_join(presence, reserve_species_NA, by = c("main_site", "sub_site", "species", "year"))

presence$absence <- as.factor(presence$absence)
presence$absence <- forcats::fct_explicit_na(presence$absence, "present")
presence$count <- as.double(presence$count)

presence <- presence %>% 
  mutate(count_analysis = case_when(absence == "present" & presence_absence == 0 ~ 0,
                                    absence == 1 ~ NA_real_,
                                    absence != 1 ~ count),
         presence_absence_analysis = if_else(absence == "1", NA_real_, presence_absence))

# Add easting and northing 

east_north <- read_csv("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/SiteCentroids.csv")

# Homogenise so it can be joined to the presence data

east_north <- left_join(east_north, reserve_information, by ="sub_site")

# Delete duplicate columns and join

east_north <- east_north %>% 
  select(main_site_uniform, sub_site_uniform, easting, northing) %>% 
  rename(main_site = main_site_uniform,
         sub_site = sub_site_uniform)

# Calculate change in count

presence <- presence %>% 
  group_by(sub_site, species) %>% 
  mutate(count_change = count_analysis - lag(count_analysis, n = 1, order_by = year))

# Create index using year of acquisition as index year

presence <- presence %>% 
  group_by(sub_site, species) %>% 
  mutate(count_analysis_index = count_analysis +1) %>% 
  mutate(index = case_when(year == year_of_acquisition ~ 100,
                           year > year_of_acquisition ~ ((count_analysis_index - lag(count_analysis_index, n = 1, order_by = year))/
                                                           lag(count_analysis_index, n = 1, order_by = year))*100))
# Gamma values along

presence <- presence %>% 
  group_by(sub_site, species) %>% 
  mutate(annual_change = ((count_analysis_index - lag(count_analysis_index, n = 1, order_by = year))/
                            lag(count_analysis_index, n = 1, order_by = year))*100)

# Add Easting and Northing

presence <- left_join(presence, east_north, by = c("main_site", "sub_site"))

#### Now to attach weather data -----
# The reserve coordinates do not match OSG grid east north as grid coords are given in centre of the 1x1 km square and sites 
# are not. Round to nearest number 1000 which can be divided by 500 to match the easting, northing format of the climate data

# rounder function used in next mutate argument
rounder <- function (x) {round(x/1000)*1000}

# first easting

presence <- presence %>% 
  mutate(easting_clim = rounder(easting)) %>%  
  mutate(easting_clim = if_else((easting_clim - easting)<0, easting_clim + 500, easting_clim - 500))

# then northing 

presence <- presence %>% 
  mutate(northing_clim = rounder(northing)) %>%  
  mutate(northing_clim = if_else((northing_clim - northing)<0, northing_clim + 500, northing_clim - 500))

# Eight of the coastal areas fall outside grids with climate variable values. These sites are coastal and some fall outside 
# the terrestrial grid boundary. We therefore chose values from nearest neighbouring grid that contains climate values 

# Adjust 1 grid up or down. Visually inspected where points are for eight sites that did not match properly with climate data 
# in the initial join attempts

presence <- presence %>% 
  mutate(northing_clim = case_when(
    sub_site == "Pagham Harbour" ~ northing_clim + 1000, 
    sub_site == "Wareham Meadows" ~ northing_clim + 1000,
    sub_site == "Higham Marshes" ~ northing_clim - 1000,
    sub_site == "Crook of Baldoon" ~ northing_clim - 1000,
    TRUE ~ northing_clim))

# Adjust 1 grid right or left. Same justification as above

presence <- presence %>% 
  mutate(easting_clim = case_when(
    sub_site == "Lincolnshire Wash Reserves - Freiston Shore" ~ easting_clim - 1000,
    sub_site == "Loch of Strathbeg" ~ easting_clim - 1000,
    sub_site == "Saltholme" ~ easting_clim - 1000,
    sub_site == "Mersehead" ~ easting_clim +1000,
    TRUE ~ easting_clim))

# Join the climatic variables - climate is fed from the climate script

presence <- left_join(presence, climate_v2, by = c("easting_clim", "northing_clim", "year")) 



## Conservation variables ----

## Function that extracts an individual conservation sheet and converts it into tidy format

cons_sheet<-function(sheet_name, value_name) {
  read_excel("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/Conservation_interventions_v1.xlsx", sheet = sheet_name) %>% 
    pivot_longer(cols = 3:28,
                 names_to = "year",
                 values_to = value_name)
}

## Read in summary sheet

status_sheet<-cons_sheet(sheet_name = "status_sheet", value_name = "comments")
status_sheet <- left_join (status_sheet, reserve_information, by = "sub_site") 
status_sheet <- status_sheet %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, comments)

status_sheet$year<-as.numeric(status_sheet$year)


lwg_reserve <- left_join(presence, status_sheet, by = c("main_site", "sub_site", "year"))

# Add predator fence data - This variable does not need any further correction and can be joined directly with breeding pair counts

predator_fence <- cons_sheet(sheet_name ="predator_fence", value_name = "fenced")
predator_fence <- left_join (predator_fence, reserve_information, by = "sub_site") 
predator_fence <- predator_fence %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, fenced)

predator_fence$year<-as.numeric(predator_fence$year)
# Attach to reserve counts

lwg_reserve <- left_join(lwg_reserve, predator_fence, by = c("main_site", "sub_site", "year")) 


# Add predator control for foxes, rows and mink - 1993 - 2014 AR period is different then 2015 and forward (Old ARs being from 1st April - 31 May vs new being 1st October - 31st September).
# Therefore, control interventions in old ARs do not affect breeding birds in the year they are recorded in, but the following year.
# Correct for this by adding +1 to years prior to 2015


# fox
fox_control <- cons_sheet (sheet_name ="fox_control", value_name = "fox_control")
fox_control <- left_join (fox_control, reserve_information, by = "sub_site") 
fox_control$year <- as.numeric(fox_control$year)
fox_control <- fox_control %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, fox_control)
fox_control <- left_join(status_sheet, fox_control, by = c("main_site", "sub_site", "year")) %>% 
  mutate(fox_control = if_else(!comments %in% c("missing","before reserve") & is.na(fox_control),0, fox_control)) %>% 
  mutate(foxes_killed = if_else(fox_control > 0, 1, 0)) %>% 
  select(main_site, sub_site, year, fox_control, foxes_killed) 

# crow
crow_control <- cons_sheet (sheet_name ="crow_control", value_name = "crow_control")
crow_control <- left_join (crow_control, reserve_information, by = "sub_site") 
crow_control$year <- as.numeric(crow_control$year)
crow_control <- crow_control %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, crow_control)
crow_control <- left_join(status_sheet, crow_control, by = c("main_site", "sub_site", "year")) %>% 
  mutate(crow_control = if_else(!comments %in% c("missing","before reserve") & is.na(crow_control),0, crow_control)) %>%
  mutate(crows_killed = if_else(crow_control > 0, 1, 0)) %>% 
  select(main_site, sub_site, year, crow_control, crows_killed)

# mink
mink_control <- cons_sheet (sheet_name ="mink_control", value_name = "mink_control")
mink_control <- left_join (mink_control, reserve_information, by = "sub_site") 
mink_control$year <- as.numeric(mink_control$year)
mink_control <- mink_control %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, mink_control)
mink_control <- left_join(status_sheet, mink_control, by = c("main_site", "sub_site", "year")) %>% 
  mutate(mink_control = if_else(!comments %in% c("missing","before reserve") & is.na(mink_control),0, mink_control)) %>%
  mutate(minks_killed = if_else(mink_control > 0, 1, 0)) %>% 
  select(main_site, sub_site, year, mink_control, minks_killed) 

# combine

control <- full_join(fox_control, crow_control, by = c("year", "sub_site","main_site")) 
control <- full_join(control, mink_control, by = c("year", "sub_site", "main_site"))

# Join predator control and the lwg_reserve sheet

lwg_reserve <- left_join(lwg_reserve, control, by = c("year", "sub_site", "main_site")) 

# Adjust the control variables accordingly so that they account for the difference between recording periods and delayed effect between control
# and response in breeding numbers

lwg_reserve <- lwg_reserve %>% 
  group_by(main_site, sub_site, species) %>% 
  mutate(fox = case_when(year < 2015 ~ lag(foxes_killed, n = 2, order_by=year),
                         year > 2014 ~ lag(foxes_killed, n = 1, order_by=year)),
         crow = case_when(year < 2015 ~ lag(crows_killed, n = 2, order_by=year),
                          year > 2014 ~ lag(crows_killed, n = 1, order_by=year)),
         mink = case_when(year < 2015 ~ lag(minks_killed, n = 2, order_by=year),
                          year > 2014 ~ lag(minks_killed, n = 1, order_by=year)))

lwg_test <- lwg_reserve %>% 
  select(fox_control:mink, everything())  

# Add cattle, horse and sheep grazing and mechanical vegetation control

# Create winter and summer grazing

cattle_grazing <- cons_sheet (sheet_name ="cattle_grazing", value_name = "cattle_grazing")
cattle_grazing <- left_join (cattle_grazing, reserve_information, by = "sub_site") 
cattle_grazing$year <- as.numeric(cattle_grazing$year)
cattle_grazing <- cattle_grazing %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, cattle_grazing) %>%
  mutate(cattle_winter = if_else(cattle_grazing == 2 | cattle_grazing == 3, 1, 0),
         cattle_summer = if_else(cattle_grazing == 1 | cattle_grazing == 3, 1, 0))

# Split winter and summer grazing up and attach to the correct year. 
# Until 2014, AR period goes from 1st April - 31st March. The 2014 - 2015 AR covers 1st April 2014 - 30 September 2015 and is therefore 
# longer than other ARs. From 1st October 2015, the AR period changes to 1st October - 30 September. 
# Report year is attributed to the year that covers the majority of months in the report. This means that a 2000 report will contain 
# summer grazing affecting the breeding population of 2000 but winter grazing for the breeding population of 2001. 
# For the 2015 - 2018 reports, both winter and summer affect the breeding population in the reporting year.  

cattle_winter <- cattle_grazing %>% 
  select(main_site, sub_site, year, cattle_winter) %>%
  filter(year != 2014) %>% 
  mutate(year = if_else(year < 2014, year + 1, year))

cattle_summer <- cattle_grazing %>% 
  select(main_site, sub_site, year, cattle_summer)

# Horse

horse_grazing <- cons_sheet (sheet_name ="horse_grazing", value_name = "horse_grazing")
horse_grazing <- left_join (horse_grazing, reserve_information, by = "sub_site") 
horse_grazing$year <- as.numeric(horse_grazing$year)
horse_grazing <- horse_grazing %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, horse_grazing) %>%
  mutate(horse_winter = if_else(horse_grazing == 2 | horse_grazing == 3, 1, 0),
         horse_summer = if_else(horse_grazing == 1 | horse_grazing == 3, 1, 0))

horse_winter <- horse_grazing %>% 
  select(main_site, sub_site, year, horse_winter) %>%
  filter(year != 2014) %>% 
  mutate(year = if_else(year < 2014, year + 1, year))

horse_summer <- horse_grazing %>% 
  select(main_site, sub_site, year, horse_summer)
# Sheep

sheep_grazing <- cons_sheet (sheet_name ="sheep_grazing", value_name = "sheep_grazing")
sheep_grazing <- left_join (sheep_grazing, reserve_information, by = "sub_site") 
sheep_grazing$year <- as.numeric(sheep_grazing$year)
sheep_grazing <- sheep_grazing %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, sheep_grazing) %>%
  mutate(sheep_winter = if_else(sheep_grazing == 2 | sheep_grazing == 3, 1, 0),
         sheep_summer = if_else(sheep_grazing == 1 | sheep_grazing == 3, 1, 0))

sheep_winter <- sheep_grazing %>% 
  select(main_site, sub_site, year, sheep_winter) %>%
  filter(year != 2014) %>% 
  mutate(year = if_else(year < 2014, year + 1, year))

sheep_summer <- sheep_grazing %>% 
  select(main_site, sub_site, year, sheep_summer)

# Mechanical

other_grass <- cons_sheet (sheet_name ="other_grass", value_name = "other_grass")
other_grass <- left_join (other_grass, reserve_information, by = "sub_site") 
other_grass$year <- as.numeric(other_grass$year)
other_grass <- other_grass %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, other_grass) %>% 
  mutate(year = if_else(year < 2015, year+1, year))

# Combine
grass_summer <- full_join(cattle_summer, horse_summer, by = c("year", "sub_site", "main_site"))
grass_summer <- full_join(grass_summer, sheep_summer, by =c("year", "sub_site", "main_site"))

grass_winter <- full_join(cattle_winter, horse_winter, by = c("year", "sub_site", "main_site"))
grass_winter <- full_join(grass_winter, sheep_winter, by = c("year", "sub_site", "main_site"))

grass <- full_join(grass_summer, grass_winter, by = c("year", "sub_site", "main_site"))

grass <- full_join(grass, other_grass, by =c("year", "sub_site", "main_site"))

lwg_reserve <- left_join(lwg_reserve, grass, by = c("year", "sub_site", "main_site")) %>% 
  mutate(cattle_summer = if_else(!is.na(comments) & is.na(cattle_summer),0, cattle_grazing),
         sheep_grazing = if_else(!is.na(comments) & is.na(sheep_grazing),0, sheep_grazing),
         horse_grazing = if_else(!is.na(comments) & is.na(horse_grazing),0, horse_grazing),
         other_grass = if_else(!is.na(comments) & is.na(other_grass),0, other_grass))

# Add adjustable and fixed water control structures

# Adjustable water control structures

water_adj <- cons_sheet(sheet_name ="water_management_structures_adj", value_name = "water_adj")
water_adj <- left_join (water_adj, reserve_information, by = "sub_site") 
water_adj$year <- as.numeric(water_adj$year)
water_adj <- water_adj %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, water_adj) %>% 
  group_by(sub_site) %>% 
  mutate(water_adj = case_when(year == 2015 & is.na(water_adj) & !is.na(lead(water_adj, n = 1, order_by = c(year))) ~ lag(water_adj, n = 1, order_by = c(year)),
                               year == 2015 & !is.na(water_adj) & is.na(lead(water_adj, n = 1, order_by = c(year))) ~ water_adj,
                               year > 2015 ~ water_adj,
                               year < 2014 ~ lag(water_adj, n = 1, order_by = c(year)))) %>% 
  mutate(water_adj_binary = if_else(!is.na(water_adj), 1, water_adj))
# Fixed water control structures

water_fix <- cons_sheet(sheet_name ="water_management_structures_fix", value_name = "water_fix")
water_fix <- left_join (water_fix, reserve_information, by = "sub_site") 
water_fix$year <- as.numeric(water_fix$year)
water_fix <- water_fix %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, water_fix) %>% 
  group_by(sub_site) %>% 
  mutate(water_fix = case_when(year == 2015 & is.na(water_fix) & !is.na(lead(water_fix, n = 1, order_by = c(year))) ~ lag(water_fix, n = 1, order_by = c(year)),
                               year == 2015 & !is.na(water_fix) & is.na(lead(water_fix, n = 1, order_by = c(year))) ~ water_fix,
                               year > 2015 ~ water_fix,
                               year < 2014 ~ lag(water_fix, n = 1, order_by = c(year)))) %>% 
  mutate(water_fix_binary = if_else(!is.na(water_fix), 1, water_fix))

# Add foot drains and pond/scrapes

#Foot drains

foot_drain <- cons_sheet(sheet_name ="foot_drain", value_name = "foot_drain")
foot_drain <- left_join (foot_drain, reserve_information, by = "sub_site") 
foot_drain$year <- as.numeric(foot_drain$year)
foot_drain <- foot_drain %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, foot_drain) %>% 
  group_by(sub_site) %>% 
  mutate(foot_drain = case_when(year == 2015 & is.na(foot_drain) & !is.na(lead(foot_drain, n = 1, order_by = c(year))) ~ lag(foot_drain, n = 1, order_by = c(year)),
                                year == 2015 & !is.na(foot_drain) & is.na(lead(foot_drain, n = 1, order_by = c(year))) ~ foot_drain,
                                year > 2015 ~ foot_drain,
                                year < 2014 ~ lag(foot_drain, n = 1, order_by = c(year)))) %>% 
  mutate(foot_drain_binary = if_else(!is.na(foot_drain), 1, foot_drain))

# Pond and scrapes

excavation_pond_scrape <- cons_sheet(sheet_name ="excavation_pond_scrape", value_name = "excavation_pond_scrape")
excavation_pond_scrape <- left_join (excavation_pond_scrape, reserve_information, by = "sub_site") 
excavation_pond_scrape$year <- as.numeric(excavation_pond_scrape$year)
excavation_pond_scrape <- excavation_pond_scrape %>% 
  select(-c("main_site","sub_site"))%>%
  rename(sub_site=sub_site_uniform, main_site=main_site_uniform)%>%
  select(main_site, sub_site, year, excavation_pond_scrape) %>% 
  group_by(sub_site) %>% 
  mutate(excavation_pond_scrape = case_when(year == 2015 & is.na(excavation_pond_scrape) & !is.na(lead(excavation_pond_scrape, n = 1, order_by = c(year))) ~ lag(excavation_pond_scrape, n = 1, order_by = c(year)),
                                            year == 2015 & !is.na(excavation_pond_scrape) & is.na(lead(excavation_pond_scrape, n = 1, order_by = c(year))) ~ excavation_pond_scrape,
                                            year > 2015 ~ excavation_pond_scrape,
                                            year < 2014 ~ lag(excavation_pond_scrape, n = 1, order_by = c(year)))) %>% 
  mutate(excavation_pond_scrape_binary = if_else(!is.na(excavation_pond_scrape), 1, excavation_pond_scrape))

# Fill in missing values in adjustable and fixed water structures to create a cumulative factor variable that increases by 1 when 
# a year has new adjustable water control structures added. Do the same for excavation work and foot drains.

# Water adj
water_adj <- water_adj %>% 
  group_by(sub_site) %>% 
  mutate(water_adj_binary1 = if_else(is.na(water_adj_binary), 0, water_adj_binary),
         water_adj_analysis = cumsum(water_adj_binary1))

# Water fixed
water_fix <- water_fix %>% 
  group_by(sub_site) %>% 
  mutate(water_fix_binary1 = if_else(is.na(water_fix_binary), 0, water_fix_binary),
         water_fix_analysis = cumsum(water_fix_binary1))

# Excavation of ponds and scrapes
excavation_pond_scrape <- excavation_pond_scrape %>% 
  group_by(sub_site) %>% 
  mutate(excavation_pond_scrape_binary1 = if_else(is.na(excavation_pond_scrape_binary), 0, excavation_pond_scrape_binary),
         excavation_pond_scrape_analysis = cumsum(excavation_pond_scrape_binary1))

# Foot drains
foot_drain <- foot_drain %>% 
  group_by(sub_site) %>% 
  mutate(foot_drain_binary1 = if_else(is.na(foot_drain_binary), 0, foot_drain_binary),
         foot_drain_analysis = cumsum(foot_drain_binary1))

# Create counter for temporal interaction in adjustable water control structures - not using this atm as it 
# makes analysis too complicated

setDT(water_adj)[, counter := seq_len(.N), by = .(sub_site, rleid(water_adj_analysis))]


# Attach to the reserve data

lwg_reserve <- left_join(lwg_reserve, water_adj, by = c("year", "sub_site", "main_site"))

lwg_reserve <- left_join(lwg_reserve, water_fix, by = c("year", "sub_site", "main_site"))

lwg_reserve <- left_join(lwg_reserve, foot_drain, by = c("year", "sub_site", "main_site"))

lwg_reserve <- left_join(lwg_reserve, excavation_pond_scrape, by = c("year", "sub_site", "main_site"))

# Create a single variable that collates fixed water, foot drains and excavation as a single effort variable.
# All of these are related to storing and retaining water so makes sense to summarise.

lwg_reserve <- lwg_reserve %>% 
  group_by(species, main_site, sub_site) %>% 
  mutate(water_surface = water_fix_analysis +foot_drain_analysis + excavation_pond_scrape_analysis)

# Create a binary variable that indicates the first year that a reserve site has adjustable water control structure 
# UPDATE not used in analysis

lwg_reserve <- lwg_reserve %>% 
  mutate(adj_bin = if_else(water_adj_analysis > 0, 1, 0))

# Get rid of every obs prior to reserve creation and without information

lwg_reserve <- lwg_reserve %>% 
  filter(comments != "before reserve")

# Calculate years since acquisition
lwg_reserve <- lwg_reserve %>%
  group_by(main_site, sub_site, species) %>% 
  mutate(year_since_acq = as.numeric(year) - as.numeric(year_of_acquisition))

# Create habitat_upon_acquisition dummy variables to use for smoothing with different RW or AR structures in the analysis script  


lwg_reserve <- lwg_reserve %>% 
  mutate(grassland = if_else(habitat_when_acquired == "Grassland", 1, 0),
         arable = if_else(habitat_when_acquired == "Arable", 1, 0),
         ex_mineral = if_else(habitat_when_acquired == "Ex-mineral workings", 1, 0),
         grassland_arable = if_else(habitat_when_acquired == "Grassland & arable", 1, 0))

# Clean data so that duplicated variables (from joining) and unnecessary variables are excluded
# UPDATE - This one needs a redoing. Prior functions have been changed so year_corrected are not used. 

lwg_reserve <- lwg_reserve %>% 
  select(everything(), - c(presence_absence, absence, water_fix, water_adj, water_fix_binary, water_fix_binary1,
                           water_adj_binary, water_adj_binary1, foot_drain_binary, foot_drain, foot_drain_binary1, excavation_pond_scrape,
                           excavation_pond_scrape_binary, excavation_pond_scrape_binary1)) %>% 
  ungroup()

# Get rid of true NAs

lwg_reserve <- lwg_reserve %>% 
  filter(!is.na(count_analysis))

# Create a cumulative year where 1994 is first year (1994 = year 1) UPDATE - not really used in analysis

lwg_reserve <- lwg_reserve %>% 
  mutate(year_cum = year - 1993)

# Create a variable for years since 1st obs

lwg_reserve <- lwg_reserve %>% 
  group_by(species, sub_site) %>% 
  mutate(index_year = min(year)) 

# Calculate age since first year of observation

lwg_reserve <- lwg_reserve %>% 
  mutate(first_year = year - index_year)


# Export to validate how it looks - It looks fine but I am not too sure about how grazing should be added
# write.csv(lwg_reserve, "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/temp_data1.csv")

# Create map of all UK reserve sites


UK_shape <- rgdal::readOGR("C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/data/UK_poly.shp")

proj4string(UK_shape) 

# Transform easting northing coords to lat long as easting northing distorts projection in national scale analysis

### shortcuts
ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

# Exclude NA lat long obs as these mess up the conversion to a spatial points dataframe

lwg_reserve <- lwg_reserve %>% 
  filter(!is.na(easting))


### Create coordinates variable
coords <- cbind(Easting = as.numeric(as.character(lwg_reserve$easting)),
                Northing = as.numeric(as.character(lwg_reserve$northing)))

### Create the SpatialPointsDataFrame
dat_SP <- SpatialPointsDataFrame(coords,
                                 data = lwg_reserve,
                                 proj4string = CRS("+init=epsg:27700"))

### Convert
dat_SP_LL <- spTransform(dat_SP, CRS(latlong))

## replace Lat, Long
lwg_reserve$long <- coordinates(dat_SP_LL)[, 1]
lwg_reserve$lat <- coordinates(dat_SP_LL)[, 2]


# There are a few sites with conservation site info but no real counts. These get assigned zero counts when we expand site x year but are not 
# true zeros. Therefore, delete those obs. We do this by creating a list with all the false zero obs and anti join. Using != doesn't work for 
# some reason but == and anti_join does... (it is probably obvious but I, for some reason, cannot see it rn)   
# Middleton Lakes 2007 are excluded for Curlew and Snipe as Redshank and Lapwing are recorded as true NAs whereas Curlew and Snipe 
# count as zeros (because of how we expand the data) if not accounted for here. Same with Barleycroft and Berry fen for Curlew and Snipe 2006 and 2007

lwg_zeros <- lwg_reserve %>% 
  filter(sub_site == "Berney Marshes - Compartment 81-103" & year == 2004 |
           sub_site == "Brading Marshes (47406)" & year == 2001 | 
           sub_site == "Brading Marshes (47407)" & year == 2002 |
           sub_site == "Brading Marshes (47412)" & year == 2003 |
           sub_site == "Brading Marshes (47412)" & year == 2004 |
           sub_site == "Brodgar" & year == 2001 & species %in%  c("Snipe","Redshank","Curlew") |
           sub_site == "Brodgar" & year == 2002 & species %in%  c("Snipe","Redshank","Curlew") |
           sub_site == "Campfield Marsh - Biglands" & year == 2006 & species %in% c("Snipe","Redshank") |
           sub_site == "Crook of Baldoon" & year == 2011 |
           sub_site == "Hollesley Marshes" & year == 2006 |
           sub_site == "Lincolnshire Wash Reserves - Freiston Shore" & year == 2005 |
           sub_site == "Lough Beg" & year == 2005 |
           sub_site == "Lydden Valley" & year == 2009 |
           sub_site == "Middleton Lakes" & year == 2007 & species %in% c("Snipe","Curlew") |
           sub_site == "Morfa Dinlle" & year == 2006 |
           sub_site == "Nene Washes - Eldernell Wash" & year == 2002 |
           sub_site == "Nene Washes - High Wash" & year == 2003 |
           sub_site == "Nene Washes - March Farmers" & year == 2006 |
           sub_site == "Nene Washes - March Farmers" & year == 2007 |
           sub_site == "Otmoor - Big Otmoor" & year == 1998 |
           sub_site == "Otmoor - Flood Field & Barn Field" & year == 1997 |
           sub_site == "Otmoor - Flood Field & Barn Field" & year == 1998 |
           sub_site == "Otmoor - Greenaways" & year == 1997 |
           sub_site == "Ouse Fen - Barleycroft & Berry Fen" & year == 2006 & species %in% c("Snipe","Curlew") |
           sub_site == "Ouse Fen - Barleycroft & Berry Fen" & year == 2007 & species %in% c("Snipe","Curlew") |
           sub_site == "Pagham Harbour" & year == 2011 |
           sub_site == "Pagham Harbour" & year == 2012 & species != "Redshank" |
           sub_site == "Saltholme - Cowpen Marsh" & year == 2011 & species %in% c("Snipe","Curlew")|
           sub_site == "Seasalter Levels - LNR (47476)" & year == 2007 |
           sub_site == "Stour Estuary - Cattawade Marshes" & year == 2005 |
           sub_site == "Ynys-hir - 47891: Penrhyngerwin" & year == 1998 |
           sub_site == "Ynys-hir - 47891: Penrhyngerwin" & year == 1999 |
           sub_site == "Ynys-hir - 47892: Lodge Farm" & year == 1998 |
           sub_site == "Ynys-hir - 47892: Lodge Farm" & year == 1999 |
           main_site == "South Essex Marshes" & year %in% c(2006, 2007))

# Filter the lwg data so that it doesn't contain any of the above observations

lwg_reserve <- anti_join(lwg_reserve, lwg_zeros, by = c("sub_site","year","species")) 

# The source for the Zuur helping functions need to go here. They don't run properly when sourced 
# in the start of the script  

source("C:/Users/seanj/OneDrive - University College London/Books/Spatial, Temporal and Spatial-Temporal Ecological Data Analysis with R-INLA/HighstatLibV11.R")

# Housekeeping. Make sure all variables are the right type
lwg_reserve$count_analysis <- as.integer(lwg_reserve$count_analysis)

lwg_reserve$main_site <-factor(lwg_reserve$main_site)

lwg_reserve$sub_site <-factor(lwg_reserve$sub_site)  

lwg_reserve$habitat_when_acquired <-factor(lwg_reserve$habitat_when_acquired)

lwg_reserve$climate_district <-factor(lwg_reserve$climate_district)

lwg_reserve$Ffenced <- lwg_reserve$fenced # Should be taken out and just use "fenced" but later code uses Ffenced

lwg_reserve$country <- factor(lwg_reserve$country)


# Standardize continuous covariates to avoid numerical complications. Site area still contains NAs so cannot be standardized
# before these are filtered away. This is done in the final analysis df for each species after using model.matrix 
# to specify a manual intercept 

lwg_reserve$rain_winter.std <- MyStd(lwg_reserve$rain_winter)

lwg_reserve$rain_spring.std <- MyStd(lwg_reserve$rain_spring)

lwg_reserve$temp_winter.std <- MyStd(lwg_reserve$temp_winter)

lwg_reserve$temp_spring.std <- MyStd(lwg_reserve$temp_spring)

lwg_reserve$maxtemp_winter.std <- MyStd(lwg_reserve$maxtemp_winter)

lwg_reserve$maxtemp_spring.std <- MyStd(lwg_reserve$maxtemp_spring)

lwg_reserve$mintemp_winter.std <- MyStd(lwg_reserve$mintemp_winter)

lwg_reserve$mintemp_spring.std <- MyStd(lwg_reserve$mintemp_spring)

lwg_reserve$groundfrost_winter.std <- MyStd(lwg_reserve$groundfrost_winter)

lwg_reserve$groundfrost_spring.std <- MyStd(lwg_reserve$groundfrost_spring)

lwg_reserve$rain_winter_lag.std <- MyStd(lwg_reserve$rain_winter_lag)

lwg_reserve$rain_spring_lag.std <- MyStd(lwg_reserve$rain_spring_lag)

lwg_reserve$temp_winter_lag.std <- MyStd(lwg_reserve$temp_winter_lag)

lwg_reserve$temp_spring_lag.std <- MyStd(lwg_reserve$temp_spring_lag)

lwg_reserve$maxtemp_winter_lag.std <- MyStd(lwg_reserve$maxtemp_winter_lag)

lwg_reserve$maxtemp_spring_lag.std <- MyStd(lwg_reserve$maxtemp_spring_lag)

lwg_reserve$mintemp_winter_lag.std <- MyStd(lwg_reserve$mintemp_winter_lag)

lwg_reserve$mintemp_spring_lag.std <- MyStd(lwg_reserve$mintemp_spring_lag)

lwg_reserve$groundfrost_winter_lag.std <- MyStd(lwg_reserve$groundfrost_winter_lag)

lwg_reserve$groundfrost_spring_lag.std <- MyStd(lwg_reserve$groundfrost_spring_lag)

# Alternative climate variables  - short winter and spring/summer delayed by one month

lwg_reserve$rain_winter_short.std <- MyStd(lwg_reserve$rain_winter_short)

lwg_reserve$rain_spring_late.std <- MyStd(lwg_reserve$rain_spring_late)

lwg_reserve$temp_winter_short.std <- MyStd(lwg_reserve$temp_winter_short)

lwg_reserve$temp_spring_late.std <- MyStd(lwg_reserve$temp_spring_late)

lwg_reserve$maxtemp_winter_short.std <- MyStd(lwg_reserve$maxtemp_winter_short)

lwg_reserve$maxtemp_spring_late.std <- MyStd(lwg_reserve$maxtemp_spring_late)

lwg_reserve$mintemp_winter_short.std <- MyStd(lwg_reserve$mintemp_winter_short)

lwg_reserve$mintemp_spring_late.std <- MyStd(lwg_reserve$mintemp_spring_late)

lwg_reserve$groundfrost_winter_short.std <- MyStd(lwg_reserve$groundfrost_winter_short)

lwg_reserve$groundfrost_spring_late.std <- MyStd(lwg_reserve$groundfrost_spring_late)

lwg_reserve$rain_winter_short_lag.std <- MyStd(lwg_reserve$rain_winter_short_lag)

lwg_reserve$rain_spring_late_lag.std <- MyStd(lwg_reserve$rain_spring_late_lag)

lwg_reserve$temp_winter_short_lag.std <- MyStd(lwg_reserve$temp_winter_short_lag)

lwg_reserve$temp_spring_late_lag.std <- MyStd(lwg_reserve$temp_spring_late_lag)

lwg_reserve$maxtemp_winter_short_lag.std <- MyStd(lwg_reserve$maxtemp_winter_short_lag)

lwg_reserve$maxtemp_spring_late_lag.std <- MyStd(lwg_reserve$maxtemp_spring_late_lag)

lwg_reserve$mintemp_winter_short_lag.std <- MyStd(lwg_reserve$mintemp_winter_short_lag)

lwg_reserve$mintemp_spring_late_lag.std <- MyStd(lwg_reserve$mintemp_spring_late_lag)

lwg_reserve$groundfrost_winter_short_lag.std <- MyStd(lwg_reserve$groundfrost_winter_short_lag)

lwg_reserve$groundfrost_spring_late_lag.std <- MyStd(lwg_reserve$groundfrost_spring_late_lag)

# Get coordinates in km 

lwg_reserve <- lwg_reserve %>% 
  mutate(easting_km = easting/1000,
         northing_km = northing/1000)

# Exclude sites with limited management information

lwg_reserve <- lwg_reserve %>% 
  filter(main_site != "St Aidans") #  St Aidans only have one annual report over 15 years. Some can be derived from other reports but limited info

# Fill out predator control for the first few year after acquisition in reserves without control

lwg_reserve <- lwg_reserve %>% 
  mutate(fox = case_when(sub_site == "Berney Marshes - Compartment 42-65" & year %in% c(1994, 1995) ~ 0,
                         sub_site == "Brading Marshes (108710)" & year == 2014 ~ 0,
                         sub_site == "Campfield Marsh - Farm (North Plain)" & year %in% c(1994, 1995) ~ 0,
                         sub_site == "Cors Ddyga - (47831) Area A Freehold" & year %in% c(1995, 1996) ~ 0,
                         sub_site == "Cors Ddyga (47836)" & year == 1997 ~ 0,
                         sub_site == "Hollesley Marshes" & year == 2007 ~ 0,
                         sub_site == "Lower Lough Erne - Muckinish" & year == 1994 ~ 0,
                         sub_site == "Mersehead" & year %in% c(1994, 1995) ~ 0,
                         sub_site == "Mid Yare - Buckenham & Cantley" & year %in% c(1994, 1995) ~ 0,
                         sub_site == "Marshside" & year == 1995 ~ 0,
                         TRUE ~ fox))


# Nested site within reserve variable used for choosing random effects in model selection and 
# subsequent analysis  

# Pretty sure I have to specify the matrix for each species isolated, otherwise the nested random effects don't really make sense 
# for modelling each species individually. Therefore, Create dataset for each species prior to creating the nested random effects

# Lapwing - Exclude 2015 Boyton Marshes as I haven't been able to identify its centre point and Ouse Washes 
#as I haven't accounted for flooding. Exclude Minsmere as we do not know when the grassland part was created


lwg_lapwing <- lwg_reserve %>% 
  filter(species == "Lapwing")%>%
  filter(year_of_acquisition >= 1993) %>% 
  filter(sub_site != "Boyton Marsh - 2015 acquisition") %>% 
  filter(main_site != "Ouse Washes") %>% 
  filter(main_site != "Minsmere") %>% 
  filter(!is.na(area_of_lwg_ha))

# Redshank - Exclude 2015 Boyton Marshes as I haven't been able to identify its centre point and Ouse Washes 
# as I haven't accounted for flooding. Exclude Minsmere as we do not know when the grassland part was created

lwg_redshank <- lwg_reserve %>% 
  filter(species == "Redshank")%>% 
  filter(year_of_acquisition >= 1993) %>% 
  filter(sub_site != "Boyton Marsh - 2015 acquisition") %>% 
  filter(main_site != "Ouse Washes") %>% 
  filter(main_site != "Minsmere") %>% 
  filter(!is.na(area_of_lwg_ha))

# Curlew - Exclude 2015 Boyton Marshes as I haven't been able to identify its centre point and Ouse Washes 
#as I haven't accounted for flooding. Exclude Minsmere as we do not know when the grassland part was created

lwg_curlew <- lwg_reserve %>% 
  filter(species == "Curlew")%>% 
  filter(year_of_acquisition >= 1993) %>% 
  filter(sub_site != "Boyton Marsh - 2015 acquisition") %>% 
  filter(main_site != "Ouse Washes") %>% 
  filter(main_site != "Minsmere") %>% 
  filter(!is.na(area_of_lwg_ha))

# Snipe - Exclude 2015 Boyton Marshes as I haven't been able to identify its centre point and Ouse Washes 
#as I haven't accounted for flooding. Exclude Minsmere as we do not know when the grassland part was created

lwg_snipe <- lwg_reserve %>% 
  filter(species == "Snipe")%>% 
  filter(year_of_acquisition >= 1993) %>% 
  filter(sub_site != "Boyton Marsh - 2015 acquisition") %>% 
  filter(main_site != "Ouse Washes") %>% 
  filter(main_site != "Minsmere") %>% 
  filter(!is.na(area_of_lwg_ha))

# Sum of breeding pairs. Used for descriptive stuff in the start of the result section. 
lwg_reserve %>% 
  filter(year_of_acquisition >= 1993) %>% 
  filter(sub_site != "Boyton Marsh - 2015 acquisition") %>% 
  filter(main_site != "Ouse Washes") %>% 
  filter(main_site != "Minsmere") %>% 
  filter(!is.na(area_of_lwg_ha)) %>% 
  group_by(species) %>% 
  summarise(breeding_pairs = sum(count_analysis))

# Plot points together with UK map 

sites<-ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_point(data = lwg_reserve, aes(long, lat, color = first_year, size = area_of_lwg_ha), shape = 21, stroke = 2) + 
  scale_color_viridis_c(option = "plasma", direction = -1) +
  theme(legend.title = element_text(size = 30),
        legend.text = element_text(size = 16, face = "bold")) +
  theme_bw()+
  labs(colour = "Site age", size = "Site size ha")

# Save
#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/sites.tiff",
#       plot = sites, compression = "lzw", width = 20, height = 25, dpi = 400, units = "cm")


# SD and mean size of sites. Use Lapwing as it has all sites that are used

lwg_lapwing %>% 
  select(sub_site, area_of_lwg_ha) %>% 
  distinct() %>% 
  ungroup() %>% 
  summarise(mean_area = mean(area_of_lwg_ha),
            sd_area   = sd(area_of_lwg_ha))

# Mean site age. Median yields similar result

lwg_reserve %>% 
  ungroup() %>% 
  filter(year == 2018) %>% 
  summarise(mean_management = mean(first_year))


## Check for missing values and if there are any, then at what sites

# Count nr of NAs in each column ----
colSums(is.na(lwg_lapwing))
colSums(is.na(lwg_redshank))
colSums(is.na(lwg_curlew))
colSums(is.na(lwg_snipe))
# Missing predator fencing data ----

lap_fence <- lwg_lapwing %>% filter(is.na(fenced))
red_fence <- lwg_redshank %>% filter(is.na(fenced))
sni_fence <- lwg_snipe %>% filter(is.na(fenced))
cur_fence <- lwg_curlew %>% filter(is.na(fenced))

## Missing predator control data

lap_control <- lwg_lapwing %>% filter(is.na(fox))
red_control <- lwg_redshank %>% filter(is.na(fox))
sni_control <- lwg_snipe %>% filter(is.na(fox))
cur_control <- lwg_curlew %>% filter(is.na(fox))

# Exclude sites with zero counts for Snipe and Curlew (try to do the same for Lapwing and Redshank). 
# identify reserves with one positive

cur_positive <- lwg_curlew %>% 
  group_by(sub_site) %>% 
  summarise(counts = sum(count_analysis))

sni_positive <- lwg_snipe %>% 
  group_by(sub_site) %>% 
  summarise(counts = sum(count_analysis))

# Select them 
cur_ana <- cur_positive %>% 
  filter(counts>0)

sni_ana <- sni_positive %>% 
  filter(counts>0)

# Join counts from main curlew data
lwg_curlew <- left_join(cur_ana, lwg_curlew, by = "sub_site")  

lwg_snipe <- left_join(sni_ana, lwg_snipe, by = "sub_site")  


# Plot the annual and mean counts across the UK to get a feeling for whether distribution range is important when working
# exclusively with reserves - The distribution range seems to be particularly important for Snipe and Curlew. 
# No clear pattern for Lapwing and Redshank when not accounting for site size

# Lapwing

#lapwing_map <- ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  geom_point(data = lwg_lapwing, aes(long, lat, color = count_analysis, size = area_of_lwg_ha)) + 
#  scale_color_viridis_c(option = "plasma", direction = -1) +
#  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
#  theme_void() +
#  guides(size = FALSE) + 
#  labs(colour = "Breeding pairs \nLapwing") +
#  facet_wrap(~year)

# Lapwing_mean <- lwg_lapwing %>% 
#  group_by(sub_site) %>%
#  mutate(mean_count = mean(count_analysis))

#lapwing_mean_map <- ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  geom_point(data = lapwing_mean, aes(long, lat, color = mean_count, size = area_of_lwg_ha)) + 
#  scale_color_viridis_c(option = "plasma", direction = -1) +
#  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
#  theme_void() +
#  guides(size = FALSE) + 
#  labs(colour = "Mean breeding pairs \nLapwing")

# Redshank

#redshank_map <- ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  geom_point(data = lwg_redshank, aes(long, lat, color = count_analysis, size = area_of_lwg_ha)) + 
#  scale_color_viridis_c(option = "plasma", direction = -1) +
#  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
#  theme_void() +
#  guides(size = FALSE) + 
#  labs(colour = "Breeding pairs \nRedshank") +
#  facet_wrap(~year)

# Redshank mean

#redshank_mean <- lwg_redshank %>% 
#  group_by(sub_site) %>%
#  mutate(mean_count = mean(count_analysis))

#redshank_mean_map <- ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  geom_point(data = redshank_mean, aes(long, lat, color = mean_count, size = area_of_lwg_ha)) + 
#  scale_color_viridis_c(option = "plasma", direction = -1) +
#  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
#  theme_void() +
#  guides(size = FALSE) + 
#  labs(colour = "Mean breeding pairs \nredshank")

# Curlew yearly

#curlew_map <- ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  geom_point(data = lwg_curlew, aes(long, lat, color = count_analysis, size = area_of_lwg_ha)) + 
#  scale_color_viridis_c(option = "plasma", direction = -1) +
#  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
#  theme_void() +
#  guides(size = FALSE) + 
#  labs(colour = "Breeding pairs \nCurlew") +
#  facet_wrap(~year)

# Curlew mean

#curlew_mean <- lwg_curlew %>% 
#  group_by(sub_site) %>%
#  mutate(mean_count = mean(count_analysis))

#curlew_mean_map <- ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  geom_point(data = curlew_mean, aes(long, lat, color = mean_count, size = area_of_lwg_ha)) + 
#  scale_color_viridis_c(option = "plasma", direction = -1) +
#  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
#  theme_void() +
#  guides(size = FALSE) + 
#  labs(colour = "Mean breeding pairs \ncurlew")

# Snipe yearly

#snipe_map <- ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  geom_point(data = lwg_snipe, aes(long, lat, color = count_analysis, size = area_of_lwg_ha)) + 
#  scale_color_viridis_c(option = "plasma", direction = -1) +
#  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
#  theme_void() +
#  guides(size = FALSE) + 
#  labs(colour = "Breeding pairs \nSnipe") +
#  facet_wrap(~year)

# Snipe mean

#snipe_mean <- lwg_snipe %>% 
#  group_by(sub_site) %>%
#  mutate(mean_count = mean(count_analysis))

#snipe_mean_map <- ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  geom_point(data = snipe_mean, aes(long, lat, color = mean_count, size = area_of_lwg_ha)) + 
#  scale_color_viridis_c(option = "plasma", direction = -1) +
#  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
#  theme_void() +
#  guides(size = FALSE) + 
#  labs(colour = "Mean breeding pairs \nsnipe")


#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/lapwing_map.tiff",
#       plot = lapwing_map, compression = "lzw", width = 50, height = 50, dpi = 400, units = "cm")

#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/lapwing_mean_map.tiff",
#       plot = lapwing_mean_map, compression = "lzw", width = 20, height = 25, dpi = 400, units = "cm")

#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/redshank_map.tiff",
#       plot = redshank_map, compression = "lzw", width = 50, height = 50, dpi = 400, units = "cm")

#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/redshank_mean_map.tiff",
#       plot = redshank_mean_map, compression = "lzw", width = 20, height = 25, dpi = 400, units = "cm")

#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/curlew_map.tiff",
#       plot = curlew_map, compression = "lzw", width = 50, height = 50, dpi = 400, units = "cm")

# ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/curlew_mean_map.tiff",
#       plot = curlew_mean_map, compression = "lzw", width = 20, height = 25, dpi = 400, units = "cm")

#ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/snipe_map.tiff",
#       plot = snipe_map, compression = "lzw", width = 50, height = 50, dpi = 400, units = "cm")

# ggsave(filename = "C:/Users/seanj/OneDrive - University College London/Articles from Thesis/2. Effect of conservation interventions/Plots and graphs/snipe_mean_map.tiff",
#       plot = snipe_mean_map, compression = "lzw", width = 20, height = 25, dpi = 400, units = "cm")

# And now proceed with the design matrix for the random effects for Lapwing 

l_zms <- as(model.matrix( ~ 0 + main_site:sub_site, data = lwg_lapwing), "Matrix")
l_zcms <- as(model.matrix(  ~ 0 + climate_district:main_site:sub_site, data = lwg_lapwing), "Matrix")
l_zcoms <- as(model.matrix(  ~ 0 + country:main_site:sub_site, data = lwg_lapwing), "Matrix")

# and convert to an index variable which can be used with the "iid" random effect feature in INLA while
# specifying the design matrix for the "z" random effect 

lwg_lapwing$mainsub <- as.factor(apply(l_zms, 1, function(x){names(x)[x == 1]}))
lwg_lapwing$countymainsub <- as.factor(apply(l_zcms, 1, function(x){names(x)[x == 1]}))
lwg_lapwing$countrymainsub <- as.factor(apply(l_zcoms, 1, function(x){names(x)[x == 1]}))

# And similar for Redshank

r_zms <- as(model.matrix( ~ 0 + main_site:sub_site, data = lwg_redshank), "Matrix")
r_zcms <- as(model.matrix(  ~ 0 + climate_district:main_site:sub_site, data = lwg_redshank), "Matrix")
r_zcoms <- as(model.matrix(  ~ 0 + country:main_site:sub_site, data = lwg_redshank), "Matrix")

lwg_redshank$mainsub <- as.factor(apply(r_zms, 1, function(x){names(x)[x == 1]}))
lwg_redshank$countymainsub <- as.factor(apply(r_zcms, 1, function(x){names(x)[x == 1]}))
lwg_redshank$countrymainsub <- as.factor(apply(r_zcoms, 1, function(x){names(x)[x == 1]}))

# And Curlew

c_zms <- as(model.matrix( ~ 0 + main_site:sub_site, data = lwg_curlew), "Matrix")
c_zcms <- as(model.matrix(  ~ 0 + climate_district:main_site:sub_site, data = lwg_curlew), "Matrix")
c_zcoms <- as(model.matrix(  ~ 0 + country:main_site:sub_site, data = lwg_curlew), "Matrix")

lwg_curlew$mainsub <- as.factor(apply(c_zms, 1, function(x){names(x)[x == 1]}))
lwg_curlew$countymainsub <- as.factor(apply(c_zcms, 1, function(x){names(x)[x == 1]}))
lwg_curlew$countrymainsub <- as.factor(apply(c_zcoms, 1, function(x){names(x)[x == 1]}))

# And Snipe

s_zms <- as(model.matrix( ~ 0 + main_site:sub_site, data = lwg_snipe), "Matrix")
s_zcms <- as(model.matrix(  ~ 0 + climate_district:main_site:sub_site, data = lwg_snipe), "Matrix")
s_zcoms <- as(model.matrix(  ~ 0 + country:main_site:sub_site, data = lwg_snipe), "Matrix")

lwg_snipe$mainsub <- as.factor(apply(s_zms, 1, function(x){names(x)[x == 1]}))
lwg_snipe$countymainsub <- as.factor(apply(s_zcms, 1, function(x){names(x)[x == 1]}))
lwg_snipe$countrymainsub <- as.factor(apply(s_zcoms, 1, function(x){names(x)[x == 1]}))

# Part 3: Data exploration prior to analysis ----
# In this script we select the variables that are used for further modelling

# Add UK shapefile and plot points. There are many ways of doing this but this approach produces a fairly simple
# plot which is easy on the eyes.

# Plot points together with UK map. UPDATE - Not necessary as we are not telling anything new in this plot. Look at earlier plot 
# for site size and annual count mapped for each individual species 

# ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
#  theme_void() + geom_point(data = lwg_lapwing, aes(long, lat), color = "red") 

# Check percentage of zeros 
#Lapwing

par(mfrow = c(2,2), mar = c(4,5,1,2), cex.lab = 1.5)
plot(table(lwg_lapwing$count_analysis), ylab = "Frequency", xlab = "Lapwing pairs" )
sum(lwg_lapwing$count_analysis == 0)#Number of zeros
100 * sum(lwg_lapwing$count_analysis == 0) / nrow(lwg_lapwing)  #% of zeros = 21.93% 

# Redshank

plot(table(lwg_redshank$count_analysis), ylab = "Frequency", xlab = "Redshank pairs" )
sum(lwg_redshank$count_analysis == 0)  #Number of zeros
100 * sum(lwg_redshank$count_analysis == 0) / nrow(lwg_redshank)  #% of zeros

# Curlew

plot(table(lwg_curlew$count_analysis), ylab = "Frequency", xlab = "Curlew pairs" )
sum(lwg_curlew$count_analysis == 0)  #Number of zeros
100 * sum(lwg_curlew$count_analysis == 0) / nrow(lwg_curlew)  #% of zeros

# Snipe

plot(table(lwg_snipe$count_analysis), ylab = "Frequency", xlab = "Snipe pairs" )
sum(lwg_snipe$count_analysis == 0)  #Number of zeros
100 * sum(lwg_snipe$count_analysis == 0) / nrow(lwg_snipe)  #% of zeros 


# Check for outliers
# With version 1 of the climate variables
MyVar <- c("year_since_acq", "area_of_lwg_ha", "groundfrost_winter",
           "groundfrost_spring" , "temp_spring", "temp_winter", "maxtemp_winter", "mintemp_winter", "maxtemp_spring", 
           "mintemp_spring", "rain_winter", "rain_spring","groundfrost_winter_lag" ,
           "groundfrost_spring_lag" , "temp_spring_lag", "temp_winter_lag", "maxtemp_winter_lag",
           "mintemp_winter_lag", "maxtemp_spring_lag", 
           "mintemp_spring_lag", "rain_winter_lag", "rain_spring_lag", "easting_km", "northing_km")

# With the shorter winter and delayed spring/summer climate variables
MyVar_alt <- c("year_since_acq", "area_of_lwg_ha", "groundfrost_winter_short",
               "groundfrost_spring_late" , "temp_spring_late", "temp_winter_short", "maxtemp_winter_short", "mintemp_winter_short", "maxtemp_spring_late", 
               "mintemp_spring_late", "rain_winter_short", "rain_spring_late","groundfrost_winter_short_lag" ,
               "groundfrost_spring_late_lag" , "temp_spring_late_lag", "temp_winter_short_lag", "maxtemp_winter_short_lag",
               "mintemp_winter_short_lag", "maxtemp_spring_late_lag", 
               "mintemp_spring_late_lag", "rain_winter_short_lag", "rain_spring_late_lag", "easting_km", "northing_km")

Mydotplot(lwg_lapwing[,MyVar]) # A few outliers in climate and 
Mydotplot(lwg_redshank[,MyVar]) # A few outliers in climate and 
Mydotplot(lwg_curlew[,MyVar]) # A few outliers in climate and 
Mydotplot(lwg_snipe[,MyVar]) # A few outliers in climate and 


Mydotplot(lwg_lapwing[,MyVar_alt]) # A few outliers in climate and 


# Outliers in reponse 

Mydotplot(lwg_lapwing[,"count_analysis"]) # not too many zeros but large variation in counts
Mydotplot(lwg_redshank[,"count_analysis"]) # alot of zeros. Lower counts than Lapwing
Mydotplot(lwg_curlew[,"count_analysis"]) # alot of zeros and low count values
Mydotplot(lwg_snipe[,"count_analysis"]) # alot of zeros and low count values

# Obs pr year
obs_year <- lwg_lapwing %>% 
  group_by(year) %>% 
  summarise(obs_pr_year = n())

# Examine relationships between the different covariates and counts ----

# Examination for Lapwing ----

par(mfrow = c(4,3), mar= c(4,4,2,2), cex.lab = 1.5)
# all temp variables
plot(x = lwg_lapwing$temp_spring_late,
     y = lwg_lapwing$count_analysis,
     xlab = "temp spring",
     ylab = "Counts")

plot(x = lwg_lapwing$temp_winter,
     y = lwg_lapwing$count_analysis,
     xlab = "temp winter",
     ylab = "Counts")

plot(x = lwg_lapwing$maxtemp_spring,
     y = lwg_lapwing$count_analysis,
     xlab = "maxtemp spring",
     ylab = "Counts")

plot(x = lwg_lapwing$maxtemp_winter,
     y = lwg_lapwing$count_analysis,
     xlab = "maxtemp winter",
     ylab = "Counts")

plot(x = lwg_lapwing$mintemp_spring,
     y = lwg_lapwing$count_analysis,
     xlab = "mintemp spring",
     ylab = "Counts")

plot(x = lwg_lapwing$mintemp_winter,
     y = lwg_lapwing$count_analysis,
     xlab = "mintemp winter",
     ylab = "Counts")

plot(x = lwg_lapwing$temp_spring_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged temp spring",
     ylab = "Counts")

plot(x = lwg_lapwing$temp_winter_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged temp winter",
     ylab = "Counts")

plot(x = lwg_lapwing$maxtemp_spring_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged maxtemp spring",
     ylab = "Counts")

plot(x = lwg_lapwing$maxtemp_winter_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged maxtemp winter",
     ylab = "Counts")

plot(x = lwg_lapwing$mintemp_spring_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged mintemp spring",
     ylab = "Counts")

plot(x = lwg_lapwing$mintemp_winter_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged mintemp winter",
     ylab = "Counts")

# rain and groundfrost
par(mfrow =c(4, 2), mar= c(4,4,2,2), cex.lab = 1.5)

plot(x = lwg_lapwing$rain_spring,
     y = lwg_lapwing$count_analysis,
     xlab = "rain spring",
     ylab = "Counts")

plot(x = lwg_lapwing$rain_winter,
     y = lwg_lapwing$count_analysis,
     xlab = "rain winter",
     ylab = "Counts")

plot(x = lwg_lapwing$rain_spring_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged rain spring",
     ylab = "Counts")

plot(x = lwg_lapwing$rain_winter_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged rain winter",
     ylab = "Counts")


plot(x = lwg_lapwing$groundfrost_spring,
     y = lwg_lapwing$count_analysis,
     xlab = "groundfrost spring",
     ylab = "Counts")

plot(x = lwg_lapwing$groundfrost_winter,
     y = lwg_lapwing$count_analysis,
     xlab = "groundfrost winter",
     ylab = "Counts")

plot(x = lwg_lapwing$groundfrost_spring_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged groundfrost spring",
     ylab = "Counts")

plot(x = lwg_lapwing$groundfrost_winter_lag,
     y = lwg_lapwing$count_analysis,
     xlab = "lagged groundfrost winter",
     ylab = "Counts")

# Easting and northing
par(mfrow = c(2,1))

plot(x = lwg_lapwing$easting_km,
     y =  lwg_lapwing$count_analysis,
     xlab = "Easting km",
     ylab = "Counts")

plot(x = lwg_lapwing$northing_km,
     y =  lwg_lapwing$count_analysis,
     xlab = "Northing km",
     ylab = "Counts")

# Site area in ha - not as strong relationship as we would expect

par(mfrow = c(1,1))

plot(x = lwg_lapwing$area_of_lwg_ha,
     y =  lwg_lapwing$count_analysis,
     xlab = "Size ha",
     ylab = "Counts")

# Add a smoother and look at patterns - Def increasing but not linearly. Maybe explained by another 
# variable such as habitat type as pr density plots in different habitat types (Ausden 2019)?
lwg_lapwing %>% 
  ggplot(., aes(x = area_of_lwg_ha, y = count_analysis)) +
  geom_smooth() +
  geom_point()
#facet_wrap(~year, scales = "free")

# Counter # UPDATE not used as it makes interpretation of the priors to complicated

plot(x = lwg_lapwing$counter,
     y =  lwg_lapwing$count_analysis,
     xlab = "Years since last change in adj water control structures",
     ylab = "Counts")

# Probably easier too inspect with a smoother

lwg_lapwing %>% 
  ggplot(., aes(x = counter, y = count_analysis)) +
  geom_smooth() +
  geom_point()

# What about years since acq? 

lwg_lapwing %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

# In base R 
plot(x = lwg_lapwing$year_since_acq,
     y =  lwg_lapwing$count_analysis,
     xlab = "Years since asquisition",
     ylab = "Counts")

# Similar to years since acq but using the first obs as the first year

lwg_lapwing %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

# Check them beside each other

plot1 <- lwg_lapwing %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

plot2 <- lwg_lapwing %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

plot1 | plot2 # Pretty similar - How about when facet_wrap by habitat type?

plot3 <- lwg_lapwing %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot4 <- lwg_lapwing %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot3 | plot4

# Fit gams using mgcv and see how they behave - Same as using ggplot smoothers except ggplot fits grassland as linear 
# Def a clear difference between habitats. Arable and grass+arable show a much stronger increase in first year

m <- gam(count_analysis ~ habitat_when_acquired + s(first_year, by = habitat_when_acquired), data = lwg_lapwing)
summary(m) # Explains more than 8% of variation
plot(m, shade = TRUE, pages = 1, scale = 0)

# Year. Here, 1994 is set as year 1, 1995 as 1 and so one

plot(x = lwg_lapwing$year_cum,
     y =  lwg_lapwing$count_analysis,
     xlab = "Year",
     ylab = "Counts")

# ggplot - No real trend over time

lwg_lapwing %>%
  ggplot(., aes(x = year_cum, y = count_analysis)) +
  geom_smooth() +
  geom_point()

# Categoricals

boxplot(count_analysis ~ habitat_when_acquired, 
        xlab = "Habitat upon acquisition",
        ylab = "Counts",
        data = lwg_lapwing)

boxplot(count_analysis ~ foxes_killed, 
        xlab = "Fox control",
        ylab = "Counts",
        data = lwg_lapwing)

boxplot(count_analysis ~ minks_killed, 
        xlab = "Mink control",
        ylab = "Counts",
        data = lwg_lapwing)

boxplot(count_analysis ~ crows_killed, 
        xlab = "Crow control",
        ylab = "Counts",
        data = lwg_lapwing)

boxplot(count_analysis ~ fenced, 
        xlab = "predator fence",
        ylab = "Counts",
        data = lwg_lapwing)

boxplot(count_analysis ~ foot_drain_analysis, 
        xlab = "Foot drain improvements",
        ylab = "Counts",
        data = lwg_lapwing)

boxplot(count_analysis ~ excavation_pond_scrape_analysis, 
        xlab = "Excavation improvements",
        ylab = "Counts",
        data = lwg_lapwing)

boxplot(count_analysis ~ water_adj_analysis, 
        xlab = "Adjustable water control structure improvements",
        ylab = "Counts",
        data = lwg_lapwing)

boxplot(count_analysis ~ water_fix_analysis, 
        xlab = "Fixed water control structure improvements",
        ylab = "Counts",
        data = lwg_lapwing)
# Collinearity
# Selecting which climate variables to include
Myclim <- c("groundfrost_winter",
            "groundfrost_spring" , "temp_spring", "temp_winter", "maxtemp_winter", "mintemp_winter", "maxtemp_spring", 
            "mintemp_spring", "rain_winter", "rain_spring")

Mypairs(lwg_lapwing[, Myclim]) # Temp and rain winter and spring are not too correlated so going with those

temp_rain <- c("temp_spring", "temp_winter", "rain_winter", "rain_spring") 

Mypairs(lwg_lapwing [, temp_rain])

Myclim_lag <- c("temp_spring", "temp_winter", "rain_winter", "rain_spring", "temp_spring_lag", "temp_winter_lag",
                "rain_winter_lag", "rain_spring_lag")

Mypairs(lwg_lapwing[, Myclim_lag]) # Not too correlated with the lagged variables

# Check whether the shorter winter and delayed spring/summer climate variables are better

Myclim_alt <- c("temp_spring_late", "temp_winter_short","rain_winter_short", "rain_spring_late")

Mypairs(lwg_lapwing[, Myclim_alt]) # Some minor differences but let's stick to the original 

Myclim_alt_lag <- c("temp_spring_late", "temp_winter_short","rain_winter_short", "rain_spring_late",
                    "temp_spring_late_lag", "temp_winter_short_lag", "rain_winter_short_lag", "rain_spring_late_lag")

Mypairs(lwg_lapwing[, Myclim_alt_lag])

# Check for correlation with the remaining variables
MyVar_con <- c("year_since_acq", "area_of_lwg_ha", "water_fix_analysis", "water_adj_analysis",
               "foot_drain_analysis", "excavation_pond_scrape_analysis")

Mypairs(lwg_lapwing[, MyVar_con]) # Pretty correlated but no problems in the VIF values


corvif(lwg_lapwing[, Myclim_alt_lag])

# Combine climate and conservation variables

MyVar <- c("year_since_acq", "area_of_lwg_ha", "water_fix_analysis", "water_adj_analysis",
           "foot_drain_analysis", "excavation_pond_scrape_analysis", "temp_spring", "temp_winter", "rain_winter", "rain_spring") # Final fixed vars

Mypairs(lwg_lapwing[, MyVar])
corvif(lwg_lapwing[,MyVar])

# Examination for redshank ----

par(mfrow = c(4,3), mar= c(4,4,2,2), cex.lab = 1.5)
# all temp variables
plot(x = lwg_redshank$temp_spring_late,
     y = lwg_redshank$count_analysis,
     xlab = "temp spring",
     ylab = "Counts")

plot(x = lwg_redshank$temp_winter,
     y = lwg_redshank$count_analysis,
     xlab = "temp winter",
     ylab = "Counts")

plot(x = lwg_redshank$maxtemp_spring,
     y = lwg_redshank$count_analysis,
     xlab = "maxtemp spring",
     ylab = "Counts")

plot(x = lwg_redshank$maxtemp_winter,
     y = lwg_redshank$count_analysis,
     xlab = "maxtemp winter",
     ylab = "Counts")

plot(x = lwg_redshank$mintemp_spring,
     y = lwg_redshank$count_analysis,
     xlab = "mintemp spring",
     ylab = "Counts")

plot(x = lwg_redshank$mintemp_winter,
     y = lwg_redshank$count_analysis,
     xlab = "mintemp winter",
     ylab = "Counts")

plot(x = lwg_redshank$temp_spring_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged temp spring",
     ylab = "Counts")

plot(x = lwg_redshank$temp_winter_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged temp winter",
     ylab = "Counts")

plot(x = lwg_redshank$maxtemp_spring_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged maxtemp spring",
     ylab = "Counts")

plot(x = lwg_redshank$maxtemp_winter_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged maxtemp winter",
     ylab = "Counts")

plot(x = lwg_redshank$mintemp_spring_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged mintemp spring",
     ylab = "Counts")

plot(x = lwg_redshank$mintemp_winter_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged mintemp winter",
     ylab = "Counts")

# rain and groundfrost
par(mfrow =c(4, 2), mar= c(4,4,2,2), cex.lab = 1.5)

plot(x = lwg_redshank$rain_spring,
     y = lwg_redshank$count_analysis,
     xlab = "rain spring",
     ylab = "Counts")

plot(x = lwg_redshank$rain_winter,
     y = lwg_redshank$count_analysis,
     xlab = "rain winter",
     ylab = "Counts")

plot(x = lwg_redshank$rain_spring_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged rain spring",
     ylab = "Counts")

plot(x = lwg_redshank$rain_winter_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged rain winter",
     ylab = "Counts")


plot(x = lwg_redshank$groundfrost_spring,
     y = lwg_redshank$count_analysis,
     xlab = "groundfrost spring",
     ylab = "Counts")

plot(x = lwg_redshank$groundfrost_winter,
     y = lwg_redshank$count_analysis,
     xlab = "groundfrost winter",
     ylab = "Counts")

plot(x = lwg_redshank$groundfrost_spring_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged groundfrost spring",
     ylab = "Counts")

plot(x = lwg_redshank$groundfrost_winter_lag,
     y = lwg_redshank$count_analysis,
     xlab = "lagged groundfrost winter",
     ylab = "Counts")

# Easting and northing
par(mfrow = c(2,1))

plot(x = lwg_redshank$easting_km,
     y =  lwg_redshank$count_analysis,
     xlab = "Easting km",
     ylab = "Counts")

plot(x = lwg_redshank$northing_km,
     y =  lwg_redshank$count_analysis,
     xlab = "Northing km",
     ylab = "Counts") # Def highest counts in eastern England. Fits well with distribution. 

# Site area in ha. More curvy than Lapwing 

par(mfrow = c(1,1))

plot(x = lwg_redshank$area_of_lwg_ha,
     y =  lwg_redshank$count_analysis,
     xlab = "Size ha",
     ylab = "Counts")

# Add a smoother and look at patterns - Def increasing but not linearly. Maybe explained by another 
# variable such as habitat type as pr density plots in different habitat types (Ausden 2019)?
lwg_redshank %>% 
  ggplot(., aes(x = area_of_lwg_ha, y = count_analysis)) +
  geom_smooth() +
  geom_point()
#facet_wrap(~year, scales = "free")

# Counter # UPDATE not used as it makes interpretation of the priors to complicated

#plot(x = lwg_redshank$counter,
#     y =  lwg_redshank$count_analysis,
#     xlab = "Years since last change in adj water control structures",
#     ylab = "Counts")

# Probably easier too inspect with a smoother

#lwg_redshank %>% 
#  ggplot(., aes(x = counter, y = count_analysis)) +
#  geom_smooth() +
#  geom_point()

# What about years since acq? Increasing but looks to be somewhat similar to Lapwing with increasing and 
# then decreasing later on.

lwg_redshank %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

# In base R 
plot(x = lwg_redshank$year_since_acq,
     y =  lwg_redshank$count_analysis,
     xlab = "Years since asquisition",
     ylab = "Counts")

# Similar to years since acq but using the first obs as the first year. Same pattern as years since acq

lwg_redshank %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

# Check them beside each other

plot1 <- lwg_redshank %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

plot2 <- lwg_redshank %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

plot1 | plot2 # Pretty similar - How about when facet_wrap by habitat type?

plot3 <- lwg_redshank %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot4 <- lwg_redshank %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot3 | plot4 # Shows the same as Lapwing. Strongest increase in arable but also some in grassland.

# Fit gams using mgcv and see how they behave - Same as using ggplot smoothers except ggplot fits grassland as linear 
# Def a clear difference between habitats. Arable and grass+arable show a much stronger increase in first year

m <- gam(count_analysis ~ habitat_when_acquired + s(first_year, by = habitat_when_acquired), data = lwg_redshank)
summary(m) # Explains 7.87% of variation
plot(m, shade = TRUE, pages = 1, scale = 0)

# Year. Here, 1994 is set as year 1, 1995 as 1 and so one

plot(x = lwg_redshank$year_cum,
     y =  lwg_redshank$count_analysis,
     xlab = "Year",
     ylab = "Counts")

# ggplot - slight increase but cannot be considered a trend as more sites enter through time.

lwg_redshank %>%
  ggplot(., aes(x = year_cum, y = count_analysis)) +
  geom_smooth() +
  geom_point()

# Categoricals

boxplot(count_analysis ~ habitat_when_acquired, 
        xlab = "Habitat upon acquisition",
        ylab = "Counts",
        data = lwg_redshank)

boxplot(count_analysis ~ foxes_killed, 
        xlab = "Fox control",
        ylab = "Counts",
        data = lwg_redshank)

boxplot(count_analysis ~ minks_killed, 
        xlab = "Mink control",
        ylab = "Counts",
        data = lwg_redshank)

boxplot(count_analysis ~ crows_killed, 
        xlab = "Crow control",
        ylab = "Counts",
        data = lwg_redshank)

boxplot(count_analysis ~ fenced, 
        xlab = "predator fence",
        ylab = "Counts",
        data = lwg_redshank)

boxplot(count_analysis ~ foot_drain_analysis, 
        xlab = "Foot drain improvements",
        ylab = "Counts",
        data = lwg_redshank)

boxplot(count_analysis ~ excavation_pond_scrape_analysis, 
        xlab = "Excavation improvements",
        ylab = "Counts",
        data = lwg_redshank)

boxplot(count_analysis ~ water_adj_analysis, 
        xlab = "Adjustable water control structure improvements",
        ylab = "Counts",
        data = lwg_redshank)

boxplot(count_analysis ~ water_fix_analysis, 
        xlab = "Fixed water control structure improvements",
        ylab = "Counts",
        data = lwg_redshank)


# Collinearity
# Selecting which climate variables to include
Myclim <- c("groundfrost_winter",
            "groundfrost_spring" , "temp_spring", "temp_winter", "maxtemp_winter", "mintemp_winter", "maxtemp_spring", 
            "mintemp_spring", "rain_winter", "rain_spring")

Mypairs(lwg_redshank[, Myclim]) # Temp and rain winter and spring are not too correlated so going with those

temp_rain <- c("temp_spring", "temp_winter", "rain_winter", "rain_spring") 

Mypairs(lwg_redshank [, temp_rain])

# Check for correlation with the remaining variables
MyVar_con <- c("year_since_acq", "area_of_lwg_ha", "water_fix_analysis", "water_adj_analysis",
               "foot_drain_analysis", "excavation_pond_scrape_analysis")

Mypairs(lwg_redshank[, MyVar_con]) # Pretty correlated but no problems in the VIF values


corvif(lwg_redshank[, MyVar_con])

# Combine climate and conservation variables

MyVar <- c("year_since_acq", "area_of_lwg_ha", "water_fix_analysis", "water_adj_analysis",
           "foot_drain_analysis", "excavation_pond_scrape_analysis", "temp_spring", "temp_winter", "rain_winter", "rain_spring") # Final fixed vars

Mypairs(lwg_redshank[, MyVar]) # Fixed and adjustable are honestly a bit too correlated
corvif(lwg_redshank[,MyVar]) # All under 3

# Examination for Curlew ----

par(mfrow = c(4,3), mar= c(4,4,2,2), cex.lab = 1.5)
# all temp variables. Curlew is not as abundant in warmer area because of its northern distribution.
plot(x = lwg_curlew$temp_spring_late,
     y = lwg_curlew$count_analysis,
     xlab = "temp spring",
     ylab = "Counts")

plot(x = lwg_curlew$temp_winter,
     y = lwg_curlew$count_analysis,
     xlab = "temp winter",
     ylab = "Counts")

plot(x = lwg_curlew$maxtemp_spring,
     y = lwg_curlew$count_analysis,
     xlab = "maxtemp spring",
     ylab = "Counts")

plot(x = lwg_curlew$maxtemp_winter,
     y = lwg_curlew$count_analysis,
     xlab = "maxtemp winter",
     ylab = "Counts")

plot(x = lwg_curlew$mintemp_spring,
     y = lwg_curlew$count_analysis,
     xlab = "mintemp spring",
     ylab = "Counts")

plot(x = lwg_curlew$mintemp_winter,
     y = lwg_curlew$count_analysis,
     xlab = "mintemp winter",
     ylab = "Counts")

plot(x = lwg_curlew$temp_spring_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged temp spring",
     ylab = "Counts")

plot(x = lwg_curlew$temp_winter_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged temp winter",
     ylab = "Counts")

plot(x = lwg_curlew$maxtemp_spring_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged maxtemp spring",
     ylab = "Counts")

plot(x = lwg_curlew$maxtemp_winter_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged maxtemp winter",
     ylab = "Counts")

plot(x = lwg_curlew$mintemp_spring_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged mintemp spring",
     ylab = "Counts")

plot(x = lwg_curlew$mintemp_winter_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged mintemp winter",
     ylab = "Counts")

# rain and groundfrost. Looks alright but no positive obs in low precip
par(mfrow =c(4, 2), mar= c(4,4,2,2), cex.lab = 1.5)

plot(x = lwg_curlew$rain_spring,
     y = lwg_curlew$count_analysis,
     xlab = "rain spring",
     ylab = "Counts")

plot(x = lwg_curlew$rain_winter,
     y = lwg_curlew$count_analysis,
     xlab = "rain winter",
     ylab = "Counts")

plot(x = lwg_curlew$rain_spring_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged rain spring",
     ylab = "Counts")

plot(x = lwg_curlew$rain_winter_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged rain winter",
     ylab = "Counts")


plot(x = lwg_curlew$groundfrost_spring,
     y = lwg_curlew$count_analysis,
     xlab = "groundfrost spring",
     ylab = "Counts")

plot(x = lwg_curlew$groundfrost_winter,
     y = lwg_curlew$count_analysis,
     xlab = "groundfrost winter",
     ylab = "Counts")

plot(x = lwg_curlew$groundfrost_spring_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged groundfrost spring",
     ylab = "Counts")

plot(x = lwg_curlew$groundfrost_winter_lag,
     y = lwg_curlew$count_analysis,
     xlab = "lagged groundfrost winter",
     ylab = "Counts")

# Easting and northing. Northern distribution is very clear. Basically not present on any English sites 
par(mfrow = c(2,1))

plot(x = lwg_curlew$easting_km,
     y =  lwg_curlew$count_analysis,
     xlab = "Easting km",
     ylab = "Counts")

plot(x = lwg_curlew$northing_km,
     y =  lwg_curlew$count_analysis,
     xlab = "Northing km",
     ylab = "Counts")  

# Site area in ha. Hard to see as pattern is distorted by the many zero obs.

par(mfrow = c(1,1))

plot(x = lwg_curlew$area_of_lwg_ha,
     y =  lwg_curlew$count_analysis,
     xlab = "Size ha",
     ylab = "Counts")

# Try exluding purely zero count sites. Doesn't really look like there is a pattern here.
curlew_positive <- lwg_curlew %>% 
  group_by(sub_site) %>% 
  summarise(site_count = sum(count_analysis)) %>% 
  filter(site_count > 0 )

curlew_pos <- left_join(curlew_positive, lwg_curlew, by = "sub_site")

curlew_pos %>% 
  ggplot(., aes(x = area_of_lwg_ha, y = count_analysis)) +
  geom_point() +
  geom_smooth()


# Counter # UPDATE not used as it makes interpretation of the posteriors to complicated

#plot(x = lwg_curlew$counter,
#     y =  lwg_curlew$count_analysis,
#     xlab = "Years since last change in adj water control structures",
#     ylab = "Counts")

# Probably easier too inspect with a smoother

#lwg_curlew %>% 
#  ggplot(., aes(x = counter, y = count_analysis)) +
#  geom_smooth() +
#  geom_point()

# What about years since acq? Increasing but looks to be somewhat similar to Lapwing with increasing and 
# then decreasing later on.

lwg_curlew %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

# In base R 
plot(x = lwg_curlew$year_since_acq,
     y =  lwg_curlew$count_analysis,
     xlab = "Years since asquisition",
     ylab = "Counts")

# Similar to years since acq but using the first obs as the first year. Same pattern as years since acq

lwg_curlew %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

# Check them beside each other

plot1 <- lwg_curlew %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

plot2 <- lwg_curlew %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

plot1 | plot2 # Pretty similar - How about when facet_wrap by habitat type?

plot3 <- lwg_curlew %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot4 <- lwg_curlew %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot3 | plot4 # Not really useful. Too few positive obs. 
# Interesting that they only occur on former grasslands though

# Fit gams using mgcv and see how they behave - Same as using ggplot smoothers except ggplot fits grassland as linear 
# Def a clear difference between habitats. Arable and grass+arable show a much stronger increase in first year

m <- gam(count_analysis ~ habitat_when_acquired + s(first_year, by = habitat_when_acquired), data = lwg_curlew)
summary(m) # Explains 7.87% of variation
plot(m, shade = TRUE, pages = 1, scale = 0)

# Year. Here, 1994 is set as year 1, 1995 as 1 and so one

plot(x = lwg_curlew$year_cum,
     y =  lwg_curlew$count_analysis,
     xlab = "Year",
     ylab = "Counts")

# ggplot - decrease but cannot be considered a trend as more sites enter through time.

lwg_curlew %>%
  ggplot(., aes(x = year_cum, y = count_analysis, color = habitat_when_acquired)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~sub_site)

# To plot each sub site individually and show habitat ehn acquired use
#lwg_curlew %>%
#  ggplot(., aes(x = year_cum, y = count_analysis, color = habitat_when_acquired)) +
#  geom_smooth() +
#  geom_point() +
#  facet_wrap(~sub_site)

# Categoricals. They don't really show as categoricals here

boxplot(count_analysis ~ habitat_when_acquired, 
        xlab = "Habitat upon acquisition",
        ylab = "Counts",
        data = lwg_curlew)

boxplot(count_analysis ~ foxes_killed, 
        xlab = "Fox control",
        ylab = "Counts",
        data = lwg_curlew)

boxplot(count_analysis ~ minks_killed, 
        xlab = "Mink control",
        ylab = "Counts",
        data = lwg_curlew)

boxplot(count_analysis ~ crows_killed, 
        xlab = "Crow control",
        ylab = "Counts",
        data = lwg_curlew)

boxplot(count_analysis ~ fenced, 
        xlab = "predator fence",
        ylab = "Counts",
        data = lwg_curlew)

boxplot(count_analysis ~ foot_drain_analysis, 
        xlab = "Foot drain improvements",
        ylab = "Counts",
        data = lwg_curlew)

boxplot(count_analysis ~ excavation_pond_scrape_analysis, 
        xlab = "Excavation improvements",
        ylab = "Counts",
        data = lwg_curlew)

boxplot(count_analysis ~ water_adj_analysis, 
        xlab = "Adjustable water control structure improvements",
        ylab = "Counts",
        data = lwg_curlew)

boxplot(count_analysis ~ water_fix_analysis, 
        xlab = "Fixed water control structure improvements",
        ylab = "Counts",
        data = lwg_curlew)


# Collinearity
# Selecting which climate variables to include - Not necessary as same sites are used across all four species
Myclim <- c("groundfrost_winter",
            "groundfrost_spring" , "temp_spring", "temp_winter", "maxtemp_winter", "mintemp_winter", "maxtemp_spring", 
            "mintemp_spring", "rain_winter", "rain_spring")

Mypairs(lwg_curlew[, Myclim]) # Temp and rain winter and spring are not too correlated so going with those

temp_rain <- c("temp_spring", "temp_winter", "rain_winter", "rain_spring") 

Mypairs(lwg_curlew [, temp_rain])

# Check for correlation with the remaining variables
MyVar_con <- c("year_since_acq", "area_of_lwg_ha", "water_fix_analysis", "water_adj_analysis",
               "foot_drain_analysis", "excavation_pond_scrape_analysis")

Mypairs(lwg_curlew[, MyVar_con]) # Pretty correlated but no problems in the VIF values


corvif(lwg_curlew[, MyVar_con])

# Combine climate and conservation variables

MyVar <- c("year_since_acq", "area_of_lwg_ha", "water_fix_analysis", "water_adj_analysis",
           "foot_drain_analysis", "excavation_pond_scrape_analysis", "temp_spring", "temp_winter", "rain_winter", "rain_spring") # Final fixed vars

Mypairs(lwg_curlew[, MyVar]) # Fixed and adjustable are honestly a bit too correlated
corvif(lwg_curlew[,MyVar]) # All under 3

# Examination for snipe ----

par(mfrow = c(4,3), mar= c(4,4,2,2), cex.lab = 1.5)
# all temp variables. Looks okay for the normal temp variables although few outliers for low and high temps.
plot(x = lwg_snipe$temp_spring_late,
     y = lwg_snipe$count_analysis,
     xlab = "temp spring",
     ylab = "Counts")

plot(x = lwg_snipe$temp_winter,
     y = lwg_snipe$count_analysis,
     xlab = "temp winter",
     ylab = "Counts")

plot(x = lwg_snipe$maxtemp_spring,
     y = lwg_snipe$count_analysis,
     xlab = "maxtemp spring",
     ylab = "Counts")

plot(x = lwg_snipe$maxtemp_winter,
     y = lwg_snipe$count_analysis,
     xlab = "maxtemp winter",
     ylab = "Counts")

plot(x = lwg_snipe$mintemp_spring,
     y = lwg_snipe$count_analysis,
     xlab = "mintemp spring",
     ylab = "Counts")

plot(x = lwg_snipe$mintemp_winter,
     y = lwg_snipe$count_analysis,
     xlab = "mintemp winter",
     ylab = "Counts")

plot(x = lwg_snipe$temp_spring_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged temp spring",
     ylab = "Counts")

plot(x = lwg_snipe$temp_winter_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged temp winter",
     ylab = "Counts")

plot(x = lwg_snipe$maxtemp_spring_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged maxtemp spring",
     ylab = "Counts")

plot(x = lwg_snipe$maxtemp_winter_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged maxtemp winter",
     ylab = "Counts")

plot(x = lwg_snipe$mintemp_spring_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged mintemp spring",
     ylab = "Counts")

plot(x = lwg_snipe$mintemp_winter_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged mintemp winter",
     ylab = "Counts")

# rain and groundfrost. Rain looks fine. 
par(mfrow =c(4, 2), mar= c(4,4,2,2), cex.lab = 1.5)

plot(x = lwg_snipe$rain_spring,
     y = lwg_snipe$count_analysis,
     xlab = "rain spring",
     ylab = "Counts")

plot(x = lwg_snipe$rain_winter,
     y = lwg_snipe$count_analysis,
     xlab = "rain winter",
     ylab = "Counts")

plot(x = lwg_snipe$rain_spring_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged rain spring",
     ylab = "Counts")

plot(x = lwg_snipe$rain_winter_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged rain winter",
     ylab = "Counts")


plot(x = lwg_snipe$groundfrost_spring,
     y = lwg_snipe$count_analysis,
     xlab = "groundfrost spring",
     ylab = "Counts")

plot(x = lwg_snipe$groundfrost_winter,
     y = lwg_snipe$count_analysis,
     xlab = "groundfrost winter",
     ylab = "Counts")

plot(x = lwg_snipe$groundfrost_spring_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged groundfrost spring",
     ylab = "Counts")

plot(x = lwg_snipe$groundfrost_winter_lag,
     y = lwg_snipe$count_analysis,
     xlab = "lagged groundfrost winter",
     ylab = "Counts")

# Easting and northing. Distribution is not clear compared with Curlew.
par(mfrow = c(2,1))

plot(x = lwg_snipe$easting_km,
     y =  lwg_snipe$count_analysis,
     xlab = "Easting km",
     ylab = "Counts")

plot(x = lwg_snipe$northing_km,
     y =  lwg_snipe$count_analysis,
     xlab = "Northing km",
     ylab = "Counts")  

# Site area in ha. Pattern but still a bit distorted by ll the zeros.

lwg_snipe %>%
  ggplot(., aes(x = area_of_lwg_ha, y = count_analysis)) +
  geom_smooth() +
  geom_point()

# Try excluding purely zero count sites. Looks like there is a pattern when using sites that 
# contain at least one positve count
snipe_positive <- lwg_snipe %>% 
  group_by(sub_site) %>% 
  summarise(site_count = sum(count_analysis)) %>% 
  filter(site_count > 0 )

snipe_pos <- left_join(snipe_positive, lwg_snipe, by = "sub_site")

snipe_pos %>% 
  ggplot(., aes(x = area_of_lwg_ha, y = count_analysis)) +
  geom_point() +
  geom_smooth()


# Counter # UPDATE not used as it makes interpretation of the priors to complicated

#plot(x = lwg_snipe$counter,
#     y =  lwg_snipe$count_analysis,
#     xlab = "Years since last change in adj water control structures",
#     ylab = "Counts")

# Probably easier too inspect with a smoother

#lwg_snipe %>% 
#  ggplot(., aes(x = counter, y = count_analysis)) +
#  geom_smooth() +
#  geom_point()

# What about years since acq? Increasing slightly

lwg_snipe %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

# In base R 
plot(x = lwg_snipe$year_since_acq,
     y =  lwg_snipe$count_analysis,
     xlab = "Years since asquisition",
     ylab = "Counts")

# Similar to years since acq but using the first obs as the first year. Same pattern as years since acq

lwg_snipe %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

# Check them beside each other

plot1 <- lwg_snipe %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

plot2 <- lwg_snipe %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point()

plot1 | plot2 # Pretty similar - How about when facet_wrap by habitat type?

plot3 <- lwg_snipe %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot4 <- lwg_snipe %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot3 | plot4 # Arable land seems to be most increasing

# What about using only positive sites?


plot3 <- snipe_pos %>%
  ggplot(., aes(x = year_since_acq, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot4 <- snipe_pos %>%
  ggplot(., aes(x = first_year, y = count_analysis)) +
  geom_smooth(method = "gam") +
  geom_point() +
  facet_wrap(~habitat_when_acquired, scales = "free")

plot3 | plot4 # Same

# Fit gams using mgcv and see how they behave - Def a clear difference between habitats. 
# Arable shows a much stronger increase in first year

m <- gam(count_analysis ~ habitat_when_acquired + s(first_year, by = habitat_when_acquired), data = lwg_snipe)
summary(m) # Explains 7.87% of variation
plot(m, shade = TRUE, pages = 1, scale = 0)

# Year. Here, 1994 is set as year 1, 1995 as 1 and so one

plot(x = lwg_snipe$year_cum,
     y =  lwg_snipe$count_analysis,
     xlab = "Year",
     ylab = "Counts")

# ggplot - decrease but cannot be considered a trend as more sites enter through time.

lwg_snipe %>%
  ggplot(., aes(x = year_cum, y = count_analysis)) +
  geom_smooth() +
  geom_point()

# look at each site seperat. Looks fine.
lwg_snipe %>%  
  ggplot(., aes(x = year_cum, y = count_analysis, colour = habitat_when_acquired)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~sub_site)

# Categoricals. They don't really show as categoricals here

boxplot(count_analysis ~ habitat_when_acquired, 
        xlab = "Habitat upon acquisition",
        ylab = "Counts",
        data = lwg_snipe)

boxplot(count_analysis ~ foxes_killed, 
        xlab = "Fox control",
        ylab = "Counts",
        data = lwg_snipe)

boxplot(count_analysis ~ minks_killed, 
        xlab = "Mink control",
        ylab = "Counts",
        data = lwg_snipe)

boxplot(count_analysis ~ crows_killed, 
        xlab = "Crow control",
        ylab = "Counts",
        data = lwg_snipe)

boxplot(count_analysis ~ fenced, 
        xlab = "predator fence",
        ylab = "Counts",
        data = lwg_snipe)

boxplot(count_analysis ~ foot_drain_analysis, 
        xlab = "Foot drain improvements",
        ylab = "Counts",
        data = lwg_snipe)

boxplot(count_analysis ~ excavation_pond_scrape_analysis, 
        xlab = "Excavation improvements",
        ylab = "Counts",
        data = lwg_snipe)

boxplot(count_analysis ~ water_adj_analysis, 
        xlab = "Adjustable water control structure improvements",
        ylab = "Counts",
        data = lwg_snipe)

boxplot(count_analysis ~ water_fix_analysis, 
        xlab = "Fixed water control structure improvements",
        ylab = "Counts",
        data = lwg_snipe)


# Collinearity
# Selecting which climate variables to include - Not necessary as same sites are used across all four species
Myclim <- c("groundfrost_winter",
            "groundfrost_spring" , "temp_spring", "temp_winter", "maxtemp_winter", "mintemp_winter", "maxtemp_spring", 
            "mintemp_spring", "rain_winter", "rain_spring")

Mypairs(lwg_snipe[, Myclim]) # Temp and rain winter and spring are not too correlated so going with those

temp_rain <- c("temp_spring", "temp_winter", "rain_winter", "rain_spring") 

Mypairs(lwg_snipe [, temp_rain])

# Check for correlation with the remaining variables
MyVar_con <- c("year_since_acq", "area_of_lwg_ha", "water_fix_analysis", "water_adj_analysis",
               "foot_drain_analysis", "excavation_pond_scrape_analysis")

Mypairs(lwg_snipe[, MyVar_con]) # Pretty correlated but no problems in the VIF values


corvif(lwg_snipe[, MyVar_con])

# Combine climate and conservation variables

MyVar <- c("year_since_acq", "area_of_lwg_ha", "water_fix_analysis", "water_adj_analysis",
           "foot_drain_analysis", "excavation_pond_scrape_analysis", "temp_spring", "temp_winter", "rain_winter", "rain_spring") # Final fixed vars

Mypairs(lwg_snipe[, MyVar]) # Fixed and adjustable are honestly a bit too correlated
corvif(lwg_snipe[,MyVar]) # All under 3


# Part 4: Model selection ----
#
# Skipping this part in the final script. Code is messy and I changed variable names so requires some cleaning to run properly.
# Instead just use the parameter estimates from the best models and validate model assumptions
#

# Priors ----

hyper_rw = list(theta = list(prior='pc prec', param=c(1,0.01))) # Weak prior from https://inla.r-inla-download.org/r-inla.org/doc/latent/rw1.pdf
hyper_iid = hyper.iid = list(prec = list(prior='loggamma', param=c(0.1,0.5))) # From Carroll et al 2015 "Comparing INLA and openBUGS for hierarchical poisson modelling in disease mapping"
control.fixed1 = list(mean.intercept=0, mean = 0,
                      prec = 0.01, prec.intercept = 0.01) # Not used atm

# First we have to create a spatial mesh for the spatial random effect
# I don't think the locations should be different between the study species but we check regardless
# Update. There seems to be minor differences but I don't think it affects the mesh

Loc_lap <- cbind(lwg_lapwing$easting_km, lwg_lapwing$northing_km) 


Loc_red <- cbind(lwg_redshank$easting_km, lwg_redshank$northing_km) 


Loc_cur <- cbind(lwg_curlew$easting_km, lwg_curlew$northing_km) 


Loc_sni <- cbind(lwg_snipe$easting_km, lwg_snipe$northing_km) 

# Lapwing
d_lap <- dist(Loc_lap)

d_red <- dist(Loc_red)


par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)
hist(d_lap, 
     freq = TRUE,
     main = "", 
     xlab = "Distance between sites (km)",
     ylab = "Frequency")

plot(x = sort(d_lap), 
     y = (1:length(d_lap))/length(d_lap), 
     type = "l",
     xlab = "Distance between sites (km)",
     ylab = "Cumulative proportion")

hist(d_red, 
     freq = TRUE,
     main = "", 
     xlab = "Distance between sites (km)",
     ylab = "Frequency")

plot(x = sort(d_red), 
     y = (1:length(d_red))/length(d_red), 
     type = "l",
     xlab = "Distance between sites (km)",
     ylab = "Cumulative proportion")

# Plot points together with UK map 

ggplot() + geom_polygon(data = UK_shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)+
  geom_point(data = lwg_reserve, aes(long, lat, color = area_of_lwg_ha), size = 3) + 
  scale_color_viridis_c(option = "plasma", direction = -1) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 16, face = "bold")) +
  theme_void() +
  guides(size = FALSE) + 
  labs(colour = "Site area ha")



# Create mesh

# Range at which spatial dependency diminishes is set as 50 km
rangeguess <- 50
maxedge <- rangeguess /5 # recommendations from haakonbakka defining maxedge as fifth of best guess of range

# Normal meshes created using location of points
mesh_1 <- inla.mesh.2d(Loc_lap, 
                       max.edge = c(1, 5)*maxedge, 
                       cutoff = maxedge/5) # Use this one for making the script run and adjust after
mesh_2 <- inla.mesh.2d(Loc_lap, max.edge = c(30, 30))
mesh_3 <- inla.mesh.2d(Loc_lap, max.edge = c(30, 10))
mesh_4 <- inla.mesh.2d(Loc_lap, max.edge = c(10, 30))
mesh_5 <- inla.mesh.2d(Loc_lap, boundary = UK_shape, max.edge = c(1, 5)*maxedge, 
                       cutoff = maxedge/5)

# Specify nonconvex hull from points and create a slightly different mesh 
convhull <- inla.nonconvex.hull(Loc_lap)  
mesh_6 <- inla.mesh.2d(boundary = convhull,
                       max.edge = c(1, 5)*maxedge, 
                       cutoff = maxedge/5)


# PLot and inspect
par(mfrow = c(2,3))
plot(mesh_1)  
points(Loc, col = 2, pch = 50, cex = 0.1)

plot(mesh_2)
points(Loc, col = 2, pch = 50, cex = 0.1)

plot(mesh_3)
points(Loc, col = 2, pch = 50, cex = 0.1)

plot(mesh_4)
points(Loc, col = 2, pch = 50, cex = 0.1)

plot(mesh_5)
points(Loc, col = 2, pch = 50, cex = 0.1)

plot(mesh_6)# Looks alright for this data.
points(Loc_lap, col = 2, pch = 50, cex = 0.1)
# Define the projector matrix
# First step is computing weight factors that relate points to each other in the mesh

A1 <- inla.spde.make.A(mesh_6, loc = Loc_lap)

# Then define informative priors and calculate the SPDE. 
# Prior.range means that the prob of range being less than 10 km is very unlikely and 
# the prior.sigma defines the magnitude (smoothness?) 
# of the spatial random field. Range is based on knowledge about bird behaviour
spde1 <- inla.spde2.pcmatern(mesh_6, 
                             prior.range = c(6, 0.01), # Unlikely that range is smaller than 6 km
                             prior.sigma = c(1.5, 0.01))


# Define the spatial random field

w1.index <- inla.spde.make.index(name = 'w',
                                 n.spde = spde1$n.spde)



# N and Extract relevant columns

N_lap = nrow(lwg_lapwing)

N_red = nrow(lwg_redshank)

N_cur = nrow(lwg_curlew)

N_sni = nrow(lwg_snipe)

# Prepare each species' df for manual intercepts by specifying model matrix


x_lap <- data.frame(area_of_lwg_ha = MyStd(lwg_lapwing$area_of_lwg_ha),
                    rain_spring.std = lwg_lapwing$rain_spring.std,
                    rain_winter.std = lwg_lapwing$rain_winter.std,
                    temp_spring.std = lwg_lapwing$temp_spring.std,
                    temp_winter.std = lwg_lapwing$temp_winter.std,
                    year = lwg_lapwing$year,
                    year_1 = lwg_lapwing$year,
                    year_2 = lwg_lapwing$year,
                    year_3 = lwg_lapwing$year,
                    first_year = lwg_lapwing$first_year,
                    first_year1 = lwg_lapwing$first_year,
                    first_year2= lwg_lapwing$first_year,
                    first_year3 = lwg_lapwing$first_year,
                    water_adj = MyStd(lwg_lapwing$water_adj_analysis),
                    surface = MyStd(lwg_lapwing$water_surface),
                    habitat = lwg_lapwing$habitat_when_acquired,
                    predator_fence = lwg_lapwing$Ffenced,
                    fox_control = lwg_lapwing$fox,
                    crow_control = lwg_lapwing$crow,
                    sub_site = lwg_lapwing$sub_site,
                    main_site = lwg_lapwing$main_site,
                    country = lwg_lapwing$country,
                    climate_district = lwg_lapwing$climate_district,
                    countrymainsub = lwg_lapwing$countrymainsub,
                    countymainsub = lwg_lapwing$countymainsub,
                    mainsub = lwg_lapwing$mainsub,
                    grass = lwg_lapwing$grassland,
                    arable = lwg_lapwing$arable,
                    ex_min = lwg_lapwing$ex_mineral,
                    grass_arable = lwg_lapwing$grassland_arable
) 

x_red <- data.frame(area_of_lwg_ha = MyStd(lwg_redshank$area_of_lwg_ha),
                    rain_spring.std = lwg_redshank$rain_spring.std,
                    rain_winter.std = lwg_redshank$rain_winter.std,
                    temp_spring.std = lwg_redshank$temp_spring.std,
                    temp_winter.std = lwg_redshank$temp_winter.std,
                    year = lwg_redshank$year,
                    year_1 = lwg_redshank$year,
                    year_2 = lwg_redshank$year,
                    year_3 = lwg_redshank$year,
                    first_year = lwg_redshank$first_year,
                    first_year1 = lwg_redshank$first_year,
                    first_year2= lwg_redshank$first_year,
                    first_year3 = lwg_redshank$first_year,
                    water_adj = MyStd(lwg_redshank$water_adj_analysis),
                    surface = MyStd(lwg_redshank$water_surface),
                    predator_fence = lwg_redshank$Ffenced,
                    fox_control = lwg_redshank$fox,
                    crow_control = lwg_redshank$crow,
                    habitat = lwg_redshank$habitat_when_acquired,
                    sub_site = lwg_redshank$sub_site,
                    main_site = lwg_redshank$main_site,
                    country = lwg_redshank$country,
                    climate_district = lwg_redshank$climate_district,
                    countrymainsub = lwg_redshank$countrymainsub,
                    countymainsub = lwg_redshank$countymainsub,
                    mainsub = lwg_redshank$mainsub,
                    grass = lwg_redshank$grassland,
                    arable = lwg_redshank$arable,
                    ex_min = lwg_redshank$ex_mineral,
                    grass_arable = lwg_redshank$grassland_arable,
                    intercept = rep(1, N_red)
) 
x_cur <- data.frame(area_of_lwg_ha = MyStd(lwg_curlew$area_of_lwg_ha),
                    rain_spring.std = lwg_curlew$rain_spring.std,
                    rain_winter.std = lwg_curlew$rain_winter.std,
                    temp_spring.std = lwg_curlew$temp_spring.std,
                    temp_winter.std = lwg_curlew$temp_winter.std,
                    year = lwg_curlew$year,
                    year_1 = lwg_curlew$year,
                    year_2 = lwg_curlew$year,
                    year_3 = lwg_curlew$year,
                    first_year = lwg_curlew$first_year,
                    first_year1 = lwg_curlew$first_year,
                    first_year2= lwg_curlew$first_year,
                    first_year3 = lwg_curlew$first_year,
                    water_adj = MyStd(lwg_curlew$water_adj_analysis),
                    surface = MyStd(lwg_curlew$water_surface),
                    predator_fence = lwg_curlew$Ffenced,
                    fox_control = lwg_curlew$fox,
                    crow_control = lwg_curlew$crow,
                    year_cum = lwg_curlew$year_cum,
                    habitat = lwg_curlew$habitat_when_acquired,
                    sub_site = lwg_curlew$sub_site,
                    main_site = lwg_curlew$main_site,
                    country = lwg_curlew$country,
                    climate_district = lwg_curlew$climate_district,
                    countrymainsub = lwg_curlew$countrymainsub,
                    countymainsub = lwg_curlew$countymainsub,
                    mainsub = lwg_curlew$mainsub,
                    grass = lwg_curlew$grassland,
                    arable = lwg_curlew$arable,
                    ex_min = lwg_curlew$ex_mineral,
                    grass_arable = lwg_curlew$grassland_arable,
                    intercept = rep(1, N_cur)
) 

x_sni <- data.frame(area_of_lwg_ha = MyStd(lwg_snipe$area_of_lwg_ha),
                    rain_spring.std = lwg_snipe$rain_spring.std,
                    rain_winter.std = lwg_snipe$rain_winter.std,
                    temp_spring.std = lwg_snipe$temp_spring.std,
                    temp_winter.std = lwg_snipe$temp_winter.std,
                    year = lwg_snipe$year,
                    year_1 = lwg_snipe$year,
                    year_2 = lwg_snipe$year,
                    year_3 = lwg_snipe$year,
                    first_year = lwg_snipe$first_year,
                    first_year1 = lwg_snipe$first_year,
                    first_year2= lwg_snipe$first_year,
                    first_year3 = lwg_snipe$first_year,
                    water_adj = MyStd(lwg_snipe$water_adj_analysis),
                    surface = MyStd(lwg_snipe$water_surface),
                    predator_fence = lwg_snipe$Ffenced,
                    fox_control = lwg_snipe$fox,
                    crow_control = lwg_snipe$crow,
                    year_cum = lwg_snipe$year_cum,
                    habitat = lwg_snipe$habitat_when_acquired,
                    sub_site = lwg_snipe$sub_site,
                    main_site = lwg_snipe$main_site,
                    country = lwg_snipe$country,
                    climate_district = lwg_snipe$climate_district,
                    countrymainsub = lwg_snipe$countrymainsub,
                    countymainsub = lwg_snipe$countymainsub,
                    mainsub = lwg_snipe$mainsub,
                    grass = lwg_snipe$grassland,
                    arable = lwg_snipe$arable,
                    ex_min = lwg_snipe$ex_mineral,
                    grass_arable = lwg_snipe$grassland_arable,
                    intercept = rep(1, N_sni)
) 
# Random effects Lapwing ----
# All fixed variables are included but no experimenting with interactions

# Exclude main site. Doesn't change Waic so exclude

f6 <- count ~ -1 + intercept + area_of_lwg_ha + predator_fence*fox_control + predator_fence*crow_control+
  rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
  water_adj * surface +
  f(sub_site, model = "iid", hyper = hyper_iid) +
  f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
  f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
  f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
  f(first_year3, ex_min, model = "rw2", hyper = hyper_rw, scale.model=TRUE)

# Specify stacks
StackFit1 <- inla.stack(
  tag = "Fit",
  data = list(count = as.integer(lwg_lapwing$count_analysis)),  
  A = list(1,1),                  
  effects = list(
    x_lap = x_lap,
    intercept = rep(1, N_lap)))



lapzinb6 <- inla(f6,
                 family = "zeroinflatednbinomial1",
                 data = inla.stack.data(StackFit1),
                 control.compute = list(dic = TRUE, waic = TRUE),
                 control.predictor = list(A = inla.stack.A(StackFit1)))
# Model validation Lapwing ----
# Diagnostics from inlatools and INLAautils don't work with stack so can be only be used for
# models were best fit was not from a SPDE model.

# Create a new df of variables. 

x_lap_val <- data.frame(area_of_lwg_ha = MyStd(lwg_lapwing$area_of_lwg_ha),
                           rain_spring.std = lwg_lapwing$rain_spring.std,
                           rain_winter.std = lwg_lapwing$rain_winter.std,
                           temp_spring.std = lwg_lapwing$temp_spring.std,
                           temp_winter.std = lwg_lapwing$temp_winter.std,
                           year = lwg_lapwing$year,
                           year_1 = lwg_lapwing$year,
                           year_2 = lwg_lapwing$year,
                           year_3 = lwg_lapwing$year,
                           first_year = lwg_lapwing$first_year,
                           first_year1 = lwg_lapwing$first_year,
                           first_year2= lwg_lapwing$first_year,
                           first_year3 = lwg_lapwing$first_year,
                           water_adj = MyStd(lwg_lapwing$water_adj_analysis),
                           surface = MyStd(lwg_lapwing$water_surface),
                           habitat = lwg_lapwing$habitat_when_acquired,
                           predator_fence = lwg_lapwing$Ffenced,
                           fox_control = lwg_lapwing$fox,
                           crow_control = lwg_lapwing$crow,
                           sub_site = lwg_lapwing$sub_site,
                           main_site = lwg_lapwing$main_site,
                           country = lwg_lapwing$country,
                           climate_district = lwg_lapwing$climate_district,
                           countrymainsub = lwg_lapwing$countrymainsub,
                           countymainsub = lwg_lapwing$countymainsub,
                           mainsub = lwg_lapwing$mainsub,
                           grass = lwg_lapwing$grassland,
                           arable = lwg_lapwing$arable,
                           ex_min = lwg_lapwing$ex_mineral,
                           grass_arable = lwg_lapwing$grassland_arable,
                           count = lwg_lapwing$count_analysis
) 



lap_val <- inla(count ~ -1 + intercept + area_of_lwg_ha + predator_fence*fox_control + predator_fence*crow_control+
                  rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
                  water_adj * surface +
                  f(sub_site, model = "iid", hyper = hyper_iid) +
                  f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year3, ex_min, model = "rw2", hyper = hyper_rw, scale.model=TRUE),
                control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
                control.predictor = list(compute = TRUE),
                family ="zeroinflatednbinomial1",
                data = x_lap_val)
summary(lap_val)
check<- dispersion_check(lap_val)
plot(check)
check_dist <- distribution_check(lap_val)
plot(check_dist)
autoplot(lap_val)

lapobserved <- lwg_lapwing$count_analysis

ggplot_inla_residuals(lapzinb6, lapobserved)
ggplot_inla_residuals2(lap_val, lapobserved)


# Calculating Overdispersion and Pearson's residuals for a ZINB model
# using lap_val as example

Betas2 <- lapzinb6$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas2, digits = 3)

# single pi value
Pi.inla <- lapzinb6$summary.hyper[2, "mean"]
Pi.inla

# unspecified row numbers only works when using a model that is not specified using a stack, 
# otherwise the rows have to be specified manually like below
mu2 <- lapzinb6$summary.fitted.values[1:N_lap,'mean'] # Since we are using a stack, otherwise the row numbers can be left blank


ExpY <- mu2*(1-Pi.inla)
# calculate k (the extra dispersion parameter in nb model). Can also be directly assigned as
# <- zinb1$summary.hyper[1, "mean"]

k_size <- lapzinb6$marginals.hyperpar$`size for nbinomial zero-inflated observations`
k_param <- inla.emarginal(function(x)  x, k_size)

# calculate ZINB variation and Pearson's res

VarY <- (1 - Pi.inla)*mu2*(1+Pi.inla*mu2 + (mu2/k_param))
E2   <- (lwg_lapwing$count_analysis - ExpY) / sqrt(VarY)

# Plot fitted vs Pearson's residuals
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

# Create a df with all the necessary information for plotting residuals against model variables

fit <- lapzinb6$summary.fitted.values[1:N_lap,"mean"]
tester <- bind_cols(fit, lwg_lapwing$count_analysis)

tester <- tester %>% 
  rename(fitted = 1, observed = 2)

tester <- tester %>% 
  mutate(residual_count = observed - fitted) 

test <- bind_cols(lwg_lapwing$count_analysis, mu2, E2, tester$residual_count) %>% rename(observed = 1, mu2 = 2, E2 = 3, res = 4)


test_com <- bind_cols(lwg_lapwing, test) %>% 
  select(main_site, sub_site, year, observed, count_analysis, mu2, res, E2)

test_com %>% 
  ggplot(., aes(x = year, y = E2)) +
  geom_smooth()+
  geom_point()#+
  #facet_wrap(~sub_site, scales = "free_y")

# Random effects Redshank ----
# redzinb5. Exclude climate district. WAIC 5368.48 so exclude

redzinb5 <- inla(lwg_redshank$count_analysis ~ -1 + intercept + area_of_lwg_ha + predator_fence*fox_control + predator_fence*crow_control+
                   rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
                   water_adj * surface +
                   f(sub_site, model = "iid", hyper = hyper_iid) + 
                   f(main_site, model = "iid", hyper = hyper_iid) +
                   f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year3, ex_min, model = "rw2", hyper = hyper_rw, scale.model=TRUE),
                 control.compute = list(dic = TRUE, waic = TRUE),
                 control.predictor = list(compute = TRUE),
                 family ="zeroinflatednbinomial1",
                 data = x_red) 

# redzinb6. Exclude main site. WAIC 5371.85 so keep main site

redzinb6 <- inla(lwg_redshank$count_analysis ~ -1 + intercept + area_of_lwg_ha + predator_fence*fox_control + predator_fence*crow_control+
                   rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
                   water_adj * surface +
                   f(sub_site, model = "iid", hyper = hyper_iid) +
                   f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year3, ex_min, model = "rw2", hyper = hyper_rw, scale.model=TRUE),
                 control.compute = list(dic = TRUE, waic = TRUE),
                 control.predictor = list(compute = TRUE),
                 family ="zeroinflatednbinomial1",
                 data = x_red) 

# Model validation Redshank ----
# Calculating Overdispersion and Pearson's residuals for a ZINB model
# First we have to create a new dataframe containing the dependent variable, as the dispersion_check doesn't work when the
# dependent is specified in a different df


x_red_val <- data.frame(count = lwg_redshank$count_analysis,
                        area_of_lwg_ha = MyStd(lwg_redshank$area_of_lwg_ha),
                        rain_spring.std = lwg_redshank$rain_spring.std,
                        rain_winter.std = lwg_redshank$rain_winter.std,
                        temp_spring.std = lwg_redshank$temp_spring.std,
                        temp_winter.std = lwg_redshank$temp_winter.std,
                        year = lwg_redshank$year,
                        year_1 = lwg_redshank$year,
                        year_2 = lwg_redshank$year,
                        year_3 = lwg_redshank$year,
                        first_year = lwg_redshank$first_year,
                        first_year1 = lwg_redshank$first_year,
                        first_year2= lwg_redshank$first_year,
                        first_year3 = lwg_redshank$first_year,
                        water_adj = MyStd(lwg_redshank$water_adj_analysis),
                        surface = MyStd(lwg_redshank$water_surface),
                        predator_fence = lwg_redshank$Ffenced,
                        fox_control = lwg_redshank$fox,
                        crow_control = lwg_redshank$crow,
                        habitat = lwg_redshank$habitat_when_acquired,
                        sub_site = lwg_redshank$sub_site,
                        main_site = lwg_redshank$main_site,
                        country = lwg_redshank$country,
                        climate_district = lwg_redshank$climate_district,
                        countrymainsub = lwg_redshank$countrymainsub,
                        countymainsub = lwg_redshank$countymainsub,
                        mainsub = lwg_redshank$mainsub,
                        grass = lwg_redshank$grassland,
                        arable = lwg_redshank$arable,
                        ex_min = lwg_redshank$ex_mineral,
                        grass_arable = lwg_redshank$grassland_arable,
                        intercept = rep(1, N_red)
) 

# And then respecify the model

red_val <- inla(count ~ -1 + intercept + area_of_lwg_ha + predator_fence*fox_control + predator_fence*crow_control+
                  rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
                  water_adj * surface +
                  f(sub_site, model = "iid", hyper = hyper_iid) + 
                  f(main_site, model = "iid", hyper = hyper_iid) +
                  f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year3, ex_min, model = "rw2", hyper = hyper_rw, scale.model=TRUE),
                control.compute = list(dic = TRUE, waic = TRUE),
                control.predictor = list(compute = TRUE),
                family ="zeroinflatednbinomial1",
                data = x_red_val) 

# Now check dispersion
check<- dispersion_check(red_val)

plot(check) # It is actually a bit underdispersed... check the quick plot

check_fast <- fast_distribution_check(red_val)

plot(check_fast) # Slightly overdispersed at first and then slightly underdispersed

autoplot(redzinb5)

redobserved <- lwg_redshank$count_analysis

ggplot_inla_residuals(redzinb5, redobserved)

ggplot_inla_residuals2(redzinb5, redobserved)


Betas2 <- redzinb5$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas2, digits = 3)

# single pi value
Pi.inla <- redzinb5$summary.hyper[2, "mean"]
Pi.inla

# unspecified row numbers only works when using a model that is not specified using a stack, 
# otherwise the rows have to be specified manually like below
mu2 <- redzinb5$summary.fitted.values[1:1186,'mean'] # Since we are using a stack, otherwise the row numbers can be left blank


ExpY <- mu2*(1-Pi.inla)
# calculate k (the extra dispersion parameter in nb model). Can also be directly assigned as
# <- zinb1$summary.hyper[1, "mean"]

k_size <- redzinb5$marginals.hyperpar$`size for nbinomial zero-inflated observations`
k_param <- inla.emarginal(function(x)  x, k_size)

# calculate ZINB variation and Pearson's res

VarY <- (1 - Pi.inla)*mu2*(1+Pi.inla*mu2 + (mu2/k_param))
E2   <- (lwg_redshank$count_analysis - ExpY) / sqrt(VarY)

# Plot fitted vs Pearson's residuals
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

# Create a df with all the necessary information for plotting residuals against model variables

fit <- redzinb5$summary.fitted.values[,"mean"]
tester <- bind_cols(fit, lwg_redshank$count_analysis)

tester <- tester %>% 
  rename(fitted = 1, observed = 2)

tester <- tester %>% 
  mutate(residual_count = observed - fitted) 

test <- bind_cols(lwg_redshank$count_analysis, mu2, E2, tester$residual_count) %>% rename(observed = 1, mu2 = 2, E2 = 3, res = 4)


test_com <- bind_cols(lwg_redshank, test) %>% 
  select(main_site, sub_site, year, observed, count_analysis, mu2, res, E2)

test_com %>% 
  ggplot(., aes(x = year, y = E2)) +
  geom_smooth()+
  geom_point()

# Random effects Snipe ----

# snizinb6. Exclude main site. WAIC 2382.51 so exclude

snizinb6 <- inla(lwg_snipe$count_analysis ~ -1 + intercept + area_of_lwg_ha + 
                   predator_fence*crow_control + predator_fence*fox_control +
                   rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
                   water_adj*surface +
                   f(sub_site, model = "iid", hyper = hyper_iid) +
                   f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE),
                 control.compute = list(dic = TRUE, waic = TRUE),
                 control.predictor = list(compute = TRUE),
                 family ="zeroinflatednbinomial1",
                 data = x_sni) 

# Model validation Snipe ----

# Specify new df containing the dependent as the dispersion check does not work without it.

x_sni_val <- data.frame(count = lwg_snipe$count_analysis,
                        area_of_lwg_ha = MyStd(lwg_snipe$area_of_lwg_ha),
                        rain_spring.std = lwg_snipe$rain_spring.std,
                        rain_winter.std = lwg_snipe$rain_winter.std,
                        temp_spring.std = lwg_snipe$temp_spring.std,
                        temp_winter.std = lwg_snipe$temp_winter.std,
                        year = lwg_snipe$year,
                        year_1 = lwg_snipe$year,
                        year_2 = lwg_snipe$year,
                        year_3 = lwg_snipe$year,
                        first_year = lwg_snipe$first_year,
                        first_year1 = lwg_snipe$first_year,
                        first_year2= lwg_snipe$first_year,
                        first_year3 = lwg_snipe$first_year,
                        water_adj = MyStd(lwg_snipe$water_adj_analysis),
                        surface = MyStd(lwg_snipe$water_surface),
                        predator_fence = lwg_snipe$Ffenced,
                        fox_control = lwg_snipe$fox,
                        crow_control = lwg_snipe$crow,
                        year_cum = lwg_snipe$year_cum,
                        habitat = lwg_snipe$habitat_when_acquired,
                        sub_site = lwg_snipe$sub_site,
                        main_site = lwg_snipe$main_site,
                        country = lwg_snipe$country,
                        climate_district = lwg_snipe$climate_district,
                        countrymainsub = lwg_snipe$countrymainsub,
                        countymainsub = lwg_snipe$countymainsub,
                        mainsub = lwg_snipe$mainsub,
                        grass = lwg_snipe$grassland,
                        arable = lwg_snipe$arable,
                        ex_min = lwg_snipe$ex_mineral,
                        grass_arable = lwg_snipe$grassland_arable,
                        intercept = rep(1, N_sni)
) 

# Specify model
sni_val <- inla(count ~ -1 + intercept + area_of_lwg_ha + 
                  predator_fence*crow_control + predator_fence*fox_control +
                  rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
                  water_adj*surface +
                  f(sub_site, model = "iid", hyper = hyper_iid) +
                  f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE),
                control.compute = list(dic = TRUE, waic = TRUE),
                control.predictor = list(compute = TRUE),
                family ="zeroinflatednbinomial1",
                data = x_sni_val) 


# Check for overdispersion - causing some problems...
check<- dispersion_check(sni_val)
plot(check)

autoplot(snizinb5)

observed <- lwg_snipe$count_analysis

res <- residuals(sni_val)
ggplot_inla_residuals(snizinb6, observed)
ggplot_inla_residuals2(snizinb6, observed)


# Calculating Overdispersion and Pearson's residuals for a ZINB model
# using sni_val as example

Betas2 <- snizinb6$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas2, digits = 3)

# single pi value
Pi.inla <- snizinb6$summary.hyper[2, "mean"]
Pi.inla

# unspecified row numbers only works when using a model that is not specified using a stack, 
# otherwise the rows have to be specified manually like below
mu2 <- snizinb6$summary.fitted.values[1:652,'mean'] # Since we are using a stack, otherwise the row numbers can be left blank


ExpY <- mu2*(1-Pi.inla)
# calculate k (the extra dispersion parameter in nb model). Can also be directly assigned as
# <- zinb1$summary.hyper[1, "mean"]

k_size <- snizinb6$marginals.hyperpar$`size for nbinomial zero-inflated observations`
k_param <- inla.emarginal(function(x)  x, k_size)

# calculate ZINB variation and Pearson's res

VarY <- (1 - Pi.inla)*mu2*(1+Pi.inla*mu2 + (mu2/k_param))
E2   <- (lwg_snipe$count_analysis - ExpY) / sqrt(VarY)

# Plot fitted vs Pearson's residuals
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

# Create a df with all the necessary information for plotting residuals against model variables

fit <- snizinb6$summary.fitted.values[,"mean"]
tester <- bind_cols(fit, lwg_snipe$count_analysis)

tester <- tester %>% 
  rename(fitted = 1, observed = 2)

tester <- tester %>% 
  mutate(residual_count = observed - fitted) 

test <- bind_cols(lwg_snipe$count_analysis, mu2, E2, tester$residual_count) %>% rename(observed = 1, mu2 = 2, E2 = 3, res = 4)


test_com <- bind_cols(lwg_snipe, test) %>% 
  select(main_site, sub_site, year, observed, count_analysis, mu2, res, E2)

test_com %>% 
  ggplot(., aes(x = year, y = E2)) +
  geom_smooth()+
  geom_point()

facet_wrap(~sub_site, scales = "free_y")

# Random effects curlew ----

# curzinb6. Exclude main site. WAIC 1102.52 so no effect. Exclude

curzinb6 <- inla(lwg_curlew$count_analysis ~ -1 + intercept + area_of_lwg_ha + 
                   predator_fence*fox_control + predator_fence*crow_control+
                   rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
                   water_adj * surface +
                   f(sub_site, model = "iid", hyper = hyper_iid) +
                   f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                   f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE),
                 control.compute = list(dic = TRUE, waic = TRUE),
                 control.predictor = list(compute = TRUE),
                 family ="poisson",
                 data = x_cur) 




# Model validation curlew ----

# Specify new df containing the dependent as the dispersion check does not work without it.

x_cur_val <- data.frame(count = lwg_curlew$count_analysis,
                        area_of_lwg_ha = MyStd(lwg_curlew$area_of_lwg_ha),
                        rain_spring.std = lwg_curlew$rain_spring.std,
                        rain_winter.std = lwg_curlew$rain_winter.std,
                        temp_spring.std = lwg_curlew$temp_spring.std,
                        temp_winter.std = lwg_curlew$temp_winter.std,
                        year = lwg_curlew$year,
                        year_1 = lwg_curlew$year,
                        year_2 = lwg_curlew$year,
                        year_3 = lwg_curlew$year,
                        first_year = lwg_curlew$first_year,
                        first_year1 = lwg_curlew$first_year,
                        first_year2= lwg_curlew$first_year,
                        first_year3 = lwg_curlew$first_year,
                        water_adj = MyStd(lwg_curlew$water_adj_analysis),
                        surface = MyStd(lwg_curlew$water_surface),
                        predator_fence = lwg_curlew$Ffenced,
                        fox_control = lwg_curlew$fox,
                        crow_control = lwg_curlew$crow,
                        year_cum = lwg_curlew$year_cum,
                        habitat = lwg_curlew$habitat_when_acquired,
                        sub_site = lwg_curlew$sub_site,
                        main_site = lwg_curlew$main_site,
                        country = lwg_curlew$country,
                        climate_district = lwg_curlew$climate_district,
                        countrymainsub = lwg_curlew$countrymainsub,
                        countymainsub = lwg_curlew$countymainsub,
                        mainsub = lwg_curlew$mainsub,
                        grass = lwg_curlew$grassland,
                        arable = lwg_curlew$arable,
                        ex_min = lwg_curlew$ex_mineral,
                        grass_arable = lwg_curlew$grassland_arable,
                        intercept = rep(1, N_cur)
)

# Specify model
cur_val <- inla(count ~ -1 + intercept + area_of_lwg_ha + 
                  predator_fence*fox_control + predator_fence*crow_control+
                  rain_spring.std + rain_winter.std + temp_spring.std + temp_winter.std +
                  water_adj * surface +
                  f(sub_site, model = "iid", hyper = hyper_iid) +
                  f(first_year, grass, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year1,arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE) +
                  f(first_year2, grass_arable, model = "rw2", hyper = hyper_rw, scale.model=TRUE),
                control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
                control.predictor = list(compute = TRUE),
                family ="poisson",
                data = x_cur_val) 


# Check for overdispersion 
check<- dispersion_check(cur_val)
plot(check)

check_dist <- distribution_check(cur_val)
plot(check_dist)

autoplot(curzinb6)

observed <- lwg_curlew$count_analysis

ggplot_inla_residuals(cur_val, observed)
ggplot_inla_residuals2(cur_val, observed)


# Calculating Overdispersion and Pearson's residuals for a ZINB model

Betas2 <- cur_val$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
print(Betas2, digits = 3)

# single pi value
Pi.inla <- snizinb5$summary.hyper[2, "mean"]
Pi.inla

# unspecified row numbers only works when using a model that is not specified using a stack, 
# otherwise the rows have to be specified manually like below
mu2 <- snizinb5$summary.fitted.values[,'mean'] # Since we are using a stack, otherwise the row numbers can be left blank


ExpY <- mu2*(1-Pi.inla)
# calculate k (the extra dispersion parameter in nb model). Can also be directly assigned as
# <- zinb1$summary.hyper[1, "mean"]

k_size <- sni_val$marginals.hyperpar$`size for nbinomial zero-inflated observations`
k_param <- inla.emarginal(function(x)  x, k_size)

# calculate ZINB variation and Pearson's res

VarY <- (1 - Pi.inla)*mu2*(1+Pi.inla*mu2 + (mu2/k_param))
E2   <- (lwg_curlew$count_analysis - ExpY) / sqrt(VarY)

# Plot fitted vs Pearson's residuals
par(mfrow = c(1,1), mar = c(5,5,2,2), cex.lab = 1.5)
plot(x = mu2, 
     y = E2,
     xlab = "Fitted values",
     ylab = "Pearson residuals")
abline(h = 0, lty = 2)

# Create a df with all the necessary information for plotting residuals against model variables

fit <- snizinb5$summary.fitted.values[1:1164,"mean"]
tester <- bind_cols(fit, lwg_curlew$count_analysis)

tester <- tester %>% 
  rename(fitted = 1, observed = 2)

tester <- tester %>% 
  mutate(residual_count = observed - fitted) 

test <- bind_cols(lwg_curlew$count_analysis, mu2, E2, tester$residual_count) %>% rename(observed = 1, mu2 = 2, E2 = 3, res = 4)


test_com <- bind_cols(lwg_curlew, test) %>% 
  select(main_site, sub_site, year, observed, count_analysis, mu2, res, E2)

test_com %>% 
  ggplot(., aes(x = year, y = E2)) +
  geom_smooth()+
  geom_point()

facet_wrap(~sub_site, scales = "free_y")

# Plot parameter estimates ----

# First, extract from INLA element

lapbeta <- lapzinb6$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
redbeta <- redzinb5$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")] 
snibeta <- snizinb6$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")]
curbeta <- curzinb6$summary.fixed[,c("mean", "sd", "0.025quant", "0.975quant")]

lapbeta <- lapbeta %>% 
  rownames_to_column("coefficients") %>% 
  mutate(species = 'Lapwing')

redbeta <- redbeta %>% 
  rownames_to_column("coefficients") %>% 
  mutate(species = 'Redshank')

snibeta <- snibeta %>% 
  rownames_to_column("coefficients") %>% 
  mutate(species = 'Snipe')

curbeta <- curbeta %>% 
  rownames_to_column("coefficients") %>% 
  mutate(species = 'Curlew')

betas <- bind_rows(lapbeta, redbeta, snibeta, curbeta) %>% 
  rename(upper = '0.025quant',
         lower = '0.975quant')

# Plot
betas %>% 
  filter(coefficients != "intercept") %>%
  #filter(species %in% c("Lapwing", "Redshank")) %>% 
  ggplot(., aes(y = factor(coefficients), x = mean, xmin = lower, xmax = upper, color = species, shape = species)) + 
  xlim(-2.5, 2) +
  geom_point(aes(shape=species, color = species),size = 3, position=position_dodge(width = 0.5)) +
  geom_errorbar(position=position_dodge(width = 0.5), width = 0) +
  #facet_wrap(~species, scales = "free_x") +
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1)+
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.title = element_blank())+
  scale_color_viridis_d(begin = 0, end = 0.7)

## Plot rw2 
beta1+beta2

# Smoothers
par(mfrow=c(4,2))

# Grass Lapwing ----
Smoother <- lapzinb6$summary.random$first_year

plot(x = Smoother[,"ID"],
     y = Smoother[,"mean"],
     type='l',
     lwd = 3,
     xlab = 'Grassland', 
     ylab = 'Trend',
     ylim = c(-1, 1),
     xlim = c (-1, 25))
abline(h = 0, lty =3)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.025quant"], 
      lty = 2)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.975quant"], 
      lty = 2)
# Arable
Smoother <- lapzinb6$summary.random$first_year1

plot(x = Smoother[,"ID"],
     y = Smoother[,"mean"],
     type='l',
     lwd = 3,
     xlab = 'Arable', 
     ylab = 'Trend',
     ylim = c(-1, 1),
     xlim = c (-1, 25))
abline(h = 0, lty =3)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.025quant"], 
      lty = 2)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.975quant"], 
      lty = 2)

# Grass Redshank ----
Smoother <- redzinb5$summary.random$first_year

plot(x = Smoother[,"ID"],
     y = Smoother[,"mean"],
     type='l',
     lwd = 3,
     xlab = 'Grassland', 
     ylab = 'Trend',
     ylim = c(-1, 1),
     xlim = c (-1, 25))
abline(h = 0, lty =3)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.025quant"], 
      lty = 2)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.975quant"], 
      lty = 2)
# Arable
Smoother <- redzinb5$summary.random$first_year1

plot(x = Smoother[,"ID"],
     y = Smoother[,"mean"],
     type='l',
     lwd = 3,
     xlab = 'Arable', 
     ylab = 'Trend',
     ylim = c(-2, 2),
     xlim = c (-1, 25))
abline(h = 0, lty =3)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.025quant"], 
      lty = 2)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.975quant"], 
      lty = 2)

# Grass Snipe ----
Smoother <- snizinb6$summary.random$first_year

plot(x = Smoother[,"ID"],
     y = Smoother[,"mean"],
     type='l',
     lwd = 3,
     xlab = 'Grassland', 
     ylab = 'Trend',
     ylim = c(-1, 1),
     xlim = c (-1, 25))
abline(h = 0, lty =3)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.025quant"], 
      lty = 2)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.975quant"], 
      lty = 2)
# Arable
Smoother <- snizinb6$summary.random$first_year1

plot(x = Smoother[,"ID"],
     y = Smoother[,"mean"],
     type='l',
     lwd = 3,
     xlab = 'Arable', 
     ylab = 'Trend',
     ylim = c(-3.5, 2.5),
     xlim = c (-1, 25))
abline(h = 0, lty =3)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.025quant"], 
      lty = 2)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.975quant"], 
      lty = 2)

# Grass Curlew ----
Smoother <- curzinb6$summary.random$first_year

plot(x = Smoother[,"ID"],
     y = Smoother[,"mean"],
     type='l',
     lwd = 3,
     xlab = 'Grassland', 
     ylab = 'Trend',
     ylim = c(-1, 1),
     xlim = c (-1, 25))
abline(h = 0, lty =3)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.025quant"], 
      lty = 2)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.975quant"], 
      lty = 2)
# Arable
Smoother <- curzinb6$summary.random$first_year1

plot(x = Smoother[,"ID"],
     y = Smoother[,"mean"],
     type='l',
     lwd = 3,
     xlab = 'Arable', 
     ylab = 'Trend',
     ylim = c(-3, 2.5),
     xlim = c (-1, 25))
abline(h = 0, lty =3)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.025quant"], 
      lty = 2)
lines(x = Smoother[, "ID"],
      y = Smoother[, "0.975quant"], 
      lty = 2)
