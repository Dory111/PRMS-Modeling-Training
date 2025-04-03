# PRMS Input files
# November 2023
# Katrina Arredondo
# Updated by Chris Dory
# March 2025
# ------------------------------------------------------------------------------------------------
# Load dependencies
library(plyr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(sf)
library(tidyr)
library(data.table)
# ------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------
# GitHub connections
proj_dir <- file.path(here::here())
functions_dir <- file.path(proj_dir,'src','functions')
src_dir <- file.path(proj_dir,'src','functions')
# ------------------------------------------------------------------------------------------------




# ------------------------------------------------------------------------------------------------
# Dropbox path connections 
username <- Sys.info()[["user"]]
dropbox_dir <- paste0("C:/Users/",username,"/LWA Dropbox/")
db_path <- paste0(dropbox_dir,"00_Project-Repositories/00598-PRMS-Modeling/butte")
# db_path_data <- paste0(dropbox_dir,"00_Project-Repositories/00598-Siskiyou-GSP-data/outputs/butte")
db_path_data <- paste0(dropbox_dir,"00_Project-Repositories/00598-PRMS-Modeling/butte/data/")
out_dir <- file.path(db_path, "input_files_extension")
# ------------------------------------------------------------------------------------------------


# 
# 
# # ------------------------------------------------------------------------------------------------
# # Temporary testing dirs 
# proj_dir <- file.path(dropbox_dir,'Christopher Dory/Projects/LWA_PRMS')
# functions_dir <- file.path(proj_dir,'src','functions')
# src_dir <- file.path(proj_dir,'src','functions')
# # ------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------
# Read functions from the subdirectory functions 
for (fc in list.files(full.names = TRUE, file.path(functions_dir),pattern="*.R")) {
  source(fc)
}
# ------------------------------------------------------------------------------------------------





#===========================================================================================
# Import_Extensions_Files
#===========================================================================================
Import_Extensions_Files <- function()
{
  
  # ------------------------------------------------------------------------------------------------
  # ----------- Import Input files for extension of model ------------------
  tbl_station <<- read_excel(file.path(db_path,"GIS","monitoring_stations", "Butte_monitoring_stations.xlsx"),
                            sheet = "Monitoring_Stations")
  
  tbl_flow_station <<- read_excel(file.path(db_path,"GIS","monitoring_stations", "Butte_monitoring_stations.xlsx"),
                                 sheet = "Streamflow_stations")
  
  tbl_diversions <<- read_excel(file.path(db_path,"GIS","monitoring_stations", "Butte_monitoring_stations.xlsx"),
                               sheet = "Diversions")
  
  start_date <<- lubridate::mdy("12/31/2015") # End of original data file
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------
Import_Extensions_Files()





#===========================================================================================
# Loads precipitation data
#===========================================================================================
Load_Precipitation <- function()
{
  
  # ------------------------------------------------------------------------------------------------
  ######  ------ Precipitation ------- #########
  # Initialize dataframe
  tbl_ppt <- data.frame(matrix(ncol = length(c(tbl_station$Station_ID))+1, nrow = 0))
  names(tbl_ppt) <- c("Date",tbl_station$Station_ID)
  tbl_ppt$Date <- as.Date(tbl_ppt$Date)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Initializes the progress bar
  max_pb <- 100
  pb <- winProgressBar(title = "Windows progress bar", # Window title
                       label = "Percentage completed", # Window label
                       min = 0,      # Minimum value of the bar
                       max = max_pb, # Maximum value of the bar
                       initial = 0,  # Initial value of the bar
                       width = 500L) # Width of the window
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # NOAA data
  station_id <- tbl_station %>% dplyr::select(stationid) %>% drop_na() %>% pull(stationid)
  
  noaa_data <- read_csv(file.path(db_path_data, "noaa", "ppt.csv")) %>%
    dplyr::mutate(prcp = signif(VALUE*0.1 / 25.4, digits = 3)) %>% # convert mm to inch
    dplyr::mutate(prcp = case_when(is.na(Q_FLAG) == FALSE ~ NA,
                                   TRUE ~ prcp)) %>% # if a flag exists, then remove data
    dplyr::filter(STATION %in% station_id) %>%
    dplyr::rename(Date = DATE) %>%
    dplyr::filter(Date > start_date) %>%
    dplyr::select(STATION, Date, prcp) %>%
    pivot_wider(names_from = STATION, values_from = prcp)
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # NRCS data
  station_id <- tbl_station %>% dplyr::select(stationTriplet) %>% drop_na() %>% pull()
  
  nrcs_data <- read_csv(file.path(db_path_data, "nrcs", "prcp.csv")) %>%
    dplyr::filter(Station_ID %in% station_id) %>%
    dplyr::select(Station_ID, Date, ppt_in) %>%
    dplyr::filter(Date > start_date) %>%
    pivot_wider(names_from = Station_ID, values_from = ppt_in)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Merge all together -----------------------------------------
  tbl_new <- left_join(noaa_data, nrcs_data, by = "Date") %>%
    dplyr::filter(Date > start_date)
  
  # merge to master data file
  tbl <- full_join(tbl_ppt, tbl_new)
  
  tbl_dates <- tbl %>%
    dplyr::select(Date) %>%
    dplyr::mutate(YY = lubridate::year(Date),
                  MM = lubridate::month(Date),
                  DD = lubridate::day(Date),
                  hh = 0,
                  mm = 0,
                  ss = 0) 
  
  tbl <- distinct(tbl, Date, .keep_all = TRUE) # remove any duplicate dates
  tbl_append <- left_join(tbl_dates, tbl, by = "Date") %>% # Append date columns
    dplyr::arrange(tbl, Date) %>% # sort Date 
    dplyr::select(-Date) %>%
    replace(is.na(.), -99) # Catch any missed NAs
  # ------------------------------------------------------------------------------------------------
  
  
  
 
  
  # ------------------------------------------------------------------------------------------------
  # Recopy original file and append new data
  if(file.exists(file.path(db_path,"model_files/extension/data/KLAMATH/PPT/KLAMATH_MEAS_PPT.data"))){
    file.copy(from = file.path(db_path,"model_files/extension/data/KLAMATH/PPT/Original/KLAMATH_MEAS_PPT_orig.data"),
              to = file.path(db_path,"model_files/extension/data/KLAMATH/PPT/KLAMATH_MEAS_PPT_temp.data"))
    file.rename(from = file.path(db_path,"model_files/extension/data/KLAMATH/PPT",
                                 "KLAMATH_MEAS_PPT_temp.data"), 
                to = file.path(db_path,"model_files/extension/data/KLAMATH/PPT",
                               "KLAMATH_MEAS_PPT.data"))
  } else {}
  
  fwrite(tbl_append,
         file = file.path(db_path,"model_files/extension/data/KLAMATH/PPT/KLAMATH_MEAS_PPT.data"),
         sep = " ", col.names=FALSE,
         append=TRUE)
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Plot timeseries to double check appended correctly
  old_data <- read_csv(file.path(db_path,"model_files/extension/data/KLAMATH/PPT/KLAMATH_MEAS_PPT.data"), skip = 7) %>%
    separate(names(read_csv(file.path(db_path,"model_files/extension/data/KLAMATH/PPT/KLAMATH_MEAS_PPT.data"), skip = 7)),
             c("YY", "MM", "DD", "hh", "mm", "ss", names(tbl_ppt)[2:ncol(tbl_ppt)]),
             sep = " ")
  
  old_data <- old_data %>% 
    dplyr::mutate(Date = lubridate::mdy(paste(MM,"/",DD,"/",YY))) %>%
    dplyr::select(-YY,-MM,-DD,-hh,-mm,-ss) %>%
    pivot_longer(!Date, names_to="Station", values_to = "Results") %>%
    dplyr::mutate(Results = as.numeric(Results))
  
  tbl_plot <- tbl %>% 
    pivot_longer(!Date, names_to="Station", values_to = "Results") %>%
    full_join(old_data) %>%
    mutate(Results = ifelse(Results == -99,NA,Results)) %>%
    dplyr::arrange(Date) # sort Date 
  
  sta_list <- unique(tbl_plot$Station[!is.na(tbl_plot$Station)])
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  for (i in 1:length(sta_list)) {
    nm <- tbl_plot %>% filter(Station == sta_list[i])
    
    g <- ggplot(nm) + geom_line(aes(x = Date, y = Results)) +
      labs(x = 'Date', y = 'Precipitation (in)')
    
    ggsave(file.path(db_path,"model_files/extension/data/KLAMATH/PPT/figures",
                     glue::glue("{make.names(nm$Station[1])}_ppt.png")), g, height = 5, width = 10)
    
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------
Load_Precipitation()






#===========================================================================================
# Loads maximum temperature data
#===========================================================================================
Load_TMax <- function()
{
  # ------------------------------------------------------------------------------------------------
  ######  ------ Maximum Temperature ------- #########
  # Initialize dataframe
  tbl_tmax <- data.frame(matrix(ncol = length(c(tbl_station$Station_ID))+1, nrow = 0))
  names(tbl_tmax) <- c("Date",tbl_station$Station_ID)
  tbl_tmax$Date <- as.Date(tbl_tmax$Date)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # NOAA data
  station_id <- tbl_station %>% dplyr::select(stationid) %>% drop_na() %>% pull(stationid)
  
  noaa_data <- read_csv(file.path(db_path_data, "noaa", "tmax.csv")) %>%
    dplyr::mutate(tmax = signif(VALUE*0.1*(9/5) + 32, digits = 3)) %>% # convert C to F 
    dplyr::mutate(tmax = case_when(is.na(Q_FLAG) == FALSE ~ NA,
                                   TRUE ~ tmax)) %>% # if a flag exists, then remove data
    dplyr::filter(STATION %in% station_id) %>%
    dplyr::rename(Date = DATE) %>%
    dplyr::filter(Date > start_date) %>%
    dplyr::select(STATION, Date, tmax) %>%
    pivot_wider(names_from = STATION, values_from = tmax)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # NRCS data
  station_id <- tbl_station %>% dplyr::select(stationTriplet) %>% drop_na() %>% pull()
  
  nrcs_data <- read_csv(file.path(db_path_data, "nrcs", "tmax.csv")) %>%
    dplyr::filter(Station_ID %in% station_id) %>%
    dplyr::select(Station_ID, Date, tmax_F) %>%
    dplyr::filter(Date > start_date) %>%
    pivot_wider(names_from = Station_ID, values_from = tmax_F) %>%
    dplyr::mutate(`395:OR:SNTL` = if_else(Date == lubridate::mdy("12/12/2017") , -99, `395:OR:SNTL`)) # anomalous data causing Draper.exe to crash
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Merge all together -----------------------------------------
  tbl_new <- left_join(noaa_data, nrcs_data, by = "Date") %>%
    dplyr::filter(Date > start_date)
  
  # merge to master data file
  tbl <- full_join(tbl_tmax, tbl_new)
  
  tbl_dates <- tbl %>%
    dplyr::select(Date) %>%
    dplyr::mutate(YY = lubridate::year(Date),
                  MM = lubridate::month(Date),
                  DD = lubridate::day(Date),
                  hh = 0,
                  mm = 0,
                  ss = 0) 
  
  tbl <- distinct(tbl, Date, .keep_all = TRUE) # remove any duplicate dates
  tbl_append <- left_join(tbl_dates, tbl, by = "Date") %>% # Append date columns
    dplyr::arrange(tbl, Date) %>% # sort Date 
    dplyr::select(-Date) %>%
    replace(is.na(.), -99) # Catch any missed NAs
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Recopy original file and append new data
  if(file.exists(file.path(db_path,"model_files/extension/data/KLAMATH/TMAX/KLAMATH_MEAS_TMAX.data"))){
    file.copy(from = file.path(db_path,"model_files/extension/data/KLAMATH/TMAX/Original/KLAMATH_MEAS_TMAX_orig.data"),
              to = file.path(db_path,"model_files/extension/data/KLAMATH/TMAX/KLAMATH_MEAS_TMAX_temp.data"))
    file.rename(from = file.path(db_path,"model_files/extension/data/KLAMATH/TMAX",
                                 "KLAMATH_MEAS_TMAX_temp.data"), 
                to = file.path(db_path,"model_files/extension/data/KLAMATH/TMAX",
                               "KLAMATH_MEAS_TMAX.data"))
  } else {}
  
  fwrite(tbl_append,
         file = file.path(db_path,"model_files/extension/data/KLAMATH/TMAX/KLAMATH_MEAS_TMAX.data"),
         sep = " ", col.names=FALSE,
         append=TRUE)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Plot timeseries to double check appended correctly
  old_data <- read_csv(file.path(db_path,"model_files/extension/data/KLAMATH/TMAX/KLAMATH_MEAS_TMAX.data"), skip = 7) %>%
    separate(names(read_csv(file.path(db_path,"model_files/extension/data/KLAMATH/TMAX/KLAMATH_MEAS_TMAX.data"), skip = 7)),
             c("YY", "MM", "DD", "hh", "mm", "ss", names(tbl_ppt)[2:ncol(tbl_ppt)]),
             sep = " ")
  
  old_data <- old_data %>% 
    dplyr::mutate(Date = lubridate::mdy(paste(MM,"/",DD,"/",YY))) %>%
    dplyr::select(-YY,-MM,-DD,-hh,-mm,-ss) %>%
    pivot_longer(!Date, names_to="Station", values_to = "Results") %>%
    dplyr::mutate(Results = as.numeric(Results))
  
  tbl_plot <- tbl %>% 
    pivot_longer(!Date, names_to="Station", values_to = "Results") %>%
    full_join(old_data) %>%
    mutate(Results = ifelse(Results == -99,NA,Results)) %>%
    dplyr::arrange(Date) # sort Date
  
  sta_list <- unique(tbl_plot$Station[!is.na(tbl_plot$Station)])
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  for (i in 1:length(sta_list)) {
    nm <- tbl_plot %>% filter(Station == sta_list[i])
    
    g <- ggplot(nm) + geom_line(aes(x = Date, y = Results)) +
      labs(x = 'Date', y = 'Temperature (F)')
    
    ggsave(file.path(db_path,"model_files/extension/data/KLAMATH/TMAX/figures",
                     glue::glue("{make.names(nm$Station[1])}_TMAX.png")), g, height = 5, width = 10)
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------
Load_TMax()





#===========================================================================================
# Loads minimum temperature data
#===========================================================================================
Load_TMin <- function()
{
  
  # ------------------------------------------------------------------------------------------------
  ######  ------ Minimum Temperature ------- #########
  
  # Initialize dataframe
  tbl_tmin <- data.frame(matrix(ncol = length(c(tbl_station$Station_ID))+1, nrow = 0))
  names(tbl_tmin) <- c("Date",tbl_station$Station_ID)
  tbl_tmin$Date <- as.Date(tbl_tmin$Date)
  # ------------------------------------------------------------------------------------------------
  
  
  max(read_csv(file.path(db_path_data, "noaa", "tmin.csv"))$DATE)
  # ------------------------------------------------------------------------------------------------
  # NOAA data
  station_id <- tbl_station %>% dplyr::select(stationid) %>% drop_na() %>% pull(stationid)
  
  noaa_data <- read_csv(file.path(db_path_data, "noaa", "tmin.csv")) %>%
    dplyr::mutate(tmin = VALUE*0.1*(9/5) + 32) %>% # convert C to F
    dplyr::mutate(tmin = case_when(is.na(Q_FLAG) == FALSE ~ NA,
                                   TRUE ~ tmin)) %>% # if a flag exists, then remove data
    dplyr::filter(STATION %in% station_id) %>%
    dplyr::rename(Date = DATE) %>%
    dplyr::filter(Date > start_date) %>%
    dplyr::select(STATION, Date, tmin) %>%
    pivot_wider(names_from = STATION, values_from = tmin)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # NRCS data
  station_id <- tbl_station %>% dplyr::select(stationTriplet) %>% drop_na() %>% pull()
  
  nrcs_data <- read_csv(file.path(db_path_data, "nrcs", "tmin.csv")) %>%
    dplyr::filter(Station_ID %in% station_id) %>%
    dplyr::select(Station_ID, Date, tmin_F) %>%
    dplyr::filter(Date > start_date) %>%
    pivot_wider(names_from = Station_ID, values_from = tmin_F)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Merge all together -----------------------------------------
  tbl_new <- left_join(noaa_data, nrcs_data, by = "Date") %>%
    dplyr::filter(Date > start_date)
  
  # merge to master data file
  tbl <- full_join(tbl_tmin, tbl_new)
  
  tbl_dates <- tbl %>%
    dplyr::select(Date) %>%
    dplyr::mutate(YY = lubridate::year(Date),
                  MM = lubridate::month(Date),
                  DD = lubridate::day(Date),
                  hh = 0,
                  mm = 0,
                  ss = 0) 
  
  tbl <- distinct(tbl, Date, .keep_all = TRUE) # remove any duplicate dates
  tbl_append <- left_join(tbl_dates, tbl, by = "Date") %>% # Append date columns
    dplyr::arrange(tbl, Date) %>% # sort Date 
    dplyr::select(-Date) %>%
    replace(is.na(.), -99) # Catch any missed NAs
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Recopy original file and append new data
  if(file.exists(file.path(db_path,"model_files/extension/data/KLAMATH/TMIN/KLAMATH_MEAS_TMIN.data"))){
    file.copy(from = file.path(db_path,"model_files/extension/data/KLAMATH/TMIN/Original/KLAMATH_MEAS_TMIN_orig.data"),
              to = file.path(db_path,"model_files/extension/data/KLAMATH/TMIN/KLAMATH_MEAS_TMIN_temp.data"))
    file.rename(from = file.path(db_path,"model_files/extension/data/KLAMATH/TMIN",
                                 "KLAMATH_MEAS_TMIN_temp.data"), 
                to = file.path(db_path,"model_files/extension/data/KLAMATH/TMIN",
                               "KLAMATH_MEAS_TMIN.data"))
  } else {}
  
  fwrite(tbl_append,
         file = file.path(db_path,"model_files/extension/data/KLAMATH/TMIN/KLAMATH_MEAS_TMIN.data"),
         sep = " ", col.names=FALSE,
         append=TRUE)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Plot timeseries to double check appended correctly
  old_data <- read_csv(file.path(db_path,"model_files/extension/data/KLAMATH/TMIN/KLAMATH_MEAS_TMIN.data"), skip = 7) %>%
    separate(names(read_csv(file.path(db_path,"model_files/extension/data/KLAMATH/TMIN/KLAMATH_MEAS_TMIN.data"), skip = 7)),
             c("YY", "MM", "DD", "hh", "mm", "ss", names(tbl_ppt)[2:ncol(tbl_ppt)]),
             sep = " ")
  
  old_data <- old_data %>% 
    dplyr::mutate(Date = lubridate::mdy(paste(MM,"/",DD,"/",YY))) %>%
    dplyr::select(-YY,-MM,-DD,-hh,-mm,-ss) %>%
    pivot_longer(!Date, names_to="Station", values_to = "Results") %>%
    dplyr::mutate(Results = as.numeric(Results))
  
  tbl_plot <- tbl %>% 
    pivot_longer(!Date, names_to="Station", values_to = "Results") %>%
    full_join(old_data) %>%
    mutate(Results = ifelse(Results == -99,NA,Results)) %>%
    dplyr::arrange(Date) # sort Date
  
  sta_list <- unique(tbl_plot$Station[!is.na(tbl_plot$Station)])
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  for (i in 1:length(sta_list)) {
    nm <- tbl_plot %>% filter(Station == sta_list[i])
    
    g <- ggplot(nm) + geom_line(aes(x = Date, y = Results)) +
      labs(x = 'Date', y = 'Temperature (F)')
    
    ggsave(file.path(db_path,"model_files/extension/data/KLAMATH/TMIN/figures",
                     glue::glue("{make.names(nm$Station[1])}_TMIN.png")), g, height = 5, width = 10)
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------
Load_TMin()





#===========================================================================================
# Loads flow data
#===========================================================================================
Load_Flow <- function()
{
  # ------------------------------------------------------------------------------------------------
  ######  ------ Streamflow ------- #########
  # Initialize dataframe
  tbl_flow <- data.frame(matrix(ncol = length(c(tbl_flow_station$Station_ID))+1, nrow = 0))
  names(tbl_flow) <- c("Date",tbl_flow_station$Station_ID)
  tbl_flow$Date <- as.Date(tbl_flow$Date)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Select USGS stations
  station_id <- tbl_flow_station %>% dplyr::filter(index <= 16) %>% dplyr::select(Station_ID) %>% pull(Station_ID)
  
  temp = list.files(full.names = TRUE, file.path(db_path_data, "nwis","PRMS"), pattern="*0.csv")
  myfiles = lapply(temp, function(n) {
    read_csv(n, col_types = "ccDdc")
  })
  
  nwis_data <- bind_rows( myfiles ) %>%
    dplyr::select(-agency_cd, -Flow_cd, -dateTime, -tz_cd) %>%
    dplyr::rename(Station = site_no) %>%
    dplyr::filter(Date > start_date) %>%
    pivot_wider(names_from = Station, values_from = Flow ) 
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # NRCS data - SWE data
  station_id <- tbl_flow_station %>% dplyr::filter(index > 16 & index <= 34 & index != 29) %>% dplyr::select(Station_ID) %>% pull()
  
  nrcs_data <- read_csv(file.path(db_path_data, "nrcs", "wteq.csv")) %>%
    dplyr::filter(Station_ID %in% station_id) %>%
    dplyr::select(Station_ID, Date, wteq_F) %>% # wse is in terms of inches
    dplyr::filter(Date > start_date) %>%
    pivot_wider(names_from = Station_ID, values_from = wteq_F)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Merge all together -----------------------------------------
  tbl_new <- left_join(nwis_data, nrcs_data, by = "Date") %>%
    dplyr::filter(Date > start_date)
  
  # merge to master data file
  tbl <- full_join(tbl_flow, tbl_new)
  
  tbl_dates <- tbl %>%
    dplyr::select(Date) %>%
    dplyr::mutate(YY = lubridate::year(Date),
                  MM = lubridate::month(Date),
                  DD = lubridate::day(Date),
                  hh = 0,
                  mm = 0,
                  ss = 0) 
  
  tbl <- distinct(tbl, Date, .keep_all = TRUE) # remove any duplicate dates
  tbl_append <- left_join(tbl_dates, tbl, by = "Date") %>% # Append date columns
    dplyr::arrange(tbl, Date) %>% # sort Date 
    dplyr::select(-Date) %>%
    replace(is.na(.), -99) # Catch any missed NAs
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Recopy original file and append new data
  if(file.exists(file.path(db_path,"model_files/extension/data/Streamflow/Klam.flow.data"))){
    file.copy(from = file.path(db_path,"model_files/extension/data/Streamflow/Original/Klam.flow_orig.data"),
              to = file.path(db_path,"model_files/extension/data/Streamflow/Klam.flow_temp.data"))
    file.rename(from = file.path(db_path,"model_files/extension/data/Streamflow",
                                 "Klam.flow_temp.data"), 
                to = file.path(db_path,"model_files/extension/data/Streamflow",
                               "Klam.flow.data"))
  } else {}
  
  fwrite(tbl_append,
         file = file.path(db_path,"model_files/extension/data/Streamflow/Klam.flow.data"),
         sep = " ", col.names=FALSE,
         append=TRUE)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Plot timeseries to double check appended correctly
  old_data <- read_csv(file.path(db_path,"model_files/extension/data/Streamflow/Klam.flow.data"), skip = 49) %>%
    separate(names(read_csv(file.path(db_path,"model_files/extension/data/Streamflow/Klam.flow.data"), skip = 49)),
             c("YY", "MM", "DD", "hh", "mm", "ss", names(tbl_flow)[2:ncol(tbl_flow)]),
             sep = " ")
  
  old_data <- old_data %>% 
    dplyr::mutate(Date = lubridate::mdy(paste(MM,"/",DD,"/",YY))) %>%
    dplyr::select(-YY,-MM,-DD,-hh,-mm,-ss) %>%
    pivot_longer(!Date, names_to="Station", values_to = "Results") %>%
    dplyr::mutate(Results = as.numeric(Results))
  
  tbl_plot <- tbl %>% 
    pivot_longer(!Date, names_to="Station", values_to = "Results") %>%
    full_join(old_data) %>%
    mutate(Results = ifelse(Results == -99,NA,Results)) %>%
    dplyr::arrange(Date) # sort Date 
  
  sta_list <- unique(tbl_plot$Station[!is.na(tbl_plot$Station)])
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  for (i in 1:16) {
    nm <- tbl_plot %>% filter(Station == sta_list[i])
    
    g <- ggplot(nm) + geom_line(aes(x = Date, y = Results)) +
      labs(x = 'Date', y = 'Streamflow (cfs)')
    
    # ggsave(file.path(db_path,"model_files/extension/data/Streamflow/figures",
    #                  glue::glue("{make.names(nm$Station[1])}_flow.png")), g, height = 5, width = 10)
    # 
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  for (i in 17:length(sta_list)) {
    nm <- tbl_plot %>% filter(Station == sta_list[i])
    
    g <- ggplot(nm) + geom_line(aes(x = Date, y = Results)) +
      labs(x = 'Date', y = 'SWE (in)')
    
    ggsave(file.path(db_path,"model_files/extension/data/Streamflow/figures",
                     glue::glue("{make.names(nm$Station[1])}_swe.png")), g, height = 5, width = 10)
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------
Load_Flow()













#===========================================================================================
# copies DRAPER and Klamath flow files to the model directory
# to be done after the DRAPER program is run to update the files
#===========================================================================================
Copy_DRAPER_Klam_Flow <- function ()
{
  
  # ------------------------------------------------------------------------------------------------
  # Paths
  DRAPER_PPT_Path <- file.path(db_path,"model_files/extension/data/KLAMATH/PPT/KLAMATH_DRAPER_PPT.data")
  DRAPER_TMAX_Path <- file.path(db_path,"model_files/extension/data/KLAMATH/TMAX/KLAMATH_DRAPER_TMAX.data")
  DRAPER_TMIN_Path <- file.path(db_path,"model_files/extension/data/KLAMATH/TMIN/KLAMATH_DRAPER_TMIN.data")
  KLAM_FLOW_Path <- file.path(db_path,"model_files/extension/data/Streamflow/Klam.flow.data")
  dest <- file.path(db_path, "model_files/extension/model_files/prms_work/input")
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # removes old data
  unlink(file.path(dest,'KLAMATH_DRAPER_PPT.data'),
         recursive = TRUE)
  unlink(file.path(dest,'KLAMATH_DRAPER_TMAX.data'),
         recursive = TRUE)
  unlink(file.path(dest,'KLAMATH_DRAPER_TMIN.data'),
         recursive = TRUE)
  unlink(file.path(dest,'Klam.flow.data'),
         recursive = TRUE)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Moves new data to directory model pulls from
  file.copy(from = DRAPER_PPT_Path,
            to = dest)
  file.copy(from = DRAPER_TMAX_Path,
            to = dest)
  file.copy(from = DRAPER_TMIN_Path,
            to = dest)
  file.copy(from = KLAM_FLOW_Path,
            to = dest)
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------


