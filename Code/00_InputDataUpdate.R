# Larry Walker Associates
# 2023-2024



#===========================================================================================
# get_gsp_data
# placeholder explanation to be filled in as Chris learns more about these functions
#===========================================================================================
get_gsp_data_f <- function(
    area, # Name of studied areas
    path_gsp_data, # Path to inputs in shared folder
    path_prms_data, # Path to outputs
    run_cdec_data=T, # TRUE or FALSE for downloading CDEC data
    plot_cdec_data=T, # TRUE or FALSE for plotting CDEC data
    run_nwis_data=T, # TRUE or FALSE for downloading NWIS data
    plot_nwis_data=T, # TRUE or FALSE for plotting NWIS data
    run_cimis_data=T, # TRUE or FALSE for downloading CIMIS data
    run_noaa_data=T, # TRUE or FALSE for downloading NOAA data
    get_prism_data=T, # TRUE or FALSE for getting PRISM data
    get_nrcs_data=T, # only used in Butte
    start_date="1985-10-01", # Start date that users want
    end_date=today(), # Update to current date or any date that users want
    start_date_prism=NULL, # Start date of PRISM data
    end_date_prism=NULL) # End date of PRISM data

{ 
  # ------------------------------------------------------------------------------------------------
  #####***SET WORKING DIRECTORY TO SOURCE FILE LOCATION***#####
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # sets end_date to last completed calendar month
  end_date <- today() - as.numeric(strsplit(as.character(today()),'-')[[1]][3])
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Setting location of prms data path and gsp data path
  username=Sys.info()[["user"]]
  dropbox_dir=paste0("C:/Users/",username,"/LWA Dropbox/")
  path_prms_data=paste0(dropbox_dir,"00_Project-Repositories/00598-PRMS-Modeling/butte/data/")
  path_gsp_data=paste0(dropbox_dir,"00_Project-Repositories/00598-Siskiyou-GSP-data/")
  # ------------------------------------------------------------------------------------------------
  # path_prms_data <- path # "C:/Users/ChristopherDory/LWA Dropbox/00_Project-Repositories/00598-PRMS-Modeling/butte/data/"
  # path_gsp_data <- path # "C:/Users/ChristopherDory/LWA Dropbox/00_Project-Repositories/00598-Siskiyou-GSP-data/"
  # area <- 'butte'
  
  # ------------------------------------------------------------------------------------------------
  # Test all the necessary packages
  pkgTest("readr")
  pkgTest("data.table")
  pkgTest("readxl")
  pkgTest("dataRetrieval")
  pkgTest("cder")
  pkgTest("lubridate")
  pkgTest("curl")
  pkgTest("stringr")
  pkgTest("raster")
  pkgTest("prism")
  pkgTest("dplyr")
  pkgTest("cimir")
  pkgTest("rgeos")
  pkgTest("sp")
  #pkgTest("rgdal")
  pkgTest("rnoaa")
  pkgTest("curl")
  pkgTest("tidyr")
  # ------------------------------------------------------------------------------------------------
  options(scipen = 999)
  
  area_name=str_to_title(area) # set the area name
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  ##### Download CDEC data #####
  # if we want to download cdec data
  if(run_cdec_data==F){}else{
    
    get_cdec_data_f(path_gsp_data,
                    path_prms_data,
                    area,
                    start_date,
                    end_date)
    
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  ##### Download NWIS data #####
  if(run_nwis_data==F){}else{
    
    get_cdec_data_f(path_gsp_data,
                    path_prms_data,
                    area,
                    start_date,
                    end_date)
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  ##### Download CIMIS data #####
  # Requires CIMIS Token
  # (free online: https://cimis.water.ca.gov/)
  if (run_cimis_data==T){
    
    get_cimis_data_f(path_gsp_data,
                     path_prms_data,
                     area,
                     start_date,
                     end_date)
    
    
  } else {}
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  ##### Download NOAA data #####
  # Weather data requires NOAA CDO token
  # (free online: https://www.ncdc.noaa.gov/cdo-web/webservices/v2)
  if(run_noaa_data==F){}else{
    
    get_noaa_data_f(path_gsp_data,
                    path_prms_data,
                    area,
                    start_date,
                    end_date)
    
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  ##### Download NRCS data #####
  ## Not being added to summary lists yet ##
  if (get_nrcs_data==T & area=="butte"){
    
    get_nrcs_data_f(path_gsp_data,
                    path_prms_data,
                    area,
                    start_date,
                    end_date)
    
  } else {}
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  ##### Combine CDEC, NWIS, CIMIS, NOAA data #####
  flag_cdec=file.exists(file.path(path_prms_data,"cdec/cdec_download_data_summary.csv"))
  flag_nwis=file.exists(file.path(path_prms_data,"nwis/nwis_download_data_summary.csv"))
  flag_cimis=file.exists(file.path(path_prms_data,"cimis/cimis_download_data_summary.csv"))
  flag_noaa=file.exists(file.path(path_prms_data,"noaa/noaa_download_data_summary.csv"))
  
  if(flag_cdec==T&flag_nwis==T&flag_cimis==T&flag_noaa==T){
    cdec_download_data_summary=read.csv(file.path(path_prms_data,"cdec/cdec_download_data_summary.csv"))
    nwis_download_data_summary=read.csv(file.path(path_prms_data,"nwis/nwis_download_data_summary.csv"))
    cimis_download_data_summary=read.csv(file.path(path_prms_data,"cimis/cimis_download_data_summary.csv"))
    noaa_download_data_summary=read.csv(file.path(path_prms_data,"noaa/noaa_download_data_summary.csv"))
    
    # download_data_summary=rbind.data.frame(cdec_download_data_summary,nwis_download_data_summary,
    #                                        cimis_download_data_summary, noaa_download_data_summary)
    # write.csv(download_data_summary,file.path(path_gsp_data,"outputs",area,"download_data_summary.csv"),row.names = F)
  } else {}
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  if(get_prism_data==T){
    
    get_prism_data_f(path_gsp_data,
                     path_prms_data,
                     area,
                     start_date,
                     end_date)
    
  } else {}
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------








#===========================================================================================
# Test whether packages are loaded and if not install them
#===========================================================================================
##### Load packages #####
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
# ------------------------------------------------------------------------------------------------





#===========================================================================================
# Tell R to pause so dont cause error when sending too many data requests too quickly
#===========================================================================================
pause_sec <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
# ------------------------------------------------------------------------------------------------







#===========================================================================================
# get CDEC data function
#===========================================================================================
get_cdec_data_f <- function(path_gsp_data,
                            path_prms_data,
                            area,
                            start_date,
                            end_date)
{
  
  
  # ------------------------------------------------------------------------------------------------
  # Load the list of sensors
  allsensors=read.csv(paste(path_gsp_data,"inputs/list of sensors in CDEC.csv",sep="")) # https://cdec.water.ca.gov/misc/senslist.html
  #allsensors=allsensors[match(unique(data_summary$variable_code),allsensors$Sensor_no),]
  colnames(allsensors)=c("variable_code","variable_name","unit")
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # How were this files created? By hand?
  # https://cdec.water.ca.gov/dynamicapp/staSearch
  area_stations=read.csv(paste(path_gsp_data,"inputs/",area,"_cdec_stations.csv",sep=""))
  area_stations=subset(area_stations,select=c(STA,Station.Na,Agency.Nam,Latitude,Longitude))
  colnames(area_stations)=c("station_id","station_name","agency","latitude","longitude")
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # set durations
  durations_list=cbind.data.frame(durations=c("D","H","E","M"),
                                  durations_name=c("","_hourly","_event","_monthly"))
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  for (d in 1:nrow(durations_list)){
    durations=durations_list[d,"durations"]
    durations_name=durations_list[d,"durations_name"]
    sensta=expand.grid(allsensors[,"variable_code"],area_stations[,"station_id"])
    colnames(sensta)=c("variable_code","station_id")
    
    pause_sec(10) # Create a pause so curl doesn't time out - dependent on internet connection?
    
    # ------------------------------------------------------------------------------------------------
    # For shasta add sensor stations
    if(area=="shasta"){
      # Outside watershed snow stations
      added_sensta=rbind.data.frame(c(82,"SVB"),c(3,"SVB"),
                                    c(82,"SVG"),c(3,"SVG"),
                                    c(82,"SDF"),c(3,"SDF"),
                                    c(82,"PET"),c(3,"PET"),
                                    c(82,"LSH"),c(3,"LSH"),
                                    c(82,"PRK"),c(3,"PRK"),
                                    c(82,"SWT"),c(3,"SWT"),
                                    c(82,"BWR"),c(3,"BWR"),
                                    c(82,"DDF"),c(3,"DDF"),
                                    c(82,"MSH"),c(3,"MSH"),
                                    c(82,"NFS"),c(3,"NFS"),
                                    c(82,"SFT"),c(3,"SFT"),
                                    c(82,"ASH"),c(3,"ASH"))
      colnames(added_sensta)=c("variable_code","station_id")
      colnames(added_sensta)=colnames(sensta)
      sensta=rbind.data.frame(sensta,added_sensta)
    } else {}
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # Initializes the progress bar
    n_iter=nrow(sensta)
    pb <- winProgressBar(title = "Windows progress bar", # Window title
                         label = "Percentage completed", # Window label
                         min = 0,      # Minimum value of the bar
                         max = n_iter, # Maximum value of the bar
                         initial = 0,  # Initial value of the bar
                         width = 300L) # Width of the window
    # ------------------------------------------------------------------------------------------------
    
    
    
    # ------------------------------------------------------------------------------------------------
    # for each sensor station
    for (i in 1:nrow(sensta)){
      variable_code=sensta[i,"variable_code"] # what code is to be loaded
      variable_name=allsensors[match(variable_code,allsensors$variable_code),"variable_name"] # what is the variable name of that code
      unit=allsensors[match(variable_code,allsensors$variable_code),"unit"] # what are the units of the variable
      station_id=sensta[i,"station_id"]
      browser()
      df3=cder::cdec_query(station_id,variable_code,durations=durations,
                           start.date = start_date,
                           end.date = end_date)
      df3=as.data.frame(df3)
      colnames(df3)[5]="Date"
      
      
      # ------------------------------------------------------------------------------------------------
      # If data for that sensor
      if(nrow(df3)==0){}else{ 
        
        # Create output directory
        dir.create(file.path(path_gsp_data,"outputs",area,"cdec",variable_name))
        
        # Save results
        write.csv(df3,file.path(path_gsp_data,"outputs",area,"cdec",variable_name,paste(station_id,durations_name,".csv",sep="")),row.names = F)
      }
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # Update progress bar
      pctg <- paste(round(i/n_iter *100, 0), "% completed for CDEC in",area_name)
      setWinProgressBar(pb, i, label = pctg)
      # ------------------------------------------------------------------------------------------------
    }
    close(pb)
}
# ------------------------------------------------------------------------------------------------

  

  
  
  
  
  
#===========================================================================================
# get NWIS data function
#===========================================================================================
get_nwis_data_f <- function(path_gsp_data,
                            path_prms_data,
                            area,
                            start_date,
                            end_date)
{
  
  
  # ------------------------------------------------------------------------------------------------
  # For Butte PRMS, the prior code in the else section doesn't work for purposes
  if (area == "butte") { 
    
    # ------------------------------------------------------------------------------------------------
    # Get station list
    stations <- read_csv(file.path(path_prms_data,"nwis","PRMS","Butte_monitoring_stations.csv")) %>%
      dplyr::filter(Index <= 16) %>%
      dplyr::pull(Station_ID)
    # ------------------------------------------------------------------------------------------------
    
    
    
    # ------------------------------------------------------------------------------------------------
    # Use length() to get number of elements, then loop through each station.
    for (i in 1:length(stations)) {
      df5=readNWISdv(siteNumbers=stations[i],parameterCd="00060",
                     startDate=start_date,endDate=end_date)
      df5=dataRetrieval::renameNWISColumns(df5)
      
      write.csv(df5,file.path(path_prms_data,"nwis","PRMS",glue::glue("{stations[i]}.csv")),row.names = F) 
    }
    # ------------------------------------------------------------------------------------------------
    
    
  # ------------------------------------------------------------------------------------------------
  } else {
    
    
    # ------------------------------------------------------------------------------------------------
    # Get station list
    stations <- read_csv(file.path(path_gsp_data,"outputs",area,"nwis","nwis_monitoring_stations.csv")) %>%
      dplyr::pull(Station_ID)
    # ------------------------------------------------------------------------------------------------
    
    
    
    # ------------------------------------------------------------------------------------------------
    # Use length() to get number of elements, then loop through each station.
    # QAQC codes: https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd
    for (i in 1:length(stations)) {
      df5=readNWISdv(siteNumbers=stations[i],parameterCd="00060",
                     startDate=start_date,endDate=end_date)
      df5=dataRetrieval::renameNWISColumns(df5)
      
      write.csv(df5,file.path(path_prms_data,"nwis","Daily_Mean_Discharge_CFS",glue::glue("{stations[i]}.csv")),row.names = F) 
    }
    # ------------------------------------------------------------------------------------------------
  } 
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------ 
  
  
  
  
  
  
  
  
  
#===========================================================================================
# get CIMIS data function
#===========================================================================================
get_cimis_data_f <- function(path_gsp_data,
                             path_prms_data,
                             area,
                             start_date,
                             end_date)
{
  
  
  # ------------------------------------------------------------------------------------------------
  # cimis_key <- read_lines(file=paste0(path_gsp_data,"inputs/CIMIS_API_key.txt"))
  cimis_key <- "1886d2ea-2579-41c5-bab2-0451dbdcd25b" # Contact CIMIS if not working
  cimir::set_key(cimis_key)
  # ------------------------------------------------------------------------------------------------
  
  area_name <- area
  
  # ------------------------------------------------------------------------------------------------
  cimis_station_list=cimis_station()
  #cimis_station_list=cimis_station_list[cimis_station_list$County=="Siskiyou",] 
  cimis_station_list$latitude=as.numeric(sub(".*/ ","",cimis_station_list$HmsLatitude))
  cimis_station_list$longitude=as.numeric(sub(".*/ ","",cimis_station_list$HmsLongitude))
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # lat lon points
  cimis_station_points=cimis_station_list[,c("longitude","latitude")]
  
  coordinates(cimis_station_points) <- ~ longitude + latitude
  proj4string(cimis_station_points) <- CRS("+proj=longlat")
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # get the basin of the area of interest
  huc8_basin_dir=paste0(path_gsp_data,"inputs/QGIS preprocessing/inputs/HU8 CA watersheds/")
  huc8_basin=shapefile(paste0(huc8_basin_dir,"HUC8_",area_name,".shp"))
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # project points to basin crs and set crs
  cimis_station_points <- spTransform(cimis_station_points, proj4string(huc8_basin))
  new_crs = proj4string(huc8_basin)
  proj4string(huc8_basin) <- new_crs
  proj4string(cimis_station_points) <- new_crs
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # get selected stations
  selected_cimis_stations=over(cimis_station_points, huc8_basin)
  
  selected_cimis_stations=cbind.data.frame(cimis_station_list,OBJECTID=selected_cimis_stations$OBJECTID)
  selected_cimis_stations=selected_cimis_stations[complete.cases(selected_cimis_stations$OBJECTID),]
  selected_cimis_stations=subset(selected_cimis_stations,select=-OBJECTID)
  # ------------------------------------------------------------------------------------------------
  

  
  
  
  # ------------------------------------------------------------------------------------------------
  # if selected cimis stations exist
  if (exists("selected_cimis_stations")==F){} else{
    # ------------------------------------------------------------------------------------------------
    # if there are no selected stations dont run it
    if(nrow(selected_cimis_stations)==0){
      run_cimis_data=F
    } else {
      # ------------------------------------------------------------------------------------------------
      # Otherwise if stations have been selected, create output directories
      dir.create(file.path(path_prms_data,"cimis"))
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # Download data by year due to data constraints
      years_seq=seq(year(start_date),year(end_date),by=1)
      count = 0 # Track when to start saving data to the master table
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      for (year in years_seq){
        #dir.create(file.path(path_gsp_data,"outputs",area,"cimis",year))
        # ------------------------------------------------------------------------------------------------
        
        
        
        # ------------------------------------------------------------------------------------------------
        # if the year is the end year then set the end date to todays date minus half a month
        if (year==years_seq[length(years_seq)]){
          start_date_cimis=ymd(paste0(year,"-01-01"))
          end_date_cimis=today()-15
        } else {
          start_date_cimis=ymd(paste0(year,"-01-01"))
          end_date_cimis=ymd(paste0(year,"-12-31"))
        }
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        targets=unique(selected_cimis_stations$StationNbr)
        # ------------------------------------------------------------------------------------------------
        
        
        
        # ------------------------------------------------------------------------------------------------
        if(is_key_set()) {
          pause_sec(90) # Add pause between data requests to help prevent rejection of the app key from too many requests
          # use day-eto, which the Scott Valley MODFLOW model also uses
          cat("targets: ",targets, ", start_date_cimis: ", as.character(start_date_cimis), 
              ", end_date_cimis: ",as.character(end_date_cimis) ,"\n")
          
          cimis_data=cimis_data(targets,start.date = start_date_cimis,
                                end.date=end_date_cimis,
                                items=c("day-precip","day-air-tmp-min",
                                        "day-air-tmp-max", #"day-air-tmp-avg",
                                        "day-eto","day-sol-rad-avg"), 
                                measure.unit = "E") 
          
        } else {}
        # ------------------------------------------------------------------------------------------------
        
        
        
        
        # ------------------------------------------------------------------------------------------------
        # if data table is empty or not for the calendar year
        # Start saving data to the master table - advance count every time
        if (nrow(cimis_data)==0){} else { 
          count = count + 1 
          
          cimis_data$Unit <- str_replace_all(cimis_data$Unit, "[^[:alnum:]]", "")
          
          ppt=cimis_data[cimis_data$Item=="DayPrecip",]
          tmin=cimis_data[cimis_data$Item=="DayAirTmpMin",]
          #tmean=cimis_data[cimis_data$Item=="DayAirTmpAvg",]
          tmax=cimis_data[cimis_data$Item=="DayAirTmpMax",]
          sorad=cimis_data[cimis_data$Item=="DaySolRadAvg",]
          eto=cimis_data[cimis_data$Item=="DayAsceEto",]
          
          # Save data to one table
          if (count == 1) {
            #cat("I am here - first if - count: ", count,"\n")
            ppt_master=ppt
            tmin_master=tmin
            #tmean_master=tmean
            tmax_master=tmax
            sorad_master=sorad
            eto_master=eto
          } else {
            #cat("I am here - second if - count: ",count,"\n")
            
            ppt_master=rbind(ppt_master,ppt)
            tmin_master=rbind(tmin_master,tmin)
            #tmean_master=rbind(tmean_master,tmean)
            tmax_master=rbind(tmax_master,tmax)
            sorad_master=rbind(sorad_master,sorad)
            eto_master=rbind(eto_master,eto)
          }
          # ------------------------------------------------------------------------------------------------
        }
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # Save table to Dropbox
      write.csv(ppt_master,file.path(path_prms_data,"cimis","ppt.csv"),row.names = F)
      write.csv(tmin_master,file.path(path_prms_data,"cimis","tmin.csv"),row.names = F)
      write.csv(tmax_master,file.path(path_prms_data,"cimis","tmax.csv"),row.names = F)
      write.csv(sorad_master,file.path(path_prms_data,"cimis","sorad.csv"),row.names = F)
      write.csv(eto_master,file.path(path_prms_data,"cimis","eto.csv"),row.names = F)
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # Create station table - why repeated lines
      cimis_download_data_summary0=distinct(subset(selected_cimis_stations,select=c(StationNbr,Name,latitude,longitude)))
      colnames(cimis_download_data_summary0)[c(1,2)]=c("station_id","station_name")
      
      cimis_download_data_summary_ppt=cimis_download_data_summary0
      cimis_download_data_summary_ppt$variable_code="day-precip"
      cimis_download_data_summary_ppt$variable_name="Precipitation"
      cimis_download_data_summary_ppt$unit=unique(ppt_master$Unit)
      cimis_download_data_summary_ppt$agency="CIMIS"
      cimis_download_data_summary_ppt$start_date=min(ppt_master$Date)
      cimis_download_data_summary_ppt$end_date=max(ppt_master$Date)
      cimis_download_data_summary_ppt=cimis_download_data_summary_ppt[,c("variable_code","station_id","variable_name","unit","station_name","agency","latitude","longitude","start_date","end_date")]
      
      cimis_download_data_summary_tmin=cimis_download_data_summary0
      cimis_download_data_summary_tmin$variable_code="day-air-tmp-min"
      cimis_download_data_summary_tmin$variable_name="Air temperature minimum"
      cimis_download_data_summary_tmin$unit=unique(tmin_master$Unit)
      cimis_download_data_summary_tmin$agency="CIMIS"
      cimis_download_data_summary_tmin$start_date=min(tmin_master$Date)
      cimis_download_data_summary_tmin$end_date=max(tmin_master$Date)
      cimis_download_data_summary_tmin=cimis_download_data_summary_tmin[,c("variable_code","station_id","variable_name","unit","station_name","agency","latitude","longitude","start_date","end_date")]
      
      cimis_download_data_summary_tmax=cimis_download_data_summary0
      cimis_download_data_summary_tmax$variable_code="day-air-tmp-max"
      cimis_download_data_summary_tmax$variable_name="Air temperature maximum"
      cimis_download_data_summary_tmax$unit=unique(tmax_master$Unit)
      cimis_download_data_summary_tmax$agency="CIMIS"
      cimis_download_data_summary_tmax$start_date=max(tmax_master$Date)
      cimis_download_data_summary_tmax$end_date=max(tmax_master$Date)
      cimis_download_data_summary_tmax=cimis_download_data_summary_tmax[,c("variable_code","station_id","variable_name","unit","station_name","agency","latitude","longitude","start_date","end_date")]
      
      cimis_download_data_summary=rbind.data.frame(cimis_download_data_summary_ppt,
                                                   cimis_download_data_summary_tmin,
                                                   cimis_download_data_summary_tmax)
      # ------------------------------------------------------------------------------------------------
      write.csv(cimis_download_data_summary,file.path(path_prms_data,"cimis","cimis_download_data_summary.csv"),row.names = F)
    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------  

  
  
  
  
  
  
  
  
#===========================================================================================
# get NOAA data function
#===========================================================================================  
get_noaa_data_f <- function(path_gsp_data,
                            path_prms_data,
                            area,
                            start_date,
                            end_date)
{
  noaa_token <- read_lines(file=paste0(path_gsp_data,"inputs/NOAA_CDO_token.txt"))
  options(noaakey = noaa_token)
  
  
  # ------------------------------------------------------------------------------------------------
  # Load and Project
  noaa_station_list=ghcnd_stations()
  noaa_station_points=noaa_station_list[,c("longitude","latitude")]
  
  coordinates(noaa_station_points) <- ~ longitude + latitude
  proj4string(noaa_station_points) <- CRS("+proj=longlat") # WGS84
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # huc8 basin
  huc8_basin_dir=paste0(path_gsp_data,"inputs/QGIS preprocessing/inputs/HU8 CA watersheds/")
  huc8_basin=shapefile(paste0(huc8_basin_dir,"HUC8_",area_name,".shp"))
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  noaa_station_points <- spTransform(noaa_station_points, proj4string(huc8_basin))
  new_crs = proj4string(huc8_basin)
  proj4string(huc8_basin) <- new_crs
  proj4string(noaa_station_points) <- new_crs
  selected_noaa_stations=over(noaa_station_points, huc8_basin) # select stations within the watershed
  
  selected_noaa_stations=cbind.data.frame(noaa_station_list,OBJECTID=selected_noaa_stations$OBJECTID)
  selected_noaa_stations=selected_noaa_stations[complete.cases(selected_noaa_stations$OBJECTID),]
  selected_noaa_stations=subset(selected_noaa_stations,select=-OBJECTID)
  # ------------------------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  if(area=="butte"){
    # Klamath PRMS stations
    add_noaa_stations=c("USC00040161", "USC00351574", "USC00041990", "USC00351946", 
                        "USC00354060", "USC00044838", "USC00354403", "USC00354835", 
                        "USC00356426", "USC00357817", "USC00358173", "USC00049053")
    selected_list <- dplyr::filter(noaa_station_list, id %in% add_noaa_stations)
    selected_noaa_stations = rbind(selected_noaa_stations, selected_list)
  } else {}
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # if the noaa stations exists
  if (exists("selected_noaa_stations")==F){} else{
    # ------------------------------------------------------------------------------------------------
    # if there are no stations done run it
    if(nrow(selected_noaa_stations)==0){
      run_noaa_data=F
      # ------------------------------------------------------------------------------------------------
    } else if (run_noaa_data == T) {
      
      # Download data
      # https://search.r-project.org/CRAN/refmans/rnoaa/html/meteo_pull_monitors.html
      # The weather flags, which are kept by specifying keep_flags = TRUE are:
      #   ⁠*_mflag⁠: Measurement flag, which gives some information on how the observation was measured.
      #   ⁠*_qflag⁠: Quality flag, which gives quality information on the measurement, like if it failed to pass certain quality checks.
      #   ⁠*_sflag⁠: Source flag. This gives some information on the weather collection system (e.g., U.S. Cooperative Summary of the Day, Australian Bureau of Meteorology) the weather observation comes from.
      
      # Interpretation of flags: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
      
      
      # ------------------------------------------------------------------------------------------------
      # old r library no longer working - use script from Bill Rice
      monitors=unique(selected_noaa_stations$id)
      for (i in 1:length(monitors)) {
        if (i == 1) {
          noaa_data <- get_noaa_current(monitors[i])
        } else {
          noaa_data <- rbind(noaa_data, get_noaa_current(monitors[i]))
        }
      }
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # Old r library no longer working
      #noaa_data=meteo_pull_monitors(monitors=monitors, keep_flags = TRUE)
      if (nrow(noaa_data)==0){} else {
        ppt=subset(noaa_data, ELEMENT == "PRCP")
        tmin=subset(noaa_data, ELEMENT == "TMIN")
        tmax=subset(noaa_data, ELEMENT == "TMAX")
        
        write.csv(ppt,file.path(path_prms_data,"noaa","ppt.csv"),row.names = F)
        write.csv(tmin,file.path(path_prms_data, "noaa","tmin.csv"),row.names = F)
        write.csv(tmax,file.path(path_prms_data, "noaa","tmax.csv"),row.names = F)
      }
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      noaa_download_data_summary0=subset(selected_noaa_stations,select=c(id,name,latitude,longitude))
      colnames(noaa_download_data_summary0)[c(1,2)]=c("station_id","station_name")
      
      noaa_download_data_summary0=subset(selected_noaa_stations,select=c(id,name,latitude,longitude))
      noaa_download_data_summary0=unique(noaa_download_data_summary0)
      colnames(noaa_download_data_summary0)[c(1,2)]=c("station_id","station_name")
      
      noaa_download_data_summary_ppt=noaa_download_data_summary0
      noaa_download_data_summary_ppt$variable_code="PRCP"
      noaa_download_data_summary_ppt$variable_name="Precipitation"
      noaa_download_data_summary_ppt$unit="mm"
      noaa_download_data_summary_ppt$agency="noaa"
      noaa_download_data_summary_ppt$start_date=min(ppt$date)
      noaa_download_data_summary_ppt$end_date=max(ppt$date)
      noaa_download_data_summary_ppt=noaa_download_data_summary_ppt[,c("variable_code","station_id","variable_name","unit","station_name","agency","latitude","longitude","start_date","end_date")]
      
      noaa_download_data_summary_tmin=noaa_download_data_summary0
      noaa_download_data_summary_tmin$variable_code="TMIN"
      noaa_download_data_summary_tmin$variable_name="Air temperature minimum"
      noaa_download_data_summary_tmin$unit="C"
      noaa_download_data_summary_tmin$agency="noaa"
      noaa_download_data_summary_tmin$start_date=min(tmin$date)
      noaa_download_data_summary_tmin$end_date=max(tmin$date)
      noaa_download_data_summary_tmin=noaa_download_data_summary_tmin[,c("variable_code","station_id","variable_name","unit","station_name","agency","latitude","longitude","start_date","end_date")]
      
      noaa_download_data_summary_tmax=noaa_download_data_summary0
      noaa_download_data_summary_tmax$variable_code="TMAX"
      noaa_download_data_summary_tmax$variable_name="Air temperature maximum"
      noaa_download_data_summary_tmax$unit="C"
      noaa_download_data_summary_tmax$agency="noaa"
      noaa_download_data_summary_tmax$start_date=max(tmax$date)
      noaa_download_data_summary_tmax$end_date=max(tmax$date)
      noaa_download_data_summary_tmax=noaa_download_data_summary_tmax[,c("variable_code","station_id","variable_name","unit","station_name","agency","latitude","longitude","start_date","end_date")]
      
      noaa_download_data_summary=rbind.data.frame(noaa_download_data_summary_ppt,
                                                  noaa_download_data_summary_tmin,
                                                  noaa_download_data_summary_tmax)
      # ------------------------------------------------------------------------------------------------
      
      
      write.csv(noaa_download_data_summary,file.path(path_prms_data,"noaa","noaa_download_data_summary.csv"),row.names = F)
    }}
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------
  
  

  
  
  
#===========================================================================================
# get NRCS data function
#===========================================================================================   
get_nrcs_data_f <- function(path_gsp_data,
                            path_prms_data,
                            area,
                            start_date,
                            end_date)
{
  # Build request based on date
  date1_split <- str_split(start_date, "-")[[1]]
  date2_split <- str_split(end_date, "-")[[1]]
  
  # Get station list
  stations <- read_csv(file.path(path_prms_data,"nrcs","Butte_monitoring_stations.csv")) %>%
    drop_na(stationTriplet) %>%
    dplyr::pull(stationTriplet)
  
  
  # ------------------------------------------------------------------------------------------------
  # Use length() to get number of elements, then loop through each station and append data to master table.
  for (i in 1:length(stations)) {
    
    station_split <- str_split(stations[i], ":")[[1]]
    curl_req <- paste("https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/data?beginDate=",date1_split[2],
                      "%2F",date1_split[3],
                      "%2F",date1_split[1],
                      "%2000%3A00&centralTendencyType=NONE&duration=DAILY&elements=TMIN%2CTMAX%2CPRCP%2CWTEQ&endDate=",
                      date2_split[2],"%2F",date2_split[3],"%2F",date2_split[1],
                      "%2000%3A00%20&periodRef=END&returnFlags=false&returnOriginalValues=false&returnSuspectData=false&stationTriplets=",
                      station_split[1],"%3A",station_split[2],"%3A",station_split[3],
                      sep="", collapse = NULL)
    
    # Send request and download data
    tmp <- tempfile() 
    curl::curl_download(curl_req,tmp)
    df_json <- rjson::fromJSON(file = tmp)
    
    station <- df_json[[1]]$stationTriplet
    ppt_Date <- unname(sapply(df_json[[1]]$data[[1]]$values,`[[`,1))
    ppt_value <- unname(sapply(df_json[[1]]$data[[1]]$values,`[[`,2))
    tmax_Date <- unname(sapply(df_json[[1]]$data[[2]]$values,`[[`,1))
    tmax_value <- unname(sapply(df_json[[1]]$data[[2]]$values,`[[`,2))
    tmin_Date <- unname(sapply(df_json[[1]]$data[[3]]$values,`[[`,1))
    tmin_value <- unname(sapply(df_json[[1]]$data[[3]]$values,`[[`,2))
    wteq_Date <- unname(sapply(df_json[[1]]$data[[4]]$values,`[[`,1))
    wteq_value <- unname(sapply(df_json[[1]]$data[[4]]$values,`[[`,2))
    
    
    # ------------------------------------------------------------------------------------------------
    if (i == 1) {
      ppt <- data.frame(Date = ppt_Date, Station_ID = station, ppt_in = ppt_value) 
      tmax <- data.frame(Date = tmax_Date, Station_ID = station, tmax_F = tmax_value) 
      tmin <- data.frame(Date = tmin_Date, Station_ID = station, tmin_F = tmin_value)
      wteq <- data.frame(Date = wteq_Date, Station_ID = station, wteq_F = wteq_value)
    } else {
      ppt <-  rbind(ppt, data.frame(Date = ppt_Date, Station_ID = station, ppt_in = ppt_value)) 
      tmax <- rbind(tmax, data.frame(Date = tmax_Date, Station_ID = station, tmax_F = tmax_value)) 
      tmin <- rbind(tmin, data.frame(Date = tmin_Date, Station_ID = station, tmin_F = tmin_value))
      wteq <- rbind(wteq, data.frame(Date = wteq_Date, Station_ID = station, wteq_F = wteq_value))
    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  write.csv(ppt,file.path(path_prms_data,"nrcs","prcp.csv"),row.names = F)
  write.csv(tmin,file.path(path_prms_data,"nrcs","tmin.csv"),row.names = F)
  write.csv(tmax,file.path(path_prms_data,"nrcs","tmax.csv"),row.names = F)
  write.csv(wteq,file.path(path_prms_data,"nrcs","wteq.csv"),row.names = F)
}
# ------------------------------------------------------------------------------------------------
  
 
  
  
  
  
  
  
  
#===========================================================================================
# get PRISM data function
#===========================================================================================    
get_prism_data_f <- function(path_gsp_data,
                             path_prms_data,
                             area,
                             start_date,
                             end_date)
  # ------------------------------------------------------------------------------------------------
  # Station list
  station_list=read.csv(file.path(path_prms_data,"prism","prism_stations.csv"))
  station_list$latlon=paste(station_list$Latitude,station_list$Longitude,sep="_")
  station_list=subset(station_list,select=c("StationID","Station","Latitude","Longitude","latlon"))
  station_list=station_list[!duplicated(station_list$latlon),]
  station_list=subset(station_list,select=-latlon)
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  path_prism = paste0(path_prms_data)
  path_prism_ppt=paste(path_prism,"prism/Precipitation/",sep="")
  path_prism_tmin=paste(path_prism,"prism/Air temperature minimum/",sep="")
  path_prism_tmax=paste(path_prism,"prism/Air temperature maximum/",sep="")
  
  station_list_prism=station_list[,c("Longitude","Latitude")]
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Precipitation
  #dir.create(paste("./outputs/",area,"/prism/Precipitation/",sep=""))
  options(prism.path=path_prism_ppt)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  #if(update_prism_data==F|add_current_stations==T){
  ppt_stack = pd_stack(prism_archive_subset("ppt","daily",minDate = start_date_prism,maxDate = end_date_prism))
  #save(ppt_stack,file=paste0(path_gsp_data,"outputs/",area,"/prism/","ppt_stack.RData"))
  ppt_station_list = raster::extract(ppt_stack, station_list_prism)
  colnames(ppt_station_list)=gsub("_bil","",colnames(ppt_station_list))
  colnames(ppt_station_list)=gsub("PRISM_ppt_stable_4kmD2_","D_",colnames(ppt_station_list))
  ppt_station_list=data.frame(ppt_station_list)
  ppt_station_list=cbind.data.frame(StationID=station_list$StationID,station_list_prism,ppt_station_list)
  rownames(ppt_station_list)=1:nrow(ppt_station_list)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  ppt_station_list_l=split(ppt_station_list,ppt_station_list$StationID)
  for (m in 1:length(ppt_station_list_l)){
    df7=ppt_station_list_l[[m]]
    df7=subset(df7,select=-c(StationID,Longitude,Latitude))
    df7=t(df7)
    df7=cbind.data.frame(rownames(df7),df7)
    colnames(df7)=c("Date","Prcp_mm")
    df7 = df7 %>% 
      mutate(Status=case_when(
        str_detect(Date, "PRISM_ppt_early_4kmD2_") ~ "Early",
        str_detect(Date, "PRISM_ppt_provisional_4kmD2_") ~ "Provisional",
        str_detect(Date, "D_") ~ "Stable",
        TRUE ~ Date)) %>% 
      dplyr::mutate(Station = ppt_station_list_l[[m]]$StationID)											  
    df7$Date=gsub("D_","",df7$Date)
    df7$Date=gsub("PRISM_ppt_early_4kmD2_","",df7$Date)
    df7$Date=gsub("PRISM_ppt_provisional_4kmD2_","",df7$Date)
    df7$Date=as.Date(df7$Date,format="%Y%m%d")
    rownames(df7)=1:nrow(df7)
    df7=df7[order(df7$Date),]
    write.csv(df7,file.path(path_prms_data,"/prism/Precipitation/",paste(names(ppt_station_list_l)[m],".csv",sep="")),row.names = F)
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Air temperature minimum
  #dir.create(paste("./outputs/",area,"/prism/Air temperature minimum/",sep=""))
  options(prism.path=path_prism_tmin)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # if(update_prism_data==F|add_current_stations==T){
  tmin_stack = pd_stack(prism_archive_subset("tmin","daily",minDate = start_date_prism,maxDate = end_date_prism))
  #save(tmin_stack,file=paste0(path_gsp_data,"outputs/",area,"/prism/","tmin_stack.RData"))
  tmin_station_list = raster::extract(tmin_stack, station_list_prism)
  colnames(tmin_station_list)=gsub("_bil","",colnames(tmin_station_list))
  colnames(tmin_station_list)=gsub("PRISM_tmin_stable_4kmD2_","D_",colnames(tmin_station_list))
  tmin_station_list=data.frame(tmin_station_list)
  tmin_station_list=cbind.data.frame(StationID=station_list$StationID,station_list_prism,tmin_station_list)
  rownames(tmin_station_list)=1:nrow(tmin_station_list)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  tmin_station_list_l=split(tmin_station_list,tmin_station_list$StationID)
  for (n in 1:length(tmin_station_list_l)){
    df8=tmin_station_list_l[[n]]
    df8=subset(df8,select=-c(StationID,Longitude,Latitude))
    df8=t(df8)
    df8=cbind.data.frame(rownames(df8),df8)
    colnames(df8)=c("Date","Tmin_degC")
    df8 = df8 %>% 
      mutate(Status=case_when(
        str_detect(Date, "PRISM_tmin_early_4kmD2_") ~ "Early",
        str_detect(Date, "PRISM_tmin_provisional_4kmD2_") ~ "Provisional",
        str_detect(Date, "D_") ~ "Stable",
        TRUE ~ Date)) %>% 
      dplyr::mutate(Station = tmin_station_list_l[[n]]$StationID)													   
    df8$Date=gsub("D_","",df8$Date)
    df8$Date=gsub("PRISM_tmin_early_4kmD2_","",df8$Date)
    df8$Date=gsub("PRISM_tmin_provisional_4kmD2_","",df8$Date)
    df8$Date=as.Date(df8$Date,format="%Y%m%d")
    rownames(df8)=1:nrow(df8)
    df8=df8[order(df8$Date),]
    write.csv(df8,file.path(path_prms_data,"prism/Air temperature minimum",paste(names(tmin_station_list_l)[n],".csv",sep="")),row.names = F)
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Air temperature maximum
  #dir.create(paste("./outputs/",area,"/prism/Air temperature maximum/",sep=""))
  options(prism.path=path_prism_tmax)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  #    if(update_prism_data==F|add_current_stations==T){
  tmax_stack = pd_stack(prism_archive_subset("tmax","daily",minDate = start_date_prism,maxDate = end_date_prism))
  #save(tmax_stack,file=paste0(path_gsp_data,"outputs/",area,"/prism/","tmax_stack.RData"))
  tmax_station_list = raster::extract(tmax_stack, station_list_prism)
  colnames(tmax_station_list)=gsub("_bil","",colnames(tmax_station_list))
  colnames(tmax_station_list)=gsub("PRISM_tmax_stable_4kmD2_","D_",colnames(tmax_station_list))
  tmax_station_list=data.frame(tmax_station_list)
  tmax_station_list=cbind.data.frame(StationID=station_list$StationID,station_list_prism,tmax_station_list)
  rownames(tmax_station_list)=1:nrow(tmax_station_list)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  tmax_station_list_l=split(tmax_station_list,tmax_station_list$StationID)
  for (p in 1:length(tmax_station_list_l)){
    df10=tmax_station_list_l[[p]]
    df10=subset(df10,select=-c(StationID,Longitude,Latitude))
    df10=t(df10)
    df10=cbind.data.frame(rownames(df10),df10)
    colnames(df10)=c("Date","Tmax_degC")
    df10 = df10 %>% 
      mutate(Status=case_when(
        str_detect(Date, "PRISM_tmax_early_4kmD2_") ~ "Early",
        str_detect(Date, "PRISM_tmax_provisional_4kmD2_") ~ "Provisional",
        str_detect(Date, "D_") ~ "Stable",
        TRUE ~ Date)) %>% 
      dplyr::mutate(Station = tmax_station_list_l[[p]]$StationID)												   
    df10$Date=gsub("D_","",df10$Date)
    df10$Date=gsub("PRISM_tmax_early_4kmD2_","",df10$Date)
    df10$Date=gsub("PRISM_tmax_provisional_4kmD2_","",df10$Date)
    df10$Date=as.Date(df10$Date,format="%Y%m%d")
    rownames(df10)=1:nrow(df10)
    df10=df10[order(df10$Date),]
    write.csv(df10,file.path(path_prms_data,"prism/Air temperature maximum",paste(names(tmax_station_list_l)[p],".csv",sep="")),row.names = F)
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------