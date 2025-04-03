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
db_path <- paste0(dropbox_dir,"00_Project-Repositories/00598-PRMS-Modeling/scott")
# db_path_data <- paste0(dropbox_dir,"00_Project-Repositories/00598-Siskiyou-GSP-data/outputs/butte")
db_path_data <- paste0(dropbox_dir,"00_Project-Repositories/00598-PRMS-Modeling/scott/data/")
out_dir <- file.path(db_path, "input_files_extension")
# ------------------------------------------------------------------------------------------------



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
  # ----------- Import Input files for extension of model ------------------
  tbl_recharge <<- read_csv(file.path(db_path,"model_files","extension","model_files","prms_work","output", "recharge_monthly.csv"))
  
  tbl_sroff <<- read_csv(file.path(db_path,"model_files","extension","model_files","prms_work","output", "sroff_monthly.csv"))
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Import 
  tbl_HRUs <<- read_csv(file.path(db_path,"GIS","klamath_403_HRUs.csv")) %>%
    dplyr::rename(HRU_ID = HRU)
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------
Import_Extensions_Files()




#===========================================================================================
# Load recharge data
#===========================================================================================
Load_Recharge <- function()
{
  # ------------------------------------------------------------------------------------------------
  ######  ------ recharge ------- #########
  # ------------------------------------------------------------------------------------------------
  hru_list <- unique(tbl_HRUs$HRU_ID[!is.na(tbl_HRUs$HRU_ID)]) # use corrected HRU_ID (consecutive numbers as used by PRMS)
  
  # Save to csv file
  hru_loc <- match( hru_list , names(tbl_recharge), ) # Column numbers are numbers
  tbl_save <- tbl_recharge[, c( 1, hru_loc )]
  
  write.csv(tbl_save, file = file.path(db_path,"model_files/extension/output_analysis","Butte_recharge_month_inch.csv"), row.names = FALSE)
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
  for (i in 1:length(hru_list)) {
    nm_loc <- match( hru_list[i] , names(tbl_recharge), ) # Column numbers are numbers
    nm <- tbl_recharge[, c( 1, nm_loc )]
    names(nm)[2] <- "Results"
    
    g <- ggplot(nm) + geom_line(aes(x = Date, y = Results)) +
      labs(x = 'Date', y = 'Monthly Recharge (sum, in)')
    
    ggsave(file.path(db_path,"model_files/extension/output_analysis/recharge_figures",
                     glue::glue("{make.names(hru_list[i])}_recharge.png")), g, height = 5, width = 10)
    
    
    
    # ------------------------------------------------------------------------------------------------
    # Update progress bar
    pctg <- paste(round((i/length(hru_list))*100, 0),
                  "% Completed for Loading Recharge Output")
    setWinProgressBar(pb,
                      round((i/length(hru_list))*100, 0),
                      label = pctg)
    # ------------------------------------------------------------------------------------------------ 
  }
  # ------------------------------------------------------------------------------------------------
  close(pb)
}
# ------------------------------------------------------------------------------------------------
Load_Recharge()





#===========================================================================================
# Load runoff data
#===========================================================================================
Load_Runoff <- function()
{
  # ------------------------------------------------------------------------------------------------
  ######  ------ sroff = runoff ------- #########
  # ------------------------------------------------------------------------------------------------
  hru_list <- unique(tbl_HRUs$HRU_ID[!is.na(tbl_HRUs$HRU_ID)]) # use corrected HRU_ID (consecutive numbers as used by PRMS)
  
  # Save to csv file
  hru_loc <- match( hru_list , names(tbl_sroff), ) # Column numbers are numbers
  tbl_save <- tbl_sroff[, c( 1, hru_loc )]
  
  write.csv(tbl_save, file = file.path(db_path,"model_files/extension/output_analysis","Butte_sroff_month_inch.csv"), row.names = FALSE)
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
  for (i in 1:length(hru_list)) {
    nm_loc <- match( hru_list[i] , names(tbl_recharge), ) # Column numbers are numbers
    nm <- tbl_recharge[, c( 1, nm_loc )]
    names(nm)[2] <- "Results"
    
    g <- ggplot(nm) + geom_line(aes(x = Date, y = Results)) +
      labs(x = 'Date', y = 'Monthly Surface Runoff to Stream Network (sum, in)') 
    
    ggsave(file.path(db_path,"model_files/extension/output_analysis/runoff_figures",
                     glue::glue("{make.names(hru_list[i])}_runoff.png")), g, height = 5, width = 10)
    
    
    # ------------------------------------------------------------------------------------------------
    # Update progress bar
    pctg <- paste(round((i/length(hru_list))*100, 0),
                  "% Completed Loading Runoff Output in")
    setWinProgressBar(pb,
                      round((i/length(hru_list))*100, 0),
                      label = pctg)
    # ------------------------------------------------------------------------------------------------ 
  }
  # ------------------------------------------------------------------------------------------------
  close(pb)
}
# ------------------------------------------------------------------------------------------------
Load_Runoff()
# browser()