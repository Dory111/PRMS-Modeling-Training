# Created by Chris Dory
# March 2025

# ------------------------------------------------------------------------------------------------
# Setting location of prms data path and gsp data path
username <- Sys.info()[["user"]]
dropbox_dir <- paste0("C:/Users/",username,"/LWA Dropbox/")
## path_prms_data <- 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Input/PRISM/'
# path_shared_data <- 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Input/PRISM/'
# path_holding_data <- 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Input/PRISM_Temporary_Holding/'
path_holding_data <- paste0(dropbox_dir,"00_Project-Repositories/00598-PRMS-Modeling/shared_data/PRISM_Temporary_Holding/")
path_shared_data <- paste0(dropbox_dir,"00_Project-Repositories/00598-PRMS-Modeling/shared_data/PRISM/")
path_prms_data <- paste0(dropbox_dir,"00_Project-Repositories/00598-PRMS-Modeling/scott/data/")
path_prism_ppt <- paste(path_shared_data,"Precipitation",sep="")
path_prism_tmin <- paste(path_shared_data,"Air temperature minimum",sep="")
path_prism_tmax <- paste(path_shared_data,"Air temperature maximum",sep="")


path_holding_prism_ppt <- paste(path_holding_data,"Precipitation",sep="")
path_holding_prism_tmin <- paste(path_holding_data,"Air temperature minimum",sep="")
path_holding_prism_tmax <- paste(path_holding_data,"Air temperature maximum",sep="")
# ------------------------------------------------------------------------------------------------


# # ------------------------------------------------------------------------------------------------
# # Function run order
Download_PRISM_Data(F) # dont download entire record, try to find date of provisional data or last downloaded date
Update_PRISM_Data() # take downloaded data and check for updates in status
Delete_Holding_Data() # delete temporary downloaded data (holding data)
# # ------------------------------------------------------------------------------------------------


#===========================================================================================
# Placeholder
#===========================================================================================
Download_PRISM_Data <- function(download_all)
{
  
  # ------------------------------------------------------------------------------------------------
  # Test packages
  pkgTest("prism")
  pkgTest("lubridate")
  pkgTest("stringr")
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # If all the folders exist then code currently assumes data is downloaded and will get the early and provisional
  # beginning date
  dir_flags = c(dir.exists(paste(path_shared_data,"Precipitation",sep="")),
                dir.exists(paste(path_shared_data,"Air temperature minimum",sep="")),
                dir.exists(paste(path_shared_data,"Air temperature maximum",sep="")))
  if(!(FALSE %in% dir_flags))
  {
    
    Get_PRISM_Early_Provisional()
    
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # If user does not currently have the correct directories then create them
  Create_Dirs_IF_Missing()
  # ------------------------------------------------------------------------------------------------
  
 
   
  # ------------------------------------------------------------------------------------------------
  # If for whatever reason the user wants to redownload the dataset
  if(download_all == T)
  {
    
    
    # ------------------------------------------------------------------------------------------------
    # sets end_date to last completed calendar month
    end_date <- today() - as.numeric(strsplit(as.character(today()),'-')[[1]][3])
    start_date <- '1981-10-01'
    # ------------------------------------------------------------------------------------------------
    
      
    # ------------------------------------------------------------------------------------------------
    # get ppt data
    options(prism.path = path_prism_ppt)
    get_prism_dailys(
      type = "ppt", 
      minDate = start_date, 
      maxDate = end_date, 
      keepZip = FALSE
    )
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # get tmin data
    options(prism.path = path_prism_tmin)
    get_prism_dailys(
      type = "tmin", 
      minDate = start_date, 
      maxDate = end_date,
      keepZip = FALSE
    )
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # out of use
    # options(prism.path=path_prism_tmean)
    # get_prism_dailys(
    #   type = "tmean", 
    #   minDate = start_date, 
    #   maxDate = end_date, 
    #   keepZip = FALSE
    # )
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # get tmax data
    options(prism.path = path_prism_tmax)
    get_prism_dailys(
      type = "tmax", 
      minDate = start_date, 
      maxDate = end_date, 
      keepZip = FALSE
    )
    # ------------------------------------------------------------------------------------------------
    save(end_date,file=paste(path_shared_data,"prism/last_downloaded_date.RData",sep=""))
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # If the user only wants to download recent data check try to use the early provisional data,
  # if it doesnt exist then try to use the last downloaded date
  # if that doesnt exist then the data must not exist on the computer and 
  # execution halts
  flag_date_files <- F
  if(download_all == F)
  {
    
    
    # ------------------------------------------------------------------------------------------------
    # If a file of the beginning of the provisional period exists
    # Then use it to set the start date of the download period
    # This prevents entire dataset from redownloading each time
    if(file.exists(paste(path_shared_data,
                         'EP_Indices.csv',
                         sep = '')))
    {
      
      # ------------------------------------------------------------------------------------------------
      start_date <- read.csv(paste(path_shared_data,
                                   'EP_Indices.csv',
                                   sep = ''))
      date_colnames <- colnames(start_date)
      start_date <- as.vector(unlist(start_date[1,]))
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # Wherever early and provisional cutoff doesnt exist set it to the last_downloaded_date
      if(file.exists(paste(path_shared_data,"last_downloaded_date.RData",sep="")))
      {
        
        load(paste(path_shared_data,"last_downloaded_date.RData",sep="")) # if the file exists then load it
        
        
        d <- day(end_date)
        m <- month(end_date)
        y <- year(end_date)-1 # Update provisional and early PRISM data
        
        start_date[start_date == '--'] <- as.character(ymd(paste(y,m,d,sep="-")))
      
        
        
      # ------------------------------------------------------------------------------------------------
      # if file doesnt exist then set the remaining missing dates to one year earlier than the present one
      } else {
        
        
        date_to_subtract <- start_date[which(start_date != '--')][1]
        year <- strsplit(date_to_subtract, '-')[[1]][1]
        year <- as.numeric(year) - 1
        missing_dates <- paste(year,
                               substr(date_to_subtract,5,nchar(date_to_subtract)),
                               sep = '')
        
        missing_names <- date_colnames[which(start_date == '--')]
        non_missing_names <- date_colnames[which(start_date != '--')]
        
        winDialog(type = 'ok', message = paste('WARNING: Provisional cutoff not found for \n\n',
                                                paste(missing_names, collapse = ' , '),
                                               '\n\n',
                                               'Using a start date of one year prior to start date of \n\n',
                                               'provisional data of \n\n',
                                                paste(non_missing_names, collapse = ' , '),
                                               paste(' (', missing_dates, ')'),
                                               sep = ''))
        
        
        start_date[which(start_date == '--')] <- missing_dates
      }
      # ------------------------------------------------------------------------------------------------
      
      
      
    # ------------------------------------------------------------------------------------------------
    # if the provisional early data cutoff doesnt exist set everything to last downloaded date
    } else if (file.exists(paste(path_shared_data,"last_downloaded_date.RData",sep=""))) {
      
      
      load(paste(path_shared_data,"last_downloaded_date.RData",sep="")) # if the file exists then load it
      start_date <- end_date 
      
      d <- day(start_date)
      m <- month(start_date)
      y <- year(start_date)-1 # Update provisional and early PRISM data
      
      start_date <- rep(ymd(paste(y,m,d,sep="-")), 4)
      
      
    # ------------------------------------------------------------------------------------------------
    # if neither date files exist then throw error, data must not already exist on the computer
    # and user must download it
    # switch the flag to TRUE so the rest of the code doesnt execute
    } else {
      
      winDialog(type = 'ok', message = paste('ERROR: date data not found in \n\n',
                                             paste(path_shared_data," \n\n",sep=""),
                                             'EXECUTION HALT', sep = ''))
      flag_date_files <- T
      
    }
    # ------------------------------------------------------------------------------------------------
    
    
    
    
    
    
    # ------------------------------------------------------------------------------------------------
    # if any date data was sucessfully retrieved
    if(flag_date_files == F)
    {
      
      # ------------------------------------------------------------------------------------------------
      # sets end_date to last completed calendar month
      end_date <- today() - as.numeric(strsplit(as.character(today()),'-')[[1]][3])
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # get ppt data
      options(prism.path = path_holding_prism_ppt)
      get_prism_dailys(
        type = "ppt", 
        minDate = start_date[1], 
        maxDate = end_date, 
        keepZip = FALSE
      )
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # out of use
      # options(prism.path=path_prism_tmean)
      # get_prism_dailys(
      #   type = "tmean", 
      #   minDate = start_date[2], 
      #   maxDate = end_date, 
      #   keepZip = FALSE
      # )
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # get tmin data
      options(prism.path = path_holding_prism_tmin)
      get_prism_dailys(
        type = "tmin", 
        minDate = start_date[3], 
        maxDate = end_date,
        keepZip = FALSE
      )
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # get tmax data
      options(prism.path = path_holding_prism_tmax)
      get_prism_dailys(
        type = "tmax", 
        minDate = start_date[4], 
        maxDate = end_date, 
        keepZip = FALSE
      )
      # ------------------------------------------------------------------------------------------------
      save(end_date,file=paste(path_shared_data,"last_downloaded_date.RData",sep=""))
    } else {}
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------









#===========================================================================================
# Placeholder
#===========================================================================================
Update_PRISM_Data <- function()
{
  
  holding_directories_paths <- list.dirs(path_holding_data, recursive = FALSE)
  holding_directories_names <- list.dirs(path_holding_data, full.names = FALSE, recursive = FALSE)
  
  existing_directories_paths <- list.dirs(path_shared_data, recursive = FALSE)
  existing_directories_names <- list.dirs(path_shared_data, full.names = FALSE, recursive = FALSE)
  
  
  
  # ------------------------------------------------------------------------------------------------
  # create output log if it doesnt exist
  if(!(file.exists(paste(path_shared_data,'PRISM_Update_History.txt',sep = ''))))
  {
    text <- paste(' ##################################\n',
                  'This document details the download history of PRISM data\n',
                  '##################################\n\n')
    cat(text,
        file = paste(path_shared_data,'PRISM_Update_History.txt',sep = ''))
    
    
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Initializes the progress bar
  pb <- winProgressBar(title = "Windows progress bar", # Window title
                       label = "Percentage completed", # Window label
                       min = 0,      # Minimum value of the bar
                       max = length(existing_directories_names), # Maximum value of the bar
                       initial = 0,  # Initial value of the bar
                       width = 300L) # Width of the window
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Update progress bar
  pctg <- paste("Checking for Unequal Statuses")
  setWinProgressBar(pb, 0, label = pctg)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # starting output text
  text <- paste(' ===========\n',
                today(),'\n',
                '===========\n')
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # get the names of files in each folder and check whether they've changed
  for(dir in 1:length(existing_directories_names))
  {
    
    # ------------------------------------------------------------------------------------------------
    # Update progress bar
    pctg <- paste("Checking for Unequal Statuses in", existing_directories_names[dir])
    setWinProgressBar(pb,
                      (dir/length(existing_directories_names)) * length(existing_directories_names),
                      label = pctg)
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # if the data held in temporary storage has an equivalent in existing data
    # aka there is existing data for tmean did i download it or not
    if(length(which(holding_directories_names == existing_directories_names[dir])) > 0)
    {
      
      existing_data_names <- Get_PRISM_Files(existing_directories_paths[dir])[[2]]
      existing_data_paths <- Get_PRISM_Files(existing_directories_paths[dir])[[1]]
      
      index <- which(holding_directories_names == existing_directories_names[dir])
      
      holding_data_names <- Get_PRISM_Files(holding_directories_paths[index])[[2]]
      holding_data_paths <- Get_PRISM_Files(holding_directories_paths[index])[[1]]
      
      
      
      # existing_data_names[14650:15730]
      # length(existing_data_names)
      # ------------------------------------------------------------------------------------------------
      # Sets up dataframes of existing and holding data that will allow merge and comparison
      existing_df <- matrix(ncol = 2,
                            nrow = length(existing_data_names))
      existing_df <- as.data.frame(existing_df)
      colnames(existing_df) <- c('Existing_Status','Date')
      
      existing_df$Existing_Status[grep('stable', existing_data_names)] <- 'stable'
      existing_df$Existing_Status[grep('provisional', existing_data_names)] <- 'provisional'
      existing_df$Existing_Status[grep('early', existing_data_names)] <- 'early'
      
      
      
      
      holding_df <- matrix(ncol = 2,
                           nrow = length(holding_data_names))
      holding_df <- as.data.frame(holding_df)
      colnames(holding_df) <- c('Holding_Status','Date')
      
      holding_df$Holding_Status[grep('stable', holding_data_names)] <- 'stable'
      holding_df$Holding_Status[grep('provisional', holding_data_names)] <- 'provisional'
      holding_df$Holding_Status[grep('early', holding_data_names)] <- 'early'
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # orders dates of existing data
      for(date in 1:nrow(existing_df))
      {
        
        year <- substr(strsplit(existing_data_names[date],
                                '_')[[1]][5],
                       1,
                       4)
        month <- substr(strsplit(existing_data_names[date],
                                 '_')[[1]][5],
                        5,
                        6)
        day <- substr(strsplit(existing_data_names[date],
                               '_')[[1]][5],
                      7,
                      9)
        existing_df$Date[date] <- as.Date(paste(year,
                                                '-',
                                                month,
                                                '-',
                                                day,
                                                sep = ''))
        
        
      }
      existing_df <- existing_df[order(existing_df$Date, decreasing = FALSE), ]
      existing_df$Date <- as.Date(existing_df$Date)
      # ------------------------------------------------------------------------------------------------
      
      
      
      
      
      # ------------------------------------------------------------------------------------------------
      # orders dates of holding df
      for(date in 1:nrow(holding_df))
      {
        
        year <- substr(strsplit(holding_data_names[date],
                                '_')[[1]][5],
                       1,
                       4)
        month <- substr(strsplit(holding_data_names[date],
                                 '_')[[1]][5],
                        5,
                        6)
        day <- substr(strsplit(holding_data_names[date],
                               '_')[[1]][5],
                      7,
                      9)
        holding_df$Date[date] <- as.Date(paste(year,
                                               '-',
                                               month,
                                               '-',
                                               day,
                                               sep = ''))
        
        
      }
      holding_df <- holding_df[order(holding_df$Date, decreasing = FALSE), ]
      holding_df$Date <- as.Date(holding_df$Date)
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # merge dataframes by date
      holding_existing_df <- merge(existing_df,
                                   holding_df,
                                   'Date',
                                   all.x = T)
      # ------------------------------------------------------------------------------------------------
      
      
      
      
      # ------------------------------------------------------------------------------------------------
      # reformat dates now that they are ordered so files can be queried and moved
      holding_existing_df$Date_Unformatted <- rep(NA,
                                                  nrow(holding_existing_df))
      for(date in 1:nrow(holding_existing_df))
      {
        
        holding_existing_df$Date_Unformatted[date] <- paste(year(holding_existing_df$Date[date]),
                                                            str_pad(month(holding_existing_df$Date[date]),
                                                                    2,
                                                                    pad = '0'),
                                                            str_pad(day(holding_existing_df$Date[date]),
                                                                    2,
                                                                    pad = '0'),
                                                            sep = '')
        
        
      }
      # ------------------------------------------------------------------------------------------------
      
      
      
      
      
      # ------------------------------------------------------------------------------------------------
      # check if any statuses are unequal
      unequal_statuses <- which(holding_existing_df$Existing_Status != holding_existing_df$Holding_Status)
      unequal_statuses_dates <- holding_existing_df$Date_Unformatted[unequal_statuses]
      max_date <- holding_existing_df$Date[nrow(holding_existing_df)]
      # ------------------------------------------------------------------------------------------------
      
      
      
      
      # ------------------------------------------------------------------------------------------------
      # if any statuses are unequal
      if(length(unequal_statuses) > 0)
      {
        
        # ------------------------------------------------------------------------------------------------
        # get the indices
        existing_unequal_statuses_indices <- c()
        holding_unequal_statuses_indices <- c()
        for(i in 1:length(unequal_statuses_dates))
        {
          
          existing_unequal_statuses_indices <- append(existing_unequal_statuses_indices ,
                                                      grep(unequal_statuses_dates[i],
                                                           existing_data_names))
          holding_unequal_statuses_indices <- append(holding_unequal_statuses_indices ,
                                                     grep(unequal_statuses_dates[i],
                                                          holding_data_names))
          
        }
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # get the paths
        existing_unequal_statuses_paths <- existing_data_paths[existing_unequal_statuses_indices]
        holding_unequal_statuses_paths <- holding_data_paths[holding_unequal_statuses_indices]
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # remove the old data
        for(folder in 1:length(existing_unequal_statuses_paths))
        {
          
          unlink(existing_unequal_statuses_paths[folder],
                 recursive = TRUE) # remove old data
          
          
          copy_path <- strsplit(existing_unequal_statuses_paths[folder],'/')[[1]]
          copy_path <- copy_path[1:(length(copy_path) - 1)]
          copy_path <- paste(paste(copy_path, collapse = '/'),'/', sep = '')
          file.copy(from = holding_unequal_statuses_paths[folder],
                    to = copy_path,
                    recursive = T) # move holding data into its place
          
          
          # unlink(holding_unequal_statuses_paths[folder],
          #        recursive = TRUE) # remove holding data now that it has been copied
          
        }
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      holding_df$Date_Unformatted <- rep(NA,nrow(holding_df))
      for(i in 1:nrow(holding_df))
      {
        str <- paste(strsplit(as.character(holding_df$Date[i]),'-')[[1]][1],
                     strsplit(as.character(holding_df$Date[i]),'-')[[1]][2],
                     strsplit(as.character(holding_df$Date[i]),'-')[[1]][3],
                     sep = '')
        holding_df$Date_Unformatted[i] <- str
        
        
      }
      # ------------------------------------------------------------------------------------------------
      
      
      
      
      # ------------------------------------------------------------------------------------------------
      # if the extent of the downloaded data exceeds and cannot be merged onto existing data
      exceedance_dates <- holding_df$Date_Unformatted[holding_df$Date > max_date]
      if(length(exceedance_dates) > 0)
      {
        # ------------------------------------------------------------------------------------------------
        # for each folder move it
        for(i in 1:length(exceedance_dates))
        {
          
          holding_exceedance_path <- holding_data_paths[grep(exceedance_dates[i], holding_data_paths)]
          copy_path <- strsplit(holding_exceedance_path,'/')[[1]]
          copy_path <- copy_path[1:(length(copy_path) - 1)]
          copy_path[copy_path == 'PRISM_Temporary_Holding'] <- 'PRISM'
          copy_path <- paste(paste(copy_path, collapse = '/'),'/', sep = '')
          file.copy(from = holding_exceedance_path,
                    to = copy_path,
                    recursive = T) # move holding data into its place
          
        }
        # ------------------------------------------------------------------------------------------------
      }else{}
      
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      sort <- sort(holding_existing_df$Date[unequal_statuses],decreasing = F)
      
      text <- paste(text,'Downloaded PRISM data for',
                    holding_directories_names[index], holding_df$Date[1] , '-', tail(holding_df$Date,1),'\n')
      text <- paste(text, 'Updated PRISM data for',
                    holding_directories_names[index], sort[1] , '-', tail(sort,1),'\n\n')
      
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  close(pb)
  
  
  # ------------------------------------------------------------------------------------------------
  # Writeout
  text <- paste(text,'\n\n')
  
  cat(text,
      file = paste(path_shared_data,'PRISM_Update_History.txt',sep = ''),
      append = T)
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------




#===========================================================================================
# Placeholder
#===========================================================================================
Delete_Holding_Data <- function()
{
  
  # ------------------------------------------------------------------------------------------------
  # Initializes the progress bar
  pb <- winProgressBar(title = "Windows progress bar", # Window title
                       label = "Percentage completed", # Window label
                       min = 0,      # Minimum value of the bar
                       max = 100, # Maximum value of the bar
                       initial = 0,  # Initial value of the bar
                       width = 300L) # Width of the window
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  holding_directories_paths <- list.dirs(path_holding_data, recursive = FALSE)
  holding_directories_names <- list.dirs(path_holding_data, full.names = FALSE, recursive = FALSE)
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # for each type of data downloaded (precip, tmin, etc.) delete the holding files
  len <- round(seq(from = 0, to = 100, length.out = length(holding_directories_paths) + 1),0) # progress bar element
  for(dir in 1:length(holding_directories_paths))
  {
    
    # ------------------------------------------------------------------------------------------------
    if(dir == 1)
    {
      # ------------------------------------------------------------------------------------------------
      # Update progress bar
      pctg <- paste("Removing Holding Data in ", holding_directories_names[dir])
      setWinProgressBar(pb,
                        0,
                        label = pctg)
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # Get paths to files to be deleted
    holding_data_names <- Get_PRISM_Files(holding_directories_paths[dir])[[2]]
    holding_data_paths <- Get_PRISM_Files(holding_directories_paths[dir])[[1]]
    # ------------------------------------------------------------------------------------------------
    
    
    
    # ------------------------------------------------------------------------------------------------
    # Delete files
    
    for(folder in 1:length(holding_data_paths))
    {
      
      # ------------------------------------------------------------------------------------------------
      # Update progress bar
      pctg <- paste("Removing Holding Data in", holding_directories_names[dir])
      setWinProgressBar(pb,
                        round(((folder/length(holding_data_paths))*100)/3,0) + len[dir],
                        label = pctg)
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      unlink(holding_data_paths[folder],
             recursive = TRUE) # remove holding data now that it has been copied
      # ------------------------------------------------------------------------------------------------

    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  close(pb)
}
# ------------------------------------------------------------------------------------------------






#===========================================================================================
# Find early and provisonal PRISM data
#===========================================================================================
Get_PRISM_Directories <- function()
{
  
  # ------------------------------------------------------------------------------------------------
  # Initializes the progress bar
  pb <- winProgressBar(title = "Windows progress bar", # Window title
                       label = "Percentage completed", # Window label
                       min = 0,      # Minimum value of the bar
                       max = 100, # Maximum value of the bar
                       initial = 0,  # Initial value of the bar
                       width = 300L) # Width of the window
  # ------------------------------------------------------------------------------------------------
  

  
  # ------------------------------------------------------------------------------------------------
  # Update progress bar
  pctg <- paste("Fetching PRISM Directories")
  setWinProgressBar(pb, 0, label = pctg)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Grabbing where the PRISM directories are stored
  prism_data_directories <- list.dirs(path = paste(path_shared_data, sep = ''), recursive = FALSE)
  # prism_data_directories <- prism_data_directories[-c(1)] # removing self reference
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Close progress bar
  setWinProgressBar(pb, 100, label = pctg)
  close(pb)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  save_dir <- path_shared_data
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # Passing Dirs to next fun
  return(list(prism_data_directories,
              save_dir))
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------






#===========================================================================================
# get list of all the file names
#===========================================================================================
Get_PRISM_Files <- function(prism_data_directory)
{
  
  
  # ------------------------------------------------------------------------------------------------
  # Initializes the progress bar
  pb <- winProgressBar(title = "Windows progress bar", # Window title
                       label = "Percentage completed", # Window label
                       min = 0,      # Minimum value of the bar
                       max = 100, # Maximum value of the bar
                       initial = 0,  # Initial value of the bar
                       width = 300L) # Width of the window
  # ------------------------------------------------------------------------------------------------
  
  
  #prism_data_directory <- directories[4]
  # ------------------------------------------------------------------------------------------------
  # Update progress bar
  root_folder <- strsplit(prism_data_directory,'/')[[1]]
  root_folder <- tail(root_folder,1)
  pctg <- paste("Fetching Folders From",root_folder)
  setWinProgressBar(pb, 0, label = pctg)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Grabbing file names for each directory
  daughter_folder_paths <- list.dirs(prism_data_directory,recursive=FALSE)
  daughter_folders_short_name <- list.dirs(prism_data_directory,full.names=FALSE,recursive=FALSE)
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Close progress bar
  setWinProgressBar(pb, 100, label = pctg)
  close(pb)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Passing files to next fun
  return(list(daughter_folder_paths,daughter_folders_short_name))
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------









#===========================================================================================
# Placeholder
#===========================================================================================
Get_PRISM_Early_Provisional <- function()
  
{

  # ------------------------------------------------------------------------------------------------
  # for each directory get the file paths and read in to temporary file
  # then scan these temporary files for early and provisional data
  PRISM_folder_directories <- Get_PRISM_Directories()[[1]]
  save_dir <- Get_PRISM_Directories()[[2]]
  folders <- c()
  P_E_Matrix <- matrix(nrow = 1,
                       ncol = length(PRISM_folder_directories)) # matrix for early provisional start period
  dir <- 1
  for(dir in 1:length(PRISM_folder_directories))
  {
    # ------------------------------------------------------------------------------------------------
    # Get all the file paths within the directory
    folder_paths_names <- Get_PRISM_Files(PRISM_folder_directories[dir])
    folder_paths <- folder_paths_names[[1]]
    folder_names <- folder_paths_names[[2]]
    # ------------------------------------------------------------------------------------------------
    
    
    
    # ------------------------------------------------------------------------------------------------
    # Initializes the progress bar
    pb <- winProgressBar(title = "Windows progress bar", # Window title
                         label = "Percentage completed", # Window label
                         min = 0,      # Minimum value of the bar
                         max = length(folder_paths), # Maximum value of the bar
                         initial = 0,  # Initial value of the bar
                         width = 400L) # Width of the window
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # Update progress bar
    folder <- strsplit(PRISM_folder_directories[dir],'/')[[1]]
    folder <- tail(folder,1)
    folders <- append(folders,folder)
    pctg <- paste("Scanning",folder,"For Early and Provisional Data")
    setWinProgressBar(pb, 0, label = pctg)
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # for each file scan for early and provisional data
    # provisional and early data formatted with characters instead of commas to be compatible with csv

    provisional_inds <- grep('provisional', folder_names)
    provisional_period_beginning <- strsplit(folder_names[provisional_inds[1]],'_')[[1]]
    provisional_period_beginning <- provisional_period_beginning[which(is.na(as.numeric(provisional_period_beginning))  == FALSE)]
    provisional_period_year <- substr(provisional_period_beginning,1,4)
    provisional_period_month <- substr(provisional_period_beginning,5,6)
    provisional_period_day <- substr(provisional_period_beginning,7,9)
    
    
    # early_inds <- grep('early', folder_names)
    # early_period_beginning <- strsplit(folder_names[early_inds[1]],'_')[[1]]
    # early_period_beginning <- early_period_beginning[which(is.na(as.numeric(early_period_beginning))  == FALSE)]
    # early_period_year <- substr(early_period_beginning,1,4)
    # early_period_month <- substr(early_period_beginning,5,6)
    # early_period_day <- substr(early_period_beginning,7,9)
      
      
    setWinProgressBar(pb, 100, label = pctg)
    # ------------------------------------------------------------------------------------------------

    
    
    # ------------------------------------------------------------------------------------------------
    # close progress bar
    close(pb)
    # ------------------------------------------------------------------------------------------------

    

    # ------------------------------------------------------------------------------------------------
    # Writeout
    P_E_Matrix[dir] <- paste(provisional_period_year,
                             '-',
                             provisional_period_month,
                             '-',
                             provisional_period_day,
                             sep = '')
    # ------------------------------------------------------------------------------------------------

  }
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # Writeout
  P_E_Matrix <- as.data.frame(P_E_Matrix)
  colnames(P_E_Matrix) <- folders
  
  write.csv(P_E_Matrix,
            paste(save_dir,'EP_Indices.csv',sep=''),
            row.names = FALSE,
            append = FALSE)
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------






#===========================================================================================
# If user doesn't have data or dirs downloaded creates those dirs
#===========================================================================================
Create_Dirs_IF_Missing <- function()
{

  
  # ------------------------------------------------------------------------------------------------
  # Create real directories
  dir.create(paste(path_shared_data,sep=""))
  path_prism_ppt=paste(path_shared_data,"Precipitation",sep="")
  dir.create(path_prism_ppt)
  path_prism_tmin=paste(path_shared_data,"Air temperature minimum",sep="")
  dir.create(path_prism_tmin)
  path_prism_tmean=paste(path_shared_data,"Air temperature mean",sep="")
  dir.create(path_prism_tmean)
  path_prism_tmax=paste(path_shared_data,"Air temperature maximum",sep="")
  dir.create(path_prism_tmax)
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  # ------------------------------------------------------------------------------------------------
  # Create directories for holding temporary data
  dir.create(paste(path_holding_data,sep=""))
  path_prism_ppt=paste(path_holding_data,"Precipitation",sep="")
  dir.create(path_prism_ppt)
  path_prism_tmin=paste(path_holding_data,"Air temperature minimum",sep="")
  dir.create(path_prism_tmin)
  # path_prism_tmean=paste(path_holding_data, Air temperature mean",sep="")
  # dir.create(path_prism_tmean)
  path_prism_tmax=paste(path_holding_data,"Air temperature maximum",sep="")
  dir.create(path_prism_tmax)
  # ------------------------------------------------------------------------------------------------

}
# ------------------------------------------------------------------------------------------------





#===========================================================================================
# Load and test packages function
#===========================================================================================
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
# ------------------------------------------------------------------------------------------------