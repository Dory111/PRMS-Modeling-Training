#####***README***#####

#####***DEFINE DIRECTORIES***#####
username=Sys.info()[["user"]]
dropbox_dir=paste0("C:/Users/",username,"/LWA Dropbox/")
path=paste0(dropbox_dir,"00_Project-Repositories/00598-Siskiyou-GSP-data/inputs/")
setwd(path)

#####***LOAD PACKAGE***#####
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("prism")
pkgTest("lubridate")

#####***DOWNLOAD PRISM DATA***#####
dir.create(paste(path,"PRISM/",sep=""))
path_prism_ppt=paste(path,"PRISM/Precipitation",sep="")
dir.create(path_prism_ppt)
path_prism_tmin=paste(path,"PRISM/Air temperature minimum",sep="")
dir.create(path_prism_tmin)
path_prism_tmean=paste(path,"PRISM/Air temperature mean",sep="")
dir.create(path_prism_tmean)
path_prism_tmax=paste(path,"PRISM/Air temperature maximum",sep="")
dir.create(path_prism_tmax)

flag=file.exists(paste(path,"PRISM/last_downloaded_date.RData",sep=""))
if(flag==T){
  load(paste(path,"PRISM/last_downloaded_date.RData",sep=""))
  start_date=end_date
  d=day(start_date)
  m=month(start_date)
  y=year(start_date)-1 # Update provisional and early PRISM data
  start_date=ymd(paste(y,m,d,sep="-"))
  end_date=today()-1
} else {
  start_date="1981-10-01"   # Scott Valley PRMS starting earlier (1985) to allow for a spin-up period. Back to 1981 for verifying water year type calc - KMA
  end_date=today()-1}

options(prism.path=path_prism_ppt)
get_prism_dailys(
  type = "ppt", 
  minDate = start_date, 
  maxDate = end_date, 
  keepZip = FALSE
)

options(prism.path=path_prism_tmin)
get_prism_dailys(
  type = "tmin", 
  minDate = start_date, 
  maxDate = end_date,
  keepZip = FALSE
)

# options(prism.path=path_prism_tmean)
# get_prism_dailys(
#   type = "tmean", 
#   minDate = start_date, 
#   maxDate = end_date, 
#   keepZip = FALSE
# )

options(prism.path=path_prism_tmax)
get_prism_dailys(
  type = "tmax", 
  minDate = start_date, 
  maxDate = end_date, 
  keepZip = FALSE
)

save(end_date,file=paste(path,"PRISM/last_downloaded_date.RData",sep=""))