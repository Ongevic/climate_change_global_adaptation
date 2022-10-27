install.packages("ncdf4")
library(ncdf4) #Package that is necessary to install and then to upload in order to read the netCDF files
library(lubridate) #Package that is necessary to install and then to upload in order to find the leap years
#path where there are the input files and where I will put the output files
path1 <- "C:/Users/VICTOR_NYABUTI/Climate/climate_change_global_adaptation/data/"

#nc_open() is the function included in the ncdf4 package necessary to use to read a netCDF file
nc <- nc_open(paste(path1,"HadCRUT.5.0.1.0.anomalies.ensemble_mean.nc",sep=""))
#I print in the console all the information included in the netCDF file that are now saved in the nc variable
#I can check the variables included and their name. The unit of the variables, the size and so on..
nc

#ncvar_get() is the function included in the ncdf4 package necessary to read a variable saved in the netCDF file
data <- ncvar_get(nc,"tas_mean") #Matrix three-dimensional
longitude <- ncvar_get(nc,"longitude") #Vector
latitude <- ncvar_get(nc,"latitude") #Vector

time <- ncvar_get(nc,"time") #Vector
nmonth <- length(time)

#Julian days for a non-leap years. The day in the mid of the month is considered
day <- c(15.5,45,74.5,105,135.5,166,196.5,227.5,258,288.5,319,349.5)
#Julian days for a leap years. The day in the mid of the month is considered
day_leap <- c(15.5,45.5,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350.5)

#leap_year() is a function included in the lubridate package that is necessary to find if a year is leap
#It get a vector with FALSE if a year is not leap and TRUE if a year is leap
#I consider the first and the last possible year
year_leap <- leap_year(1850:2022)
start_year <- 1850
end_year <- 2022
years <- c(1850:2022)
n_year <- end_year-start_year+1

for(i in 1:n_year){
  #if we are considering the first year
  if(i==1){
    day_sequence <- matrix(ncol=3,nrow=12)
    day_sequence[,1] <- years[i]
    day_sequence[,2] <- c(1:12)
    if(year_leap[i]==FALSE) day_sequence[,3] <- day #If the year is not leap
    if(year_leap[i]==TRUE) day_sequence[,3] <- day_leap #If the year is leap
    if(year_leap[i]==FALSE) count_day <- 365
    if(year_leap[i]==TRUE) count_day <- 366
  }
  #if we are not considering the first year
  if(i!=1){
    day_sequence_x <- matrix(ncol=3,nrow=12)
    day_sequence_x[,1] <- years[i]
    day_sequence_x[,2] <- c(1:12)
    if(year_leap[i]==FALSE) day_sequence_x[,3] <- day+count_day
    if(year_leap[i]==TRUE) day_sequence_x[,3] <- day_leap+count_day
    day_sequence <- rbind(day_sequence,day_sequence_x)
    if(year_leap[i]==FALSE) count_day <- count_day+365
    if(year_leap[i]==TRUE) count_day <- count_day+366
  } 
}
