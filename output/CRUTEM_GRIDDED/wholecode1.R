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

#I check which time element are present in my data. day sequence includes all the possible times from the beginning to the end my it is not mandatory that all these times are present in my data
time_ok <- vector()
for(i in 1:length(time)){
  time_ok[i] <- which(time[i]==day_sequence[,3])
}
day_sequence <- day_sequence[time_ok,]
#I delete the last year because it is not complete
day_sequence <- day_sequence[-which(day_sequence[,1]==2022),]

#The matrix with the data includes the monthly values for all the available grid-points
#I cut a part and I calculate the monthly mean for the selected area
area_lon_min <- 5
area_lon_max <- 19
area_lat_min <- 35
area_lat_max <- 47

#I search the grid-points included in the selected area
lat_selected <- which(latitude>=area_lat_min & latitude<=area_lat_max)
lon_selected <- which(longitude>=area_lon_min & longitude<=area_lon_max)

#Number of point included in the selected area
npoint_selected <- length(lat_selected)*length(lon_selected)
#Coordinates of the selected frod-points
grid_selected <- matrix(ncol=2,nrow=npoint_selected)
colnames(grid_selected) <- c("Latitude","Longitude")
ii <- 0
for(i in 1:length(lat_selected)){
  for(j in 1:length(lon_selected)){
    ii <- ii+1
    grid_selected[ii,"Latitude"] <- latitude[lat_selected[i]]
    grid_selected[ii,"Longitude"] <- longitude[lon_selected[j]]
  }
}
write.table(grid_selected,paste(path3,"Grid_points_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

#For each month I extract the temperature anomaly data of the selected area and I calculate the area mean
temp_selected <- vector()
for(i in 1:length(day_sequence[,1])){
  time_ok <- which(time==day_sequence[i,3])
  data_selected <- data[lon_selected,lat_selected,time_ok]
  temp_selected[i] <- mean(data_selected,na.rm=TRUE)
}

temp_selected <- cbind(day_sequence,temp_selected)
colnames(temp_selected) <- c("Year","Month","Julian_day","Mean")

write.table(temp_selected,paste(path3,"Temperature_anomaly_mean_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),,sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

