library(ncdf4) #Package that is necessary to install and then to upload in order to read the netCDF files
library(lubridate) #Package that is necessary to install and then to upload in order to find the leap years
#path where there are the input files and where I will put the output files
path <- "C:/Users/verim/Desktop/Exercitation_4/"

#nc_open() is the function included in the ncdf4 package necessary to use to read a netCDF file
nc <- nc_open(paste(path,"adaptor.mars.internal-1666869299.9918544-12781-6-16a66dc2-b922-45ac-b6b9-3ba02e836e4b.nc",sep=""))
#I print in the console all the information included in the netCDF file that are now saved in the nc variable
#I can check the variables included and their name. The unit of the variables, the size and so on..
nc

#ncvar_get() is the function included in the ncdf4 package necessary to read a variable saved in the netCDF file
data <- ncvar_get(nc,"t2m") #Matrix three-dimensional
longitude <- ncvar_get(nc,"longitude") #Vector
latitude <- ncvar_get(nc,"latitude") #Vector

time <- ncvar_get(nc,"time") #Vector
nhours <- length(time)

#leap_year() is a function included in the lubridate package that is necessary to find if a year is leap
#It get a vector with FALSE if a year is not leap and TRUE if a year is leap
#I consider the first and the last possible year
year_leap <- leap_year(1900:2021)
start_year <- 1900
end_year <- 2021
years <- c(1900:2021)
n_year <- end_year-start_year+1

day_sequence <- matrix(ncol=3,nrow=(n_year*12))
day_sequence[,1] <- rep(c(start_year:end_year),each=12)
day_sequence[,2] <- rep(c(1:12),times=n_year)

for(i in 1:length(day_sequence[,1])){
  month <- day_sequence[i,2]
  year <- day_sequence[i,1]
  if(month==1 | month==3 | month==5 | month==7 | month==8 | month==10 | month==12) ng <- 31
  if(month==4 | month==6 | month==9 | month==11) ng <- 30
  if(month==2 & !leap_year(year)) ng <- 28
  if(month==2 & leap_year(year)) ng <- 29
  
  if(i==1) {
    day_sequence[i,3] <- 0
    nhours <- ng*24
  }
  if(i!=1){
    day_sequence[i,3] <- nhours+day_sequence[i-1,3]
    nhours <- ng*24
  } 
}

#I check which time element are present in my data. day sequence includes all the possible times from the beginning to the end my it is not mandatory that all these times are present in my data
time_ok <- vector()
for(i in 1:length(time)){
  time_ok[i] <- which(time[i]==day_sequence[,3])
}
day_sequence <- day_sequence[time_ok,]

#The matrix with the data includes the monthly values for all the available grid-points
#I cut a part and I calculate the monthly mean for the selected area
area_lon_min <- 6
area_lon_max <- 19
area_lat_min <- 36
area_lat_max <- 47.5

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
write.table(grid_selected,paste(path,"Grid_points_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

#For each month I extract the temperature anomaly data of the selected area and I calculate the area mean
temp_selected <- vector()
for(i in 1:length(day_sequence[,1])){
  data_selected <- data[lon_selected,lat_selected,i]
  temp_selected[i] <- mean(data_selected,na.rm=TRUE)-273.15
}

temp_selected <- cbind(day_sequence,temp_selected)
colnames(temp_selected) <- c("Year","Month","Number_of_hours","Mean")

write.table(temp_selected,paste(path,"Temperature_anomaly_mean_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

number_of_year<- day_sequence[length(day_sequence[,1]),1]-day_sequence[1,1]+1
temp_matrix <- matrix(data=temp_selected[,"Mean"],ncol=12,nrow=number_of_year,byrow=TRUE)
temp_matrix <- cbind(c(day_sequence[1,1]:day_sequence[length(day_sequence[,1]),1]),temp_matrix)
colnames(temp_matrix) <- c("Year","January","February","March","April","May","June","July","August","September","October","November","December")

write.table(temp_matrix,paste(path,"Monthly_temperature_anomaly_mean_matrix_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)