library(ncdf4)
library(lubridate)
path_input <- "F:/02_Didattica/07_Climate_change_impact_and_adaptation/01_Anno_accademico_2022_2023/Exercitation_5/adaptor.esgf_wps.retrieve-1667645283.2475002-27291-20-cc61564d-58a3-48d7-9ed3-3dd6131381e7/"
path_output <- "F:/02_Didattica/07_Climate_change_impact_and_adaptation/01_Anno_accademico_2022_2023/Exercitation_5/"

nc <- nc_open(paste(path_input,"tas_Amon_EC-Earth3_ssp119_r4i1p1f1_gr_20500116-20991216_v20200425.nc",sep=""))

#time
time <- ncvar_get(nc,"time_bnds")

#I convert time in years and months
nmonth <- length(time)

#Julian days for a non-leap years. 
day <- c(0,31,59,90,120,151,181,212,243,273,304,334)
#Julian days for a leap years.
day_leap <- c(0,31,60,91,121,152,182,213,244,274,305,335)

#leap_year() is a function included in the lubridate package that is necessary to find if a year is leap
#It get a vector with FALSE if a year is not leap and TRUE if a year is leap
#I consider the first and the last possible year
year_leap <- leap_year(1850:2099)
start_year <- 1850
end_year <- 2099
years <- c(1850:2099)
n_year <- end_year-start_year+1

for(i in 1:n_year){
  #if we are considering the first year
  if(i==1){
    day_sequence <- matrix(ncol=3,nrow=12)
    day_sequence[,1] <- years[i]
    day_sequence[,2] <- c(1:12)
    day_sequence[,3] <- day
    if(year_leap[i]==FALSE) count_day <- 365
    if(year_leap[i]==TRUE) count_day <- 366
  }
  #if we are not considering the first year
  if(i!=1){
    #if the considered year is not leap
    if(year_leap[i]==FALSE){
      day_sequence_x <- matrix(ncol=3,nrow=12)
      day_sequence_x[,1] <- years[i]
      day_sequence_x[,2] <- c(1:12)
      day_sequence_x[,3] <- day+count_day
      day_sequence <- rbind(day_sequence,day_sequence_x)
      count_day <- count_day+365
    }
    #if the considered is leap
    if(year_leap[i]==TRUE){
      day_sequence_x <- matrix(ncol=3,nrow=12)
      day_sequence_x[,1] <- years[i]
      day_sequence_x[,2] <- c(1:12)
      day_sequence_x[,3] <- day_leap+count_day
      day_sequence <- rbind(day_sequence,day_sequence_x)
      count_day <- count_day+366
    }
  } 
}

#I check which time element are present in my data. day sequence includes all the possible times from the beginning to the end my it is not mandatory that all these times are present in my data
time_ok <- vector()
for(i in 1:length(time[1,])){
  time_ok[i] <- which(time[1,i]==day_sequence[,3])
}
day_sequence <- day_sequence[time_ok,]


#latitude: for each cell I have the minimum latitude and the maximum latitude. I calculate the latitude of the central point of the cell
latitude <- ncvar_get(nc,"lat_bnds")
lat_point <- latitude[1,,1]+(latitude[2,,1]-latitude[1,,1])/2

#longitude: for each cell I have the minimum longitude and the maximum longitude. I calculate the longituede of the central point of the cell
longitude <- ncvar_get(nc,"lon_bnds")
lon_point <- longitude[1,,1]+(longitude[2,,1]-longitude[1,,1])/2

#The matrix with the data includes the monthly values for all the available grid-points
#I cut a part and I calculate the monthly mean for the selected area
area_lon_min <- 5
area_lon_max <- 19
area_lat_min <- 35
area_lat_max <- 47.5

#I search the grid-points included in the selected area
lat_selected <- which(lat_point>=area_lat_min & lat_point<=area_lat_max)
lon_selected <- which(lon_point>=area_lon_min & lon_point<=area_lon_max)

#Number of point included in the selected area
npoint_selected <- length(lat_selected)*length(lon_selected)
#Coordinates of the selected frod-points
grid_selected <- matrix(ncol=2,nrow=npoint_selected)
colnames(grid_selected) <- c("Latitude","Longitude")
ii <- 0
for(i in 1:length(lat_selected)){
  for(j in 1:length(lon_selected)){
    ii <- ii+1
    grid_selected[ii,"Latitude"] <- lat_point[lat_selected[i]]
    grid_selected[ii,"Longitude"] <- lon_point[lon_selected[j]]
  }
}
write.table(grid_selected,paste(path_output,"Grid_points_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

data <- ncvar_get(nc,"tas")
#For each month I extract the temperature data of the selected area and I calculate the area mean
temp_selected <- vector()
for(i in 1:length(day_sequence[,1])){
  data_selected <- data[lon_selected,lat_selected,i]
  temp_selected[i] <- mean(data_selected,na.rm=TRUE)-273.15
}

temp_selected <- cbind(day_sequence,temp_selected)
colnames(temp_selected) <- c("Year","Month","Julian_day","Mean")

write.table(temp_selected,paste(path_output,"Temperature_mean_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),,sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

start_year <- temp_selected[1,1]
end_year <- temp_selected[length(temp_selected[,1]),1]
n_year <- end_year-start_year+1
temp_matrix <- matrix(data=temp_selected[,"Mean"],ncol=12,nrow=(n_year),byrow=TRUE)
temp_matrix <- cbind(c(start_year:end_year),temp_matrix)
colnames(temp_matrix) <- c("Year","January","February","March","April","May","June","July","August","September","October","November","December")

write.table(temp_matrix,paste(path_output,"Monthly_temperature_mean_matrix_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)
  