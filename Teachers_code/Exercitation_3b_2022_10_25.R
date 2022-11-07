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
write.table(grid_selected,paste(path,"Grid_points_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

#For each month I extract the temperature anomaly data of the selected area and I calculate the area mean
temp_selected <- vector()
for(i in 1:length(day_sequence[,1])){
  time_ok <- which(time==day_sequence[i,3])
  data_selected <- data[lon_selected,lat_selected,time_ok]
  temp_selected[i] <- mean(data_selected,na.rm=TRUE)
}

temp_selected <- cbind(day_sequence,temp_selected)
colnames(temp_selected) <- c("Year","Month","Julian_day","Mean")

write.table(temp_selected,paste(path,"Temperature_anomaly_mean_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

temp_matrix <- matrix(data=temp_selected[,"Mean"],ncol=12,nrow=(n_year-1),byrow=TRUE)
temp_matrix <- cbind((start_year:(end_year-1)),temp_matrix)
colnames(temp_matrix) <- c("Year","January","February","March","April","May","June","July","August","September","October","November","December")

write.table(temp_matrix,paste(path,"Monthly_temperature_anomaly_mean_matrix_",area_lon_min,"_",area_lon_max,"_",area_lat_min,"_",area_lat_max,".csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)