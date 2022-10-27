#path where you can find the data that you want analyse
#Attention: you must use SLASH (/) and not BACKSLASH (\)
#Attention: you have to change the path and the name of the file
path <- "C:/Users/verim/Desktop/Exercitation_2/"
#Read the data and save them in a matrix
values <- as.matrix(read.table(paste(path,"HadCRUT5_Analysis_GL.csv",sep=""),sep=";",header=FALSE))

#I use the function which() to find the values equal to -9.999, number used in this dataset to indicate missing values. R uses NA as missing value
#which() counts the cells scanning for each column all the rows.
missing_values <- which(values==-9.999)
#I replace -9.999 with NA
values[missing_values] <- NA
#->
#I calculate the number of column anf the number of row of my matrix using the function length()
n_column <- length(values[1,])
n_row <- length(values[,1])
#I delete the last column that is the annual mean.
values <- values[,-n_column]

#I create two vectors, one with the even number from 2 to the number of row of my matrix and one with the odd number from 1 to the number of row
even_row <- seq(2,n_row,by=2)
odd_row <- seq(1,n_row,by=2)
#I create two matrix. In the first I put the temperature values and in the second one I put the percentage of available grid-points
temp <- values[odd_row,]
perc <- values[even_row,]

#I delete the year first seven years and the last year because it is not complete
temp <- temp[-c(1:7),]
temp <- temp[-length(temp[,1]),]
perc <- perc[-c(1:7),]
perc <- perc[-length(perc[,1]),]

#I add a header to my matrices
colnames(temp) <- c("Year","January","February","March","April","May","June","July","August","September","October","November","December")
colnames(perc) <- c("Year","January","February","March","April","May","June","July","August","September","October","November","December")
write.table(temp,paste(path,"Temperature_anomaly.csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)
write.table(perc,paste(path,"Percentage_of_available_grid.csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)