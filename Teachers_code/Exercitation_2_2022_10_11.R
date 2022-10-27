#I extrapolate from my matrix which is the first year, the last year and then I calculate the number of years
start_year <- temp[1,1]
end_year <- temp[length(temp[,1]),1]
n_year <- end_year-start_year+1

#I calculate for each year the yearly average. I do it in two different ways. I put the results in a matrix
#1° method: I use the cycle for
annual_mean_1 <- matrix(ncol=2,nrow=n_year)
annual_mean_1[,1] <- start_year:end_year 
for(i in 1:n_year){
  annual_mean_1[i,2] <- mean(temp[i,2:13],na.rm=FALSE)
}
colnames(annual_mean_1) <- c("Year","Annual_mean")
write.table(annual_mean_1,paste(path,"Yearly_average_temperature_anomaly.csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

#2° method: I use the function apply. 1 indicates row, 2 indicates column
#The function apply works as lines 43-45.
#The problem is that at line 44 you can use mean(na.rm=TRUE) or mean(na.rm=FALSE) depending on the situation. The function apply use mean(na.rm=FALSE) and you can not change it. To change it you have to use more complex commands.  
annual_mean_2 <- matrix(ncol=2,nrow=n_year)
annual_mean_2[,1] <- start_year:end_year
annual_mean_2[,2] <- apply(temp[,2:13],1,mean)
#->
#I plot the annual mean
#To make graphs it is useful the package ggplot2. It is not a base package of R and so it should be installed and then uploaded
library(ggplot2)
#ggplot2 works with dataframe and not with matrix
#I transform the matrix table in a dataframe
annual_mean <- as.data.frame(annual_mean_1)

ggplot()+ #I create a grey paper
  geom_line(data=annual_mean,aes(x=Year,y=Annual_mean),size=1.5)+ #I add a line. The data are included in the dataframe annual_mean. On the X-axis I put the Years while on the Y-axis I put the annual mean 
  xlab("Year")+ #I change the title of the X-axis
  ylab("Annual mean temperature [°C]")+ #I change the title of the Y-axis
  theme_bw()+ #I change the background from gray that is the default to white
  theme(axis.title.x = element_text(size=30), # I increase the size of the X-axis title (You have to set these numbers manually)
        axis.title.y = element_text(size=30, angle=90), # I increase the size of the Y-axis title and I rotate it of an angle equal to 90° 
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30))
ggsave(file="01_Annual_mean_temperature_anomaly.png",path=path,dpi=500,width=40,height=40,units="cm") #I save the plot in a file