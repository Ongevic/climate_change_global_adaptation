path1 = "C:/Users/VICTOR_NYABUTI/Climate/climate_change_global_adaptation/data/CRUTEM_REANALYSIS/"

values = as.matrix(read.table(paste(path1,"GL.csv", sep=""), sep = ",",dec = "."))


#search the exact position of not available values which can be -99999 in R. this values
#indicate that there was an a normally

missing_values = which(values==-9.999)

# to select cells and data in a table use [] and the indexes like 1,1 row and column


values[1,1]


#or use just the cell indicate the exact cell for example 

values[]

# to print all values in the first column use ,1

values[,1]


#now to make replace the values with abnormalities 
values[missing_values] <-  NA

n_column = length(values[1,])  # number of columns
n_rows   = length(values[,1]) #  number of rows

#to eliminate the last column because it has the averages and we need to calculate them ourselves 
values = values[, -n_column]
values


#temp anomalies are in the even rows so we save them separately

even_row = seq(2,n_rows, 2)

# temp averages are on odd  rows so we save them separately

odd_row = seq(1,n_rows, 2)

#so now we wanna select the anomalies 

temp = values[even_row,]

#so now we wanna select the percentages 
perc = values[odd_row,]

temp  = temp[-(1:7),] #to remove the first 7 rows

temp  = temp[-length(temp[,1]),] #to remove the last row


perc  = perc[-(1:7),]

perc  = perc[-length(perc[,1]),]

colnames(temp) = c("Year", "January" , "February", "March", "April", "May", "June", "July","Aug", "Sep","Oct", "Nov", "December")
colnames(temp)  =   c("Year", "January" , "February", "March", "April", "May", "June", "July","Aug", "Sep","Oct", "Nov", "December")

#To select first column

temp[,1]  #or

temp["Year"] #for more general 

#diff for df because in data frame we use $
path2 = "C:/Users/VICTOR_NYABUTI/Climate/climate_change_global_adaptation/output/CRUTEM_REANALYSIS/tables/"


#what, where the name and path its very important to define the wd and exporting the direcory
write.table(temp,paste(path2,"Temparature_anomally.csv", sep = ""), sep = ",",col.names = TRUE, row.names = FALSE, quote =FALSE )



# we wanna work on the same file. one started 1850 another 1857 
# we remove both the temp  and Percentages 






#The second part

#the last year and then I calculate the number of years
start_year <- temp[1,1]
end_year <- temp[length(temp[,1]),1]
n_year <- end_year-start_year+1

#I calculate for each year the yearly average. I do it in two different ways. I put the results in a matrix
#1? method: I use the cycle for
annual_mean_1 <- matrix(ncol=2,nrow=n_year)

annual_mean_1 #empty ,matrix 

annual_mean_1[,1] <- start_year:end_year # defined what is the column, the row by [,1]

annual_mean_1 #now the matrix has a column 

for(i in 1:n_year){
  annual_mean_1[i,2] <- mean(temp[i,2:13],na.rm=FALSE)
} 
# a For loop consists of three parts: the keyword
#For that starts the loop, the condition being tested, 
#and the End For keyword that terminates the loop

#note the parts of for loop
#the first part for then x here as i in y here 1. Note that y has to be an interger and the values of 
#start and end must also be intergers
#In our for i can be can x can be whaterve letter, 1(interger :(to) n_year(number of year we alaready defined i,e 165
# i.e 1:165 ) values for annual_mean_1 should be (note the square brackets that define indexes) [i,2] (this means
# that values between 1:165 insert a second column i.e 2 and that column should have the means.

#the mean should be from the temp variable which if you look at is a table having 13 columns. we
#do not need the first coolumn . it has zears so we are intrested on column 2:13 which are out months.
# comes from the English word 'for' which is 
#used to state the purpose of an object or action, 
#in this case the purpose and details of the iteration.

#iteration describes going through a set of operations that deal with computer code
#for example here the loopp
# loop A loop is a software program or script that repeats the same instructions
#or processes the same information over and over until receiving the order to stop
annual_mean_1

colnames(annual_mean_1) <- c("Year","Annual_mean") #name of columns. if we are naming them using chracters we use ""
# if we were naming them using intergers we do not need.we use c to put the names the together. the comma
#is to separate between the columns. i,e columnn 1, column2

annual_mean_1

write.table(annual_mean_1,paste(path2,"Yearly_average_temperature_anomaly.csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)

#2? method: I use the function apply. 1 indicates row, 2 indicates column
#The function apply works as lines 43-45.
#The problem is that at line 44 you can use mean(na.rm=TRUE) or mean(na.rm=FALSE) depending on the situation. 
#The function apply use mean(na.rm=FALSE) and you can not change it. To change it you have to use
#more complex commands.  
 

# so now  we need to do some plotinng 

# we are using the package ggplot 
#to install package use install.packages("name of the package ")
#to use the package we need to load it to our working space 
#to load use library(ggplot) Note that you do not need to to pu the brackets 

#to use the data we need to comvert the dat into a data frame. ggplot works witth data frames
annual_mean <- as.data.frame(annual_mean_1)

library(ggplot2)
ggplot()+ #I create a grey paper
  geom_line(data=annual_mean,aes(x=Year,y=Annual_mean),size=1.5)+ #I add a line. 
  #The data are included in the dataframe annual_mean. On the X-axis I put the 
  #Years while on the Y-axis I put the annual mean 
  xlab("Year")+ #I change the title of the X-axis
  ylab("Annual mean temperature [?C]")+ #I change the title of the Y-axis
  theme_bw()+ #I change the background from gray that is the default to white
  theme(axis.title.x = element_text(size=30), # I increase the size of the 
        #X-axis title (You have to set these numbers manually)
        axis.title.y = element_text(size=30, angle=90), # I increase the size 
        #of the Y-axis title and I rotate it of an angle equal to 90? 
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30))
ggsave(file="01_Annual_mean_temperature_anomaly.png",path=path2,dpi=500,
       width=40,height=40,units="cm") #I save the plot in a file




#the third lesson 


#I plot the annual mean

#I calculate a low-pass-filter. In this case I use a moving average

#I calculate the moving average with three different length of the window

#a moving average (rolling average or running average) is a calculation 
#to analyze data points by creating a series of averages of different 
#subsets of the full data set. It is also called a moving mean (MM)[1] 
#or rolling mean and is a type of finite impulse response filter. 
#Variations include: simple, cumulative, or weighted forms
#https://en.wikipedia.org/wiki/Moving_average
#https://www.computerhope.com/jargon/l/loop.htm


moving_average <- matrix(ncol=4,nrow=n_year) #an empty matrix where we will put our means

moving_average

colnames(moving_average) <- c("Year","Window_1","Window_2","Window_3")#inserting column names

moving_average

moving_average[,1] <- start_year:end_year #To indicate the first column 
#I can use both the number of the column and the name of the column

moving_average

#1?window
window_1 <- 5

#if you understood the meaning of a moving average 
#we are not consindering the means of  alll years an more
#just a couple of years.  If we have a dataset.  
#we want to consinder the middle part of the datase
#this means that if our first window is 5 then, we want to considner the mean at 3
#thats why we subtract 1 then find the median of this and then add 1.
#remember how to find the median value
#if the data set is odd its way easier
#thats why as a rule of thumb we consinder odd numbered data sets

start_1 <- ((window_1-1)/2)+1 #First row where I can calculate the moving average

moving_average


end_1 <- n_year-((window_1-1)/2) #Last row where I can calculate the moving average

for(i in start_1:end_1){
  moving_average[i,"Window_1"] <- mean(annual_mean[(i-((window_1-1)/2)):(i+((window_1-1)/2)),"Annual_mean"])
}


#remember what we discussed about for loops. so for i our mean plotting which starts at start 
#one ends at end 1 moving average will have values of i in the x axis and values of window_1 in the y axis 

moving_average


#2?window
window_2 <- 11
start_2 <- ((window_2-1)/2)+1 #First row where I can calculate the moving average
end_2 <- n_year-((window_2-1)/2) #Last row where I can calculate the moving average

for(i in start_2:end_2){
  moving_average[i,"Window_2"] <- mean(annual_mean[(i-((window_2-1)/2)):(i+((window_2-1)/2)),"Annual_mean"])
}

moving_average

#3?window
window_3 <- 21
start_3 <- ((window_3-1)/2)+1 #First row where I can calculate the moving average
end_3 <- n_year-((window_3-1)/2) #Last row where I can calculate the moving average

for(i in start_3:end_3){
  moving_average[i,"Window_3"] <- mean(annual_mean[(i-((window_3-1)/2)):(i+((window_3-1)/2)),"Annual_mean"])
}

moving_average


write.table(moving_average,paste(path2,"Moving_averages.csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)
#->
#I transfor the matrix into a data.frame to work with ggplot 

moving_average <- as.data.frame(moving_average)

#I plot the yearly average temperature anomaly and the three different moving averages
ggplot()+ #I create a grey paper
  geom_line(data=annual_mean,aes(x=Year,y=Annual_mean),size=1.5,col="black")+ #I add a line. The data are included in 
  #the dataframe annual_mean. On the X-axis I put the Years while on the Y-axis I put the annual mean. 
  #I decide the size of the line and also the colour
  geom_line(data=moving_average,aes(x=Year,y=Window_1),size=1,col="blue")+
  geom_line(data=moving_average,aes(x=Year,y=Window_2),size=1,col="green")+
  geom_line(data=moving_average,aes(x=Year,y=Window_3),size=1,col="red")+
  xlab("Year")+ #I change the title of the X-axis
  ylab("Annual mean temperature [?C]")+ #I change the title of the Y-axis
  theme_bw()+ #I change the background from gray that is the default to white
  theme(axis.title.x = element_text(size=30), # I increase the size of the X-axis 
        #title (You have to set hese numbers manually)
        axis.title.y = element_text(size=30, angle=90), # I increase the size of the 
        #Y-axis title and I rotate it of an angle equal to 90? 
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30))
ggsave(file="02_Annual_mean_temperature_anomaly.png",
       path=path2,dpi=500,width=40,height=40,units="cm") #I save the plot in a file

#I do again the same plot but I add a legend
ggplot()+ #I create a grey paper
  geom_line(data=annual_mean,aes(x=Year,y=Annual_mean,colour="Temp"),size=1.5)+ #I add a line. 
  #The data are included in the dataframe annual_mean. On the X-axis I put the Years while on 
  #the Y-axis I put the annual mean. I decide the size of the line and also the colour
  geom_line(data=moving_average,aes(x=Year,y=Window_1,colour="Window 05"),size=1)+
  geom_line(data=moving_average,aes(x=Year,y=Window_2,colour="Window 11"),size=1)+
  geom_line(data=moving_average,aes(x=Year,y=Window_3,colour="Window 21"),size=1)+
  xlab("Year")+ #I change the title of the X-axis
  ylab("Annual mean temperature [?C]")+ #I change the title of the Y-axis
  theme_bw()+ #I change the background from gray that is the default to white
  theme(axis.title.x = element_text(size=30), # I increase the size of the X-axis title 
        #(You have to set hese numbers manually)
        axis.title.y = element_text(size=30, angle=90), # I increase the size of the
        #Y-axis title and I rotate it of an angle equal to 90? 
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text=element_text(size=20))+
  scale_colour_manual(values=c("black","blue","green","red"))+ #I add the legend
  labs(colour="") # I remuve the title of the legend
ggsave(file="03_Annual_mean_temperature_anomaly.png",path=path2,dpi=500,width=40,height=40,units="cm") #I
#save the plot in a file

#->

#I calculate the trend over the whole period with the function lm
y <- annual_mean[,"Annual_mean"]
x <- annual_mean[,"Year"]
trend <- lm(y ~ x)
summary(trend) #To view the results of the regression line
#I extract the slope and the intercept, their errors and the 
#significance of the trends using the function coef() and I 
#transform the extracted values in numbers
slope <- as.numeric(coef(summary(trend))[, "Estimate"][2])
intercept <- as.numeric(coef(summary(trend))[, "Estimate"][1])
slope_error <- as.numeric(coef(summary(trend))[, "Std. Error"][2])
intercept_error <- as.numeric(coef(summary(trend))[, "Std. Error"][1])
significance <- as.numeric(coef(summary(trend))[, "Pr(>|t|)"][1])

#I put the results in a matrix
trend_results <- matrix(ncol=2,nrow=5)
trend_results[1,1] <- "Slope"
trend_results[1,2] <- slope
trend_results[2,1] <- "Intercept"
trend_results[2,2] <- intercept
trend_results[3,1] <- "Slope_error"
trend_results[3,2] <- slope_error
trend_results[4,1] <- "Intercept_error"
trend_results[4,2] <- intercept_error
trend_results[5,1] <- "Significance"
trend_results[5,2] <- significance
write.table(trend_results,paste(path2,"Trend_results.csv",sep=""),sep=";",col.names=FALSE,row.names=FALSE,quote=FALSE)


window <- 51
start <- ((window-1)/2)+1 #First row where I can calculate the moving average
end <- n_year-((window-1)/2) #Last row where I can calculate the moving average
n_window <- end-start+1

trend_window <- matrix(ncol=7,nrow=n_window)
colnames(trend_window) <- c("Start_year","End_year", "Slope","Intercept","Slope_error","Intercept_error","Significance")
ii <- 0
t = for(i in start:end){
     ii <- ii+1
     trend_window[ii,"Start_year"] <- annual_mean[(i-((window-1)/2)),"Year"]
     trend_window[ii,"End_year"] <- annual_mean[(i+((window-1)/2)),"Year"]
     y <- annual_mean[(i-((window-1)/2)):(i+((window-1)/2)),"Annual_mean"]
     x <- annual_mean[(i-((window-1)/2)):(i+((window-1)/2)),"Year"]
     trend <- lm(y ~ x)
     as.numeric((coef(summary(trend))[, "Pr(>|t|)"][1]))
     
if(t >= 0.005){
  print(t)
} 
     else{
       window = window + 1}

       
       start <- ((window-1)/2)+1 #First row where I can calculate the moving average
       end <- n_year-((window-1)/2) #Last row where I can calculate the moving average
       n_window <- end-start+1
     }
}
  window
  
  (coef(summary(trend))[, "Pr(>|t|)"][1])

if 
  
  
  
  
summary(trend)

standard_deviation_anomaly <- sd(annual_mean[,2])

#I calculate the residuals of the anomaly from the low-pass-filter
residuals <- matrix(ncol=2,nrow=n_year)
colnames(residuals) <- c("Year","Residuals")
residuals[,"Year"] <- start_year:end_year
residuals[,"Residuals"] <- annual_mean[,"Annual_mean"]-moving_average[,"Window_2"]
standard_deviation_residuals <- sd(residuals[,"Residuals"],na.rm=TRUE)
standard_deviation_anomaly
  

