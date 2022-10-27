#I calculate the trends over all the 50 years windows covering the investigated period 
window <- 51
start <- ((window-1)/2)+1 #First row where I can calculate the moving average
end <- n_year-((window-1)/2) #Last row where I can calculate the moving average
n_window <- end-start+1

trend_window <- matrix(ncol=7,nrow=n_window)
colnames(trend_window) <- c("Start_year","End_year", "Slope","Intercept","Slope_error","Intercept_error","Significance")
ii <- 0
for(i in start:end){
  ii <- ii+1
  trend_window[ii,"Start_year"] <- annual_mean[(i-((window-1)/2)),"Year"]
  trend_window[ii,"End_year"] <- annual_mean[(i+((window-1)/2)),"Year"]
  y <- annual_mean[(i-((window-1)/2)):(i+((window-1)/2)),"Annual_mean"]
  x <- annual_mean[(i-((window-1)/2)):(i+((window-1)/2)),"Year"]
  trend <- lm(y ~ x)
  trend_window[ii,"Slope"] <- as.numeric(coef(summary(trend))[, "Estimate"][2])
  trend_window[ii,"Intercept"] <- as.numeric(coef(summary(trend))[, "Estimate"][1])
  trend_window[ii,"Slope_error"] <- as.numeric(coef(summary(trend))[, "Std. Error"][2])
  trend_window[ii,"Intercept_error"] <- as.numeric(coef(summary(trend))[, "Std. Error"][1])
  trend_window[ii,"Significance"] <- as.numeric(coef(summary(trend))[, "Pr(>|t|)"][1])
}
write.table(trend_window,paste(path,"Trend_window_results.csv",sep=""),sep=";",col.names=TRUE,row.names=FALSE,quote=FALSE)
#->
#I calculate the standard deviation of the yearly temperature anomaly
standard_deviation_anomaly <- sd(annual_mean[,2])

#I calculate the residuals of the anomaly from the low-pass-filter
residuals <- matrix(ncol=2,nrow=n_year)
colnames(residuals) <- c("Year","Residuals")
residuals[,"Year"] <- start_year:end_year
residuals[,"Residuals"] <- annual_mean[,"Annual_mean"]-moving_average[,"Window_2"]
standard_deviation_residuals <- sd(residuals[,"Residuals"],na.rm=TRUE)
