#####   @Author: Jasmeet Singh Sasan #####



######## Here the maion goal is to decompose the time series into its components: trend, seasonality and remainder ##########

sp.dfAll<- sp.dsFinal[,4:23]

## creating individual time series for every city
for(x in 1:20){
  
  assign(paste(city.name[x],"AllRows",sep="."), sp.dfAll[x])
  
}

####  creating time-series objects vector to hold all Actual value series #######
ts.All<- c()

for(i in 1:20){    
  ts.All[i]<- paste(city.name[i],"tsAllRows",sep=".")
  
}


for(x in 1:20){
  
  assign(paste(ts.All[x]), ts(eval(parse(text=paste(city.name[x],"AllRows",sep=".")))[,1], start=1991, freq=12))
  
}

### Function to call time series by  name with its index ###
timeSeriesAllNum <- function(x)(eval(parse(text=ts.All[x])))


#####  Plotting all the decomposition for Actual Values including all the years ########
par(mar=c(1,1,1,1))

for(x in 1:20){   
  
  print(city.name[x])
  
  plot.dir<- paste(sp.ActualAllRowPlot, city.name[x],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  # par(mfrow = c(1,1))  
  
  plot(stl(timeSeriesAllNum(x), s.window = "periodic"), main=paste(" Decomposition plot of ", city.name[x]))
  ## plot(stl(log(timeSeriesAllNum(1)), s.window = "periodic"), main=paste(" Decomposition plot (log) of ", city.name[1]))
  
  dev.off()
  
}


#####  Plotting all the decomposition for 1st order differencing Values including all the years ########
for(x in 1:20){   
  
  print(city.name[x])
  
  plot.dir<- paste(sp.Diff1AllRowsPlot, city.name[x],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  # par(mfrow = c(1,1))  
  
  plot(stl(diff(timeSeriesAllNum(x)), s.window = "periodic"), main=paste("1st Order Differencing Decomposition plot of ", city.name[x]))
  ## plot(stl(log(timeSeriesAllNum(1)), s.window = "periodic"), main=paste(" Decomposition plot (log) of ", city.name[1]))
  
  dev.off()
  
}

#####  Plotting all the decomposition for 2nst order differencing Values including all the years ########
for(x in 1:20){   
  
  print(city.name[x])
  
  plot.dir<- paste(sp.Diff2AllRowsPlot, city.name[x],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  # par(mfrow = c(1,1))  
  
  plot(stl(diff(diff(timeSeriesAllNum(x))), s.window = "periodic"), main=paste("2nd Order Differencing Decomposition plot of ", city.name[x]))
  ## plot(stl(log(timeSeriesAllNum(1)), s.window = "periodic"), main=paste(" Decomposition plot (log) of ", city.name[1]))
  
  dev.off()
  
}
######################################################################################################################
######################################################################################################################





################################################     ARIMA MODEL Testing Starts here     #############################
######################################################################################################################


####  creating time-series objects vector to hold all Actual value series starting with year 2003#######
ts.arma03<- c()

for(i in 1:20){    
  ts.arma03[i]<- paste(city.name[i],"ts03",sep=".")
  
}

###ts.arma03

for(x in 1:20){
  
  assign(paste(ts.arma03[x]), ts(eval(parse(text=paste(city.name[x],"AllRows",sep=".")))[145:222,1], start=2003, freq=12))
  
}

### Function to call time series by  name with its index ###
timeSeriesARIMA03 <- function(x)(eval(parse(text=ts.arma03[x])))



## List of arima orders i.e. value of p,d,q to be modelled/fit -- differencing order 1 and order 2
order.arima.diff1<-list(c(0,1,0),c(0,1,1),c(0,1,2),c(0,1,3),c(1,1,0),c(1,1,1),c(1,1,2),c(1,1,3),c(2,1,0),c(2,1,1),c(2,1,2),c(2,1,3),c(3,1,0),c(3,1,1),c(3,1,2),c(3,1,3))

order.arima.diff2<- list(c(0,2,0),c(0,2,1),c(0,2,2),c(0,2,3),c(1,2,0),c(1,2,1),c(1,2,2),c(1,2,3),c(2,2,0),c(2,2,1),c(2,2,2),c(2,2,3),c(3,2,0),c(3,2,1),c(3,2,2),c(3,2,3))


##### Loop Over to generate model for first 4 cities #########
for(i in 1:4){     
  
  for(j in 1:length(order.arima.diff2)){ 
    
    print(paste("   (p,d,q) parameter for ", city.name[i], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
    print("------------------------------------------------------------------")
    print( arima(x=timeSeriesARIMA03(i),order=eval(parse(text= order.arima.diff2[j]))))
    
    print("#####################################################################")
  }
}

##### Loop Over to generate model forindex 5 city i.e. Denver since it needs only 1st order differencing #########

for(j in 1:length(order.arima.diff1)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[5], " is  ", order.arima.diff1[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(5),order=eval(parse(text= order.arima.diff1[j]))))
  
  print("#####################################################################")
}


##### Loop Over to generate model for index 6 to 16 cities #########
for(i in 6:8){     
  
  for(j in 1:length(order.arima.diff2)){ 
    
    print(paste("   (p,d,q) parameter for ", city.name[i], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
    print("------------------------------------------------------------------")
    print( arima(x=timeSeriesARIMA03(i),order=eval(parse(text= order.arima.diff2[j]))))
    
    print("#####################################################################")
  }
}


##### Loop Over to generate model for index 9 city i.e. Atlanta since it needs only 1st order differencing #########
for(j in 1:length(order.arima.diff1)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[9], " is  ", order.arima.diff1[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(9),order=eval(parse(text= order.arima.diff1[j]))))
  
  print("#####################################################################")
}

for(i in 10:12){     
  
  for(j in 1:length(order.arima.diff2)){ 
    
    print(paste("   (p,d,q) parameter for ", city.name[i], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
    print("------------------------------------------------------------------")
    print( arima(x=timeSeriesARIMA03(i),order=eval(parse(text= order.arima.diff2[j]))))
    
    print("#####################################################################")
  }
}

for(i in 13:16){     
  
  for(j in 1:length(order.arima.diff2)){ 
    
    print(paste("   (p,d,q) parameter for ", city.name[i], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
    print("------------------------------------------------------------------")
    print( arima(x=timeSeriesARIMA03(i),order=eval(parse(text= order.arima.diff2[j]))))
    
    print("#####################################################################")
  }
}

##### Loop Over to generate model for index 17 city i.e. Cleavland since it needs only 1st order differencing #########

for(j in 1:length(order.arima.diff1)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[17], " is  ", order.arima.diff1[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(17),order=eval(parse(text= order.arima.diff1[j]))))
  
  print("#####################################################################")
}

##### Loop Over to generate model for index 18 city #################

for(j in 1:length(order.arima.diff2)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[18], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(18),order=eval(parse(text= order.arima.diff2[j]))))
  
  print("#####################################################################")
}


##### Loop Over to generate model for index 19 city i.e. Seattle since it needs only 1st order differencing #########

for(j in 1:length(order.arima.diff2)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[19], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(19),order=eval(parse(text= order.arima.diff2[j]))))
  
  print("#####################################################################")
}


##### Loop Over to generate model for index 20- city

for(j in 1:length(order.arima.diff2)){ 
  
  print(paste("   (p,d,q) parameter for ", city.name[20], " is  ", order.arima.diff2[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  print( arima(x=timeSeriesARIMA03(20),order=eval(parse(text= order.arima.diff2[j]))))
  
  print("#####################################################################")
}

#####################################   Model Testing Ends Here    #################################################################
##########################################################################################################################################




################################################     ARIMA MODEL Fitting Starts here     #############################
######################################################################################################################

###### final orders of all models (cities)  #############
order.arima.fit<- list(c(3,2,2),c(3,2,2),c(3,2,2),c(1,2,0),c(3,1,3),c(3,2,1),c(1,2,0),c(1,2,0),c(3,1,0),c(2,2,3),c(1,2,3),c(1,2,3),c(1,2,2),c(1,2,3),c(1,2,2),c(1,2,3),c(1,1,3),c(3,2,3),c(0,2,3),c(3,2,2))

length(order.arima.fit)

### individual models have been generated with names as e.g. model.fit.AZ_Phoenix
for(j in 1:20){ 
  print("")
  print(paste("   (p,d,q) parameter for ", city.name[j], " is  ", order.arima.fit[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  model.fit.name<- paste("model.fit",city.name[j], sep=".")
  assign(model.fit.name,  arima(x=timeSeriesARIMA03(j),order=eval(parse(text= order.arima.fit[j]))))
  print(eval(parse(text=model.fit.name)))
  print("#####################################################################")
}





###############################################     ARIMA MODEL Fitting Ends here     #############################
######################################################################################################################



##########################      diagnostic checks of the fitted model    ##############################

## vector to store the models by their names
model.fit.ByName<- c()

for(i in 1:20){  
  model.fit.ByName[i]<- paste("model.fit", city.name[i], sep=".")
  
}


##### Ljung-Box Test ##############
for(i in 1:20){  
  
  print(city.name[i])
  
  plot.dir<- paste(sp.LJungBoxTestPlots, city.name[i],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  par(mfrow = c(1,1))
  
  tsdiag(eval(parse(text=model.fit.ByName[i])))
  
  dev.off()
  
}

## qqNorm Plot on the residual values of the fitted model  ##########
for(i in 1:20){  
  
  print(city.name[i])
  
  plot.dir<- paste(sp.QQNormTestPlots, city.name[i],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  par(mfrow = c(1,1))
  
  
  qqnorm(eval(parse(text=model.fit.ByName[i]))$residuals, asp = 1)
  qqline(eval(parse(text=model.fit.ByName[i]))$residuals, asp=1)
  
  dev.off()
  
}




##########################      End of the diagnostic checks of the fitted model    ########################



###################  Final parameters (p,d,q) for the prediction  ##################

order.arima.fit

#### this can be further improved given more time  #####

############################################################################    





########################################################################################################################################################
###############################      FORECASTING        ################################################################################################    
########################################################################################################################################################


#####################   FORECASTING USING THE MODEL FIT FOR NEXT 18 MONTHS    #######################

for(j in 1:20){ 
  print("")
  print(paste("   Forecasting of ", city.name[j], " for  ", order.arima.fit[j]," and Statistics are shown below:"))
  print("------------------------------------------------------------------")
  pred.SPIndex.Name<- paste("pred.SPIndex",city.name[j], sep=".")
  #assign(pred.SPIndex.Name,   predict(eval(parse(text=model.fit.ByName[j])), n.ahead=18, se.fit = TRUE))
  assign(pred.SPIndex.Name,   forecast(eval(parse(text=model.fit.ByName[j])), h=18))
  print(eval(parse(text=pred.SPIndex.Name)))
  print("#####################################################################")
}


##############################################################################
########################### plotting the prediction   ##########################
##############################################################################

for(i in 1:20){  
  plot.dir<- paste(sp.PredictionsPlots, city.name[i],sep="/")
  pdf(paste(plot.dir,".pdf",sep=""))
  print(city.name[i])
  
  plot(eval(parse(text=paste("pred.SPIndex", city.name[i],sep="."))), 
       main=paste(city.name[i],": 18 Months Forecasting") , xlab = "Year", ylab = "Index", ylim = c(50,275),
       panel.first=grid(nx=4,ny=10, col="red",lty = "dotted"))
  
  dev.off()
}



###########################################################################################
###########################################################################################