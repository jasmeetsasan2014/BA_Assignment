sp.RootDir<- "C:/Users/jasmeet/Desktop/BA_Assignment/Yahoo"


#####   @Author: Jasmeet Singh Sasan #####


############# Creating Root Directories    ##################
dir.R_Plot<- paste(sp.RootDir, "/R_Plot", sep="")
dir.create(dir.R_Plot)

dir.Auto.Arima <- paste(dir.R_Plot, "/Auto.Arima", sep="")
dir.create(dir.Auto.Arima)

dir.Initial_ACF_PACF <- paste(dir.R_Plot, "/Initial_ACF_PACF", sep="")
dir.create(dir.Initial_ACF_PACF)

dir.SeriesDecomposition<- paste(dir.R_Plot, "/SeriesDecomposition", sep="")
dir.create(dir.SeriesDecomposition)

dir.DiagnosticPlots<- paste(dir.R_Plot, "/DiagnosticPlots", sep="")
dir.create(dir.DiagnosticPlots)

dir.PredictionPlots <- paste(dir.R_Plot, "/PredictionPlots", sep="")
dir.create(dir.PredictionPlots)


## creating child directories
dir.AutoArimaPrediction<- paste(dir.Auto.Arima, "/AutoArimaPrediction", sep="")
dir.create(dir.AutoArimaPrediction)

dir.qqNormTest<- paste(dir.DiagnosticPlots, "/qqNormTest", sep="")
dir.create(dir.qqNormTest)

dir.Box_LjungTestt<- paste(dir.DiagnosticPlots, "/Residual_ACF_Box_LjungTest", sep="")
dir.create(dir.Box_LjungTestt)

dir.ActualValAllRows <- paste(dir.SeriesDecomposition, "/ActualValAllRows", sep="")
dir.create(dir.ActualValAllRows)

dir.Diff1ValAllRows <- paste(dir.SeriesDecomposition, "/Diff1ValAllRows", sep="")
dir.create(dir.Diff1ValAllRows)

dir.Diff2ValAllRows<- paste(dir.SeriesDecomposition, "/Diff2ValAllRows", sep="")
dir.create(dir.Diff2ValAllRows)

#### creating variables for respective directory to store plots  ###

sp.R_Plot<-     paste(sp.RootDir,"/R_Plot", sep="")                                    ###### "C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot"
sp.ACFPACFPlot<-   paste(sp.R_Plot,"/Initial_ACF_PACF", sep="")                         ####  "C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/Initial_ACF_PACF"
sp.AutoArimaPlot<-  paste(sp.R_Plot,"/Auto.Arima", sep="")                             ####"C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/Auto.Arima"
sp.AutoArimaPredPlot<- paste(sp.AutoArimaPlot,"/AutoArimaPrediction", sep="")
sp.SeriesSplitRoot<-   paste(sp.R_Plot,"/SeriesDecomposition", sep="")                  ####"C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/SeriesDecomposition"
sp.ActualAllRowPlot<-  paste(sp.SeriesSplitRoot,"/ActualValAllRows", sep="")          ####"C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/SeriesDecomposition/ActualValAllRows"
sp.Diff1AllRowsPlot<-  paste(sp.SeriesSplitRoot,"/Diff1ValAllRows", sep="")            ####"C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/SeriesDecomposition/Diff1ValAllRows"
sp.Diff2AllRowsPlot<-   paste(sp.SeriesSplitRoot,"/Diff2ValAllRows", sep="")            #####"C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/SeriesDecomposition/Diff2ValAllRows"
sp.DiagnosticRoot<-      paste(sp.R_Plot,"/DiagnosticPlots", sep="")                     ####"C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/DiagnosticPlots"
sp.LJungBoxTestPlots<-    paste(sp.DiagnosticRoot,"/Residual_ACF_Box_LjungTest", sep="")  ####"C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/DiagnosticPlots/Residual_ACF_Box_LjungTest"
sp.QQNormTestPlots<-       paste(sp.DiagnosticRoot,"/qqNormTest", sep="")                  ####"C:/Users/jasmeet/Desktop/BA_Assignment/Adobe/R_Plot/DiagnosticPlots/qqNormTest"
sp.PredictionsPlots<-       paste(sp.R_Plot,"/PredictionPlots", sep="")


install.packages("XML")
require(XML)

sp.url<- "http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Dinov_091609_SnP_HomePriceIndex"
sp.extractedData<- readHTMLTable(sp.url, which =1, header= TRUE, stringAsFactors= FALSE)

head(sp.extractedData)
tail(sp.extractedData)
dim(sp.extractedData)
class(sp.extractedData)
names(sp.extractedData)

#renaming the column, unwanted characters
colnames(sp.extractedData)[23]<- "Composite-10"

#function to get rid off leading and trailing spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
sp.oldName<- names(sp.extractedData)
sp.newName0<- trim(sp.oldName)

#function to replace hyphens from variable names with _
repHyp <- function (x) gsub("\\-", "_", x)
sp.newName<- repHyp(sp.newName0)

#replacing dataset with new variable names
colnames(sp.extractedData) <- sp.newName
names(sp.extractedData)

#Function to convert character value to numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


# creating new data set with numeric values-- Note: all variables are numeric
sp.ds <- data.frame(lapply(sp.extractedData, as.numeric.factor), stringsAsFactors=FALSE)
any(is.na(sp.ds))

#converting numeric to factors (categorical variables)
sp.ds$Month<- as.factor(sp.ds$Month)
sp.ds$Year<- as.factor(sp.ds$Year)

#getting the values from older dataset
sp.ds$Month<- sp.extractedData$Month

#Creating an ordered factor of Months
sp.dsFinal<- sp.ds

factorMon<-c("January","February","March","April","May","June","July","August","September","October","November","December")
factorMon

sp.dsFinal$Month<- factor(sp.ds$Month, levels=factorMon,ordered=TRUE)
head(sp.dsFinal)

class(sp.ds$Month)
class(sp.dsFinal$Month)

all.equal(sp.ds[,1:23],sp.dsFinal[,1:23])


### creating a date variable from exiting month and year  ###
sp.dsFinal$Date<- paste(sp.dsFinal$Year,sp.dsFinal$Month, "01", sep="-")
sp.dsFinal$Date<-as.Date(sp.dsFinal$Date,"%Y-%B-%d") 
head(sp.dsFinal)

#class(sp.dsFinal$Date)

# removing unwanted datasets and variables
rm(sp.ds, sp.newName,sp.newName0,sp.oldName)

#saving the dataframe in R format
save(sp.dsFinal, file="sp.fullDataset")
# saving the dataset in csv format
write.table(sp.dsFinal, file="fullDataset.csv", sep=",", row.names= FALSE)

#check for missing values 
any(is.na(sp.dsFinal))

#checking the order of levels of the Month
levels(sp.dsFinal$Month)

######### Here ends  data cleaning and validation ###########



######################################################################################################################
######################################################################################################################




#sessionInfo()

#Basic Over all Plots
install.packages("ggplot2")
require(reshape2)
require(ggplot2)

install.packages("ggthemes")
library(ggthemes)


ggplot(sp.dsFinal, aes(y=AZ_Phoenix,x=Date)) +geom_line()

### this is individual line plot of all cities together
## take its output image 

sp.DFlinePlot<- melt(sp.dsFinal[,4:24], id="Date")
head(sp.DFlinePlot)

sp.matrixName<- names(sp.dsFinal[,4:24])
class(sp.matrixName)
sp.matrixName

plot.overall<- ggplot(data=sp.DFlinePlot,aes(x=Date,y=value, color=variable))

p1<- plot.overall + geom_line() + labs(x= "Year", y= "INDEX" ,title='S&P/Case-Shiller Home Price Index')+
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

colors.factor<- c("dodgerblue4", "darkolivegreen4","darkorchid3", "goldenrod1", "red","darkgreen","yellow","black","grey58","brown",
                  "azure","skyblue","yellowgreen","orange4","khaki4","magenta2","chocolate","deeppink4","seagreen1","tomato2")

p2<- p1+ guides(colour = guide_legend(override.aes = list(size=4)))+scale_color_manual(values=colors.factor)+theme(panel.grid.minor = element_line(colour="grey", size=0.5))

plot.dir<- paste(sp.R_Plot, "OverAllPlot",sep="/")
pdf(paste(plot.dir,".pdf",sep=""))
p2
dev.off()


######################################################################################################################
######################################################################################################################






#############################################   Start OF mvtsplot   ##################################################


install.packages("mvtsplot")
require(mvtsplot)

## creating a datafrmae containig time-series for all the cities
sp.matrix<- sp.dsFinal[,4:23]
head(sp.matrix)

### this package is used just for the plotting purpose    ######

####  this is telling me when housing bubblt started building. It depicts the a trend of downfall of index 
## Setting norm as global would normalize the data wrt all data series. Note: This is not correlation with other series

## box plot on Right Hand panel confirms abscence of outliers in the data
## as we increase the number of levels we can more clearly identify the index change
## and this is possible since out data is not sparse and pretty smooth

## here individual time series for all states are depicted that means index is categorized
## based on  individual time series. It clearly shows the overall index level of every City

plot.dir<- paste(sp.R_Plot, "VisualAll",sep="/")
pdf(paste(plot.dir,".pdf",sep=""))

par(mfrow = c(2,1))

mvtsplot(sp.matrix, norm="global",  level= 3,
         margin = TRUE, rowstat = "mean", main="Multi- Region time series plot discretized in LOW(purple), MEDIUM(grey) and HIGH(green) category.")

mvtsplot(sp.matrix, norm="global",  level= 5,
         margin = TRUE,rowstat = "mean", main="Multi- Region time series plot discretized in LOW(purple), MEDIUM(grey) and HIGH(green) category." )

dev.off()

#############################################   END OF mvtsplot   ##################################################


