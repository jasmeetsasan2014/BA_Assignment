# Extracting data from the webpage using readHTMLTable of XML package since the data is available
# clearly on the HTML page

install.packages("XML")

require(XML)
sp.url<- "http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Dinov_091609_SnP_HomePriceIndex"
sp.extractedData<- readHTMLTable(sp.url, which =1, header= TRUE, stringAsFactors= FALSE)

# checking the heads and tails of the output dataset.
head(sp.extractedData)
tail(sp.extractedData)

# checking the dimension of the dataset
dim(sp.extractedData)

#checking the class of dataset, should be a dataframe
class(sp.extractedData)

# checking the variable names of the dataset
names(sp.extractedData)

#renaming the last column since it has special character \n
colnames(sp.extractedData)[23]<- "Composite-10"

#saving the dataframe in R format
save(sp.extractedData, file="sp.fullDataset")

# saving the dataset in csv format
write.table(sp.extractedData, file="fullDataset.csv", sep=",", row.names= FALSE)

####### Here I have cut out the last six columns ############
sp.dataNew<- sp.extractedData[1:216, ]
tail(sp.dataNew)

# saving the new dataset in csv format
write.table(sp.dataNew, file="sixLessDataset.csv", sep=",", row.names= FALSE)

#check for missing values
any(is.na(sp.extractedData))
any(is.na(sp.dataNew))

















