wd = "/home/bearry/DataAnalysis/Meta-Analysis/"
setwd(wd)
#Library import, data cleaning
library("tidyr")
library(data.table)
source("R/utils.R") #contains several meta-related functions to be used
source("R/n-age-separator.R")
filename = "Data/MBI on Cognitive Functions (additional articles) V7.csv"
cols.to.use = c("ID","Author","Year","Title","Type","Task","Variable","Inclusion..1..Yes..2..No.", "Sample","Total.N...Mean.Age.","Total.N...Mean.Age.", "nT", "nC", "Num_Session", "Treat_Duration_Hour","Home_Duration_Hour","Mean_Post_T","SD_Post_T","Mean_Post_C","SD_Post_C","Task_Value")
data <- read.csv(filename, stringsAsFactors = FALSE, sep = ",")[,cols.to.use]
colnames(data)[colnames(data) == 'Total.N...Mean.Age.'] = "N"
colnames(data)[colnames(data) == 'Total.N...Mean.Age..1'] = "Mean Age"
colnames(data)[colnames(data) == 'Inclusion..1..Yes..2..No.'] = "Inclusion"
to.use <- which(data[,"Inclusion"] == 1)
data <- data[to.use,]
data$Type = toupper(data$Type)
for (i in 1:length(data[,"N"])){
  N_age = suppressWarnings(getN_Age(data,"N",i))
  data[i,"N"] = N_age[2]
  data[i,"Mean Age"] = N_age[3]
}
data <- data %>% drop_na(`Mean Age`)
#Data saved as characters, this converts them to numeric values
for (i in 10:21){ ##START AT 10
  data[,i] <- suppressWarnings(as.numeric(data[,i]))
}
#removing rows with missing n's, means, sd's
colCheck <- c("nT","nC","Mean_Post_T","Mean_Post_C","SD_Post_T","SD_Post_C")
for (cc in colCheck){
  n <- length(data[,cc]) #length will decrease after some iterations
  bad.vals <- c() #empty vector for rows we will later drop. this will also reset on each iteration
  pos <- 1 #position for the bad.vals vector
  for (num in 1:n){
    if (is.na(data[num,cc])){
      bad.vals[pos] = num    
      pos = pos+1  
    }      
  }  
  if (length(bad.vals) == 0)
    next()
  data = data[-bad.vals,] #remove the rows captured in the inner loop, the move to next iteration
  rownames(data) <- seq(length=nrow(data))
}


#Alter means based on polarity
meanCols = c(17,19)
data[,meanCols] = data[,meanCols] * data$Task_Value
attach(data)
data <- data[order(Author),]
detach(data)
#only use save/loadRDS for ONE object 
saveRDS(data,file="Output/ModData1.Rda")
#bar <- readRDS("ModData1.Rda")