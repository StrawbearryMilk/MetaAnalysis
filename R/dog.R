wd = "/home/bearry/DataAnalysis/Meta-Analysis/"
setwd(wd)
#Library import, data cleaning
library("compute.es")
library("metafor")
library("MAd")
library("meta")
library("ggplot2")
library("plotly")
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

a = getN_Age(data,"N","Mean Age",6)
print(a)