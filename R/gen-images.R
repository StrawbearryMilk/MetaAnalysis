library("compute.es")
library("metafor")
library("MAd")
library("meta")
library("ggplot2")
library("plotly")
library("data.table")

wd = "/home/bearry/DataAnalysis/Meta-Analysis/"
setwd(wd)
getwd()
source("R/utils.R")

data <- readRDS("/Output/ModData1.Rda")

bin1 <- data[(data$`Mean Age` <= 30),]
bin2 <- data[(30 < data$`Mean Age` & data$`Mean Age` <= 55),]
bin3 <- data[(data$`Mean Age` > 55),]

for(i in 1:length(data$Type)){
  #Skip the following 4 names as they do not need changing
  if (data$Type[i]=="AT-SUSTAINED" | data$Type[i]=="AT-SELECTIVE" | data$Type[i] == "WM" | data$Type[i] == "LM") 
    next
  #AT cleaning on sustained, selective. Assignment of misc entries to a single type 
  else if (substring(data$Type[i],1,2) == "AT"){
    
    if (data$Type[i] == "AT - SUSTAINED" | data$Type[i] == "AT- SUSTAINED")
    {
      data$Type[i] = "AT-SUSTAINED"
    }
    else if (data$Type[i] == "AT - SELECTIVE")
    {
      data$Type[i] = "AT-SELECTIVE"
    }
    else 
    {
      data$Type[i] = "AT-MISC"
    }  
    
  } 
  #nb5 for EF condition statements
  else {
    data$Type[i] = "EF"
  }
}

data.table(data$Type)[,.N,by=data$Type]
name <- data.table(data$Type)[,.N,by=data$Type]$data

loc1 = "Output/ageUnder30/"
loc2 = "Output/ageIn30to55/"
loc3 = "Output/ageAbove55/"

imgGen(bin1,name,loc1)
imgGen(bin2,name,loc2)
imgGen(bin3,name,loc3)
