#moderation analysis printouts
wd = "/home/bearry/DataAnalysis/Meta-Analysis/"
setwd(wd)
library(compute.es)
library(ggplot2)
library(MAd)
library(meta)
library(metafor)
library(data.table)
source("R/utils.R")

data <- readRDS("Output/ModData2.Rda")

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

ageResults <- function(data, age = TRUE){ #data, vector of column names
  file = ""
  if (age){
    data <- data %>% drop_na(`MeanAge`)
    file = "Output/age-mod.txt"
  }
  else{
    data <- data %>% drop_na(`Num_Session`,`Treat_Duration_Hour`,`Home_Duration_Hour`)
    file = "Output/sess_duration.txt"
  }
  fileConn<-file(file,open="wt")
  attach(data)
  
  for (i in name){
    if (!(i %in% data$Type)){
      next
    }
    print(i)
    subdata <- getSubset(i, data)
    meta.data <- getMeta(subdata, getTaskVariables(subdata), resort = TRUE)

    if (length(meta.data$TE) == 1)
    {
      print("Cannot perform regression: Only one study")
    }
    
    else
    {
      #filename <- paste("Output/AgeVals",i,"_",file,sep="")
      
      if (age)
        regRes = metareg(meta.data,~MeanAge)
      else{                
        regRes = metareg(meta.data,~Num_Session + Home_Duration_Hour + Treat_Duration_Hour)
      }
      #print.rma.uni(regRes)
      txt = paste(i, " - mean age p-val = ",regRes$pval[2],"\n")
      writeLines(txt, fileConn)
    }
  }
  close(fileConn)
  detach(data)
}

#ageResults(data)
ageResults(data,FALSE)
