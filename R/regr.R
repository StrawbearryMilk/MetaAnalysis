#TV regression
wd = "/home/bearry/DataAnalysis/Meta-Analysis/"
setwd(wd)
library(compute.es)
library(ggplot2)
library(MAd)
library(meta)
library(metafor)
library(plotly)
library(data.table)

source("R/utils.R")

data <- readRDS("/Output/ModData2.Rda")

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

tvgen <- function(dataset,name,folder){
  dataset <- dataset[order(dataset),]
  for (i in name){
    if (!(i %in% dataset$Type)){
      next
    }
    cat("Type: ", i, "  \n")
    subdata <- getSubset(i, dataset)
    cat(" \n")
    
    meta.data <- getMeta(subdata, getTaskVariables(subdata), resort = TRUE)
    meta.data
    cat("\n##", i,"funnel and forest plots","\n")
    
    
    png(paste(folder,i, "-task-var-funnel",".png",sep=""))
    fp1 <- funnel(meta.data, xlab = "Hedge's g")
    print(fp1)
    dev.off()
    
    png(paste(folder,i, "_task-var-forest",".png",sep=""), width = 995, height = 616)
    forest(meta.data, leftcols=c("studlab"), fontsize = 11, plotwidth = "6cm", weight.study="random", squaresize =0, 
           col.square = "black", label.right="Favors MBI", label.left = "Favors Control", bylab = "Study",text.random = "Summary", 
           col.random = "red", smlab = "", overall = TRUE)
    dev.off()
    }
}

tvgen(data,name,"figs/TaskVar/")
