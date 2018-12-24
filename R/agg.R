wd = "/home/bearry/DataAnalysis/Meta-Analysis/"
setwd(wd)
library(compute.es)
library(ggplot2)
library(MAd)
library(meta)
library(metafor)
library(plotly)
library("data.table")

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

aggen <- function(dataset,name,folder,agg.set = c("Author","Sample")){
  if (agg.set == "Sample")
    dataset <- dataset[order(dataset$Sample),]
  
  name <- data.table(data$Type)[,.N,by=data$Type]$data
  
  for (i in name){
    if (!(i %in% dataset$Type)){
      next
    }
    cat("Type: ", i, "  \n")
    subdata <- getSubset(i, dataset)
    cat(" \n")
    
    meta.data <- getMeta(subdata, getTaskVariables(subdata))
    meta.data
    cat("\n##", i,"funnel and forest plots","\n")
    
    cat("\n",i,"Aggregated data")
    if (agg.set == "Sample")
      meta.agg <- aggregateSubdata(subdata, meta.data, TRUE)
    else 
      meta.agg <- aggregateSubdata(subdata, meta.data)
    meta.agg
    cat("\n",i,"Aggregated meta funnel and forest plots","\n")
    
    png(paste(folder,i, "-agg-funnel",".png",sep=""))
    fp2<-funnel(meta.agg, xlab = "Hedge's g")
    print(fp2)
    dev.off()
    png(paste(folder,i, "aggforest",".png",sep=""), width = 600, height = 600)
    fo2 <- forest(meta.agg, leftcols=c("studlab"), fontsize = 11, plotwidth = "6cm", weight.study="random", squaresize =0, 
                  col.square = "black", label.right="Favors MBI", label.left = "Favors Control", bylab = "Study",text.random = "Summary", 
                  col.random = "red", smlab = "", overall = TRUE)
    print(fo2)
    dev.off()
    cat("  \n")
  }
}

folder = c("figs/AuthorAgg/","figs/SampleAgg/")
aggen(data,name,folder[1],"Author")
aggen(data,name,folder[2],"Sample")