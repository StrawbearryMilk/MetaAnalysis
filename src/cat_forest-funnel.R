library("compute.es")
library("metafor")
library("MAd")
library("meta")
library("ggplot2")
library("plotly")
library("data.table")
library("MetaFunctionsPkg")
data <- read.csv("~/R/PsYR/MBI on Cognitive Functions (additional articles) V5.csv", header = TRUE, stringsAsFactors = FALSE, 
                 sep = ",")[,c(1:8,13,14,18:22)] #only reading in first 8 columns, sample sizes, and mean/sd values

for (i in ncol(data)){ 
  data[,i] <- as.numeric(data[,i])
}

to.use <- which(data[,8] == 1) #when inclusion == 1
data = data[to.use,]
rownames(data) <- seq(length=nrow(data))

#if sample size, mean, or sd is empty, drop the value
#loop through each column and drop based on this
colCheck <- c(9:14) #columns for sample sizes, means, and sds
#Weird ordering, but I do this to reduce the looping time. If a treatment value is missing, it's corresponding control value will
#also probably be missing. SD is more likely to be missing than mean, so I scan these before means.

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

data$Type = toupper(data$Type)

#Alter means based on polarity
meanCols = c(11,13)
data[,meanCols] = data[,meanCols] * data$Task_Value

#cleaning the type names
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

###############################
#Plot generation
for (i in name){
  cat("Type: ", i, "  \n")
  subdata <- getSubset(i, data)
  cat(" \n")
  
  meta.data <- getMeta(subdata, getTaskVariables(subdata))
  meta.data
  cat("\n##", i,"funnel and forest plots","\n")
  
  
  png(paste(i, "-task-var-funnel",".png",sep=""))
  fp1 <- funnel(meta.data, xlab = "Hedge's g")
  print(fp1)
  dev.off()
  
  png(paste(i, "_task-var-forest",".png",sep=""), width = 995, height = 616)
  forest(meta.data, leftcols=c("studlab"), fontsize = 11, plotwidth = "6cm", weight.study="random", squaresize =0, 
         col.square = "black", label.right="Favors MBI", label.left = "Favors Control", bylab = "Study",text.random = "Summary", 
         col.random = "red", smlab = "", overall = TRUE)
  dev.off()
  
  cat("\n",i,"Aggregated data")
  meta.agg <- aggregateSubdata(subdata, meta.data)
  meta.agg
  cat("\n",i,"Aggregated meta funnel and forest plots","\n")
  
  png(paste(i, "-agg-funnel",".png",sep=""))
  fp2<-funnel(meta.agg, xlab = "Hedge's g")
  print(fp2)
  dev.off()
  png(paste(i, "aggforest",".png",sep=""), width = 600, height = 600)
  fo2 <- forest(meta.agg, leftcols=c("studlab"), fontsize = 11, plotwidth = "6cm", weight.study="random", squaresize =0, 
         col.square = "black", label.right="Favors MBI", label.left = "Favors Control", bylab = "Study",text.random = "Summary", 
         col.random = "red", smlab = "", overall = TRUE)
  print(fo2)
  dev.off()
  cat("  \n")
}