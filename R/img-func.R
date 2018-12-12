####COnvert this into a function which calls on source: utils
#input: binned sub-data set, name vector
#output: save images to a corresponding output folder 
#Plot generation
source("R/utils")

imgGen <- function(dataset,name,folder){ #loop through which the bins
  #folder denotes where this is saved to
  #also have to fix forest stuff........... maybe no nyeek tomorrow...?
  for (i in name){
    cat("Type: ", i, "  \n")
    subdata <- getSubset(i, dataset)
    cat(" \n")
    
    meta.data <- getMeta(subdata, getTaskVariables(subdata))
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
    
    cat("\n",i,"Aggregated data")
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