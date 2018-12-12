library(compute.es)
library(ggplot2)
library(MAd)
library(meta)
library(metafor)
library(plotly)

aggregateSubdata <- function(subdata, meta.data = getMeta1(subdata)){
	study <- paste(subdata$Author, ", ", subdata$Year, sep="")
	agg <- update(meta.data, byvar = study, bylab = "Study")
	agg <- metabind(agg, pooled = "random")
	return(agg)
	}

getES <- function(subdata){
  res <- mes(m.1 = subdata$Mean_Post_T, m.2 = subdata$Mean_Post_C, sd.1 = subdata$SD_Post_T, sd.2 = subdata$SD_Post_C,
             n.1 = subdata$nT, n.2 = subdata$nC, id = as.numeric(subdata$ID), data = subdata)
  eff.size <- cbind(subdata, res[,c(13,14,15,16)])
  #print(res)
  eff.size
}

getMeta <- function(subdata, task.variables){
  prepData <- cbind(subdata[c(1,2,3)],task.variables,subdata[4:ncol(subdata)]) #preparing data for metacont function
  
  metaF <- metacont(n.e = nT, mean.e = Mean_Post_T, sd.e = SD_Post_T,
                    n.c = nC, mean.c = Mean_Post_C, sd.c = SD_Post_C,
                    comb.fixed = FALSE, studlab = task.variables, data = prepData, sm = "SMD",method.smd = "Hedges")
  return(metaF)
}
#		subdata (atsu) [subdata comes from rows, so get rows first and call on it here

getMeta1 <- function(subdata, task.variables){
	prepData <- cbind(subdata[c(1,2,3)],task.variables,subdata[9:21]) #preparing data for metacont function

	meta1 <- metacont(n.e = nT, mean.e = Mean_Post_T, sd.e = SD_Post_T,
						n.c = nC, mean.c = Mean_Post_C, sd.c = SD_Post_C,
						studlab = task.variables, data = prepData, sm = "SMD",method.smd = "Hedges")
	return(meta1)
	}
#		subdata (atsu) [subdata comes from rows, so get rows first and call on it here

getRESandES <- function(subdata){
	res <- mes(m.1 = subdata$Mean_Post_T, m.2 = subdata$Mean_Post_C, sd.1 = subdata$SD_Post_T, sd.2 = subdata$SD_Post_C,
	n.1 = subdata$nT, n.2 = subdata$nC, id = as.numeric(subdata$ID), data = subdata)
	eff.size <- cbind(subdata[c(1,2,3,5,6,7,8,9,10,11,12,18)], res[,c(13,14,15,16)])
	#print(res)
	eff.size
}

getSubset <- function(type.v, data = data){
	rows <- which(data$Type == type.v)
	subdata <- data[rows,]
	return(subdata)
}

getTaskVariables <- function(subdata){

	task.variable <- paste(subdata$Task, subdata$Variable, sep = " - ")

	return(task.variable) #Object that will be called on later
}

metaregTCB <- function(meta.data){

	if (length(meta.data$TE) == 1)
	{
		print("Cannot perform regression: Only one study")
	}

	else
	{
	return(metareg(x = meta.data, ~1))
	}
}

#need to adjust this if we decide to take moderators into account

plotfunn <- function(meta.data){  #modified version of post by user: jsakaluk
										#https://stats.stackexchange.com/questions/5195/how-to-draw-funnel-plot-using-ggplot2-in-r
	beta.est = meta.data$TE.random#0.0892, the beta coeff
	beta.se = meta.data$seTE.random#0.0035, beta std

	#Store a vector of values that spans the range from 0
	#to the max value of impression (standard error) in your dataset.
	#Make the increment (the final value) small enough (I choose 0.001)
	#to ensure your whole range of data is captured
	se.seq=seq(0, max(meta.data$seTE), 0.001)

	#Compute vectors of the lower-limit and upper limit values for
	#the 95% CI region
	ll95 = beta.est-(1.96*se.seq)
	ul95 = beta.est+(1.96*se.seq)


	#And finally, calculate the confidence interval for your meta-analytic estimate
	meanll95 = beta.est-(1.96*beta.se)
	meanul95 = beta.est+(1.96*beta.se)

	#Put all calculated values into one data frame
	#You might get a warning about '...row names were found from a short variable...'
	#You can ignore it.
	df.CI = data.frame(ll95, ul95, se.seq, beta.est, meanll95, meanul95)

	dt = data.frame(cbind(meta.data$TE,meta.data$seTE))

	#Draw Plot
	fp = ggplot(aes(x = meta.data$TE.random, y = meta.data$seTE.random), data = dt) + #originally aes(x = se, y = Zr), data = dat
	  geom_point(aes(x = meta.data$TE, y = meta.data$seTE), data = dt, shape = 1) +
	  xlab('SMD') + ylab('Standard Error')+
	  geom_line(aes(x = ll95, y = se.seq), linetype = 'dashed', data = df.CI) +
	  geom_line(aes(x = ul95, y = se.seq), linetype = 'dashed', data = df.CI) +
	  geom_segment(aes(x = meanll95, y = min(se.seq), xend = meanll95, yend = max(se.seq)), linetype='dotted', data=df.CI) +
	  geom_segment(aes(x = meanul95, y = min(se.seq), xend = meanul95, yend = max(se.seq)), linetype='dotted', data=df.CI) +
	  scale_y_reverse()+
	  scale_x_continuous(breaks=seq(-3,3,1))+
	  #coord_flip()+ #turning this one off
	  theme_bw()
	ggplotly(fp)
}

####################

imgGen <- function(dataset,name,folder){ #loop through which the bins
  #folder denotes where this is saved to
  #also have to fix forest stuff........... maybe no nyeek tomorrow...?
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
