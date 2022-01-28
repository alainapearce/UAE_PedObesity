#### Basic Stats ####

##extracts standard deviation table for DV (either 1 variable or a vector of variables)--for more information on how tapply works, use the R help or RStudio help menu
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Block)
sd.function = function(data, DV, IV){
	sd=with(data, tapply(DV, IV, sd))
	return(sd)
}

sd.function.na = function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd, na.rm=T))
  return(sd)
}


##extracts standard error table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to er
##  eg. er=se.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
se.function=function(data, DV, IV){
	sd=with(data, tapply(DV, IV, sd))
	length=with(data, tapply(DV, IV, length))
  #length is determining the n of the data
	er=sd/sqrt(length)
	return(er)
}

se.function.na=function(data, DV, IV){
  sd=with(data, tapply(DV, IV, sd, na.rm=T))
  length=with(data, tapply(DV, IV, length))
  #length is determining the n of the data
  er=sd/sqrt(length)
  return(er)
}

##extracts mean table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to means
##  eg. means=means.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
means.function = function(data, DV, IV){
	means=with(data, tapply(DV, IV, mean))
	return(means)
}

means.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, mean, na.rm=T))
  return(means)
}

##extracts median table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to medians
##  eg. medians=med.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
med.function = function(data, DV, IV){
  means=with(data, tapply(DV, IV, median))
  return(means)
}

med.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, median, na.rm = T))
  return(means)
}

##extracts IQR table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to interquartile range
##  eg. IQR=IQR.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
IQR.function = function(data, DV, IV){
  means=with(data, tapply(DV, IV, IQR))
  return(means)
}

IQR.function.na = function(data, DV, IV){
  means=with(data, tapply(DV, IV, IQR, na.rm = T))
  return(means)
}

##extracts range table for DV (either 1 variable or a set of variables)
##--to use with bar_graph.se, set function equal to interquartile range
##  eg. IQR=IQR.function()
##--DV can be a single variable or a data.frame/matrix of multiple variables 
##     eg. DV=data.frame(RTime.long$Load, cRTime.long$Load)
range.function = function(data, DV, IV){
  ranges=with(data, tapply(DV, IV, range))
  return(ranges)
}

range.function.na = function(data, DV, IV){
  ranges=with(data, tapply(DV, IV, range, na.rm=T))
  return(ranges)
}

#produces a matrix of pvalues from two-sided t-test (no correction, then bonferoni correct)

pvalue.matrix_none=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  p_num=((l-1)/2)
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      res=t.test(x,y,alternative='two.sided')$p.value
      
      p_res=res
      p_res=round(p_res, digits=3)
      
      if (p_res>0.150){
        p_res="NA"
      }
      if (p_res<0.001){
        p_res="<0.001"
      }
      
      res_matrix[icount,jcount]=p_res
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

pvalue.matrix_bon=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  p_num=((l-1)/2)
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      res=t.test(x,y,alternative='two.sided')$p.value
      
      p_res=p.adjust(res, method = "bonferroni", n = (p_num))
      p_res=round(p_res, digits=3)
      
      if (p_res>0.150){
        p_res="NA"
      }
      if (p_res<0.001){
        p_res="<0.001"
      }
      
      res_matrix[icount,jcount]=p_res
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_0.10=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      
      if (p_res>0.050){
        c_res="NA"
      }
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_0.05=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      
      if (p_res>0.050){
        c_res="NA"
      }
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}


##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_Method_0.05=function(var_vector, var_names, method){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE, method = method)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE, method = method)$estimate, 2)
      
      if (p_res>0.050){
        c_res="NA"
      }
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      
      if (p_res <= 0.05){
        res_matrix[icount,jcount]=paste0(c_res, '*')
      } else{
        res_matrix[icount,jcount]=c_res
      }
      
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_ps=function(var_vector, var_names){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE)$estimate, 2)
      
      res_matrix[icount,jcount]=p_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

##correlation matrix--only show values with p<0.100. Note: missing values are removed via case-wise deletion. var_vector is a data set or matrix with variables, var_names is a vector of strings for collumn headers/variable names
cor.matrix_Method=function(var_vector, var_names, method){
  l=length(var_vector)
  res_matrix=matrix(numeric(0),l,l)
  dimnames(res_matrix) <- list(var_names, var_names)
  icount=0
  jcount=0
  
  for (i in 1:l) {
    icount=icount+1
    jcount=0
    for (j in 1:l){
      jcount=jcount+1
      x=var_vector[[i]]
      y=var_vector[[j]]
      
      p_res=round(cor.test(x,y, na.rm=TRUE, method=method)$p.value, 3)
      c_res=round(cor.test(x,y, na.rm=TRUE, method=method)$estimate, 2)
      
      res_matrix[icount,jcount]=c_res
      
    }
    
  }
  
  #diag_matrix=as.dist(res_matrix)
  res_matrix[upper.tri(res_matrix,diag=TRUE)] <- "" 
  diag_matrix=res_matrix[,1:l]
  return(diag_matrix)
}

#### gtsummary Table Functions ####
library(gtsummary)

my_ttest <- function(data, variable, by, ...) {
  round(t.test(data[[variable]] ~ as.factor(data[[by]]))$p.value, 4)
}

my_chifisher <- function(data, variable, by, ...) {
  tab <- xtabs(~data[[variable]] + data[[by]])
  if (min(tab) <= 5){
    round(fisher.test(tab)$p.value, 4)
  } else {
    round(chisq.test(tab)$p.value, 4)
  }
}

my_anova <- function(data, variable, by, ...) {
  mod <- anova(lm(data[[variable]] ~ as.factor(data[[by]])))
  return(round((mod$`Pr(>F)`[1]), 4))
}

#### Graphing ####

##make bar graph with standard error bars and is designed to be used in congunction with the means and se functions above.  In this case it will only work if your DV vector has 2 or less variables.  If graphing a 3-way interaction see other function for splitting data sets by factors

##--group=0 if only have 1 DV, if DV is multiple variables, group is the variable name for the grouping one
##if group =! 0, it means you have two DV's/a DV and a covariate. The first variable listed in your DV vector will be represtened by different colors in the legend. This will be the "group" variable and will create side by side bars. The second variable will have levels represented on x-axis. note: xpd=False restrains bars to graphic pane (can truncate lower part of graph)
bar_graph.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
    barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    #palette(c("steelblue4", "lightsteelblue2", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    palette(c("blue", "cadetblue1", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

texture_bar_graph.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
		barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
		barx<-barplot(means, add=TRUE, col="white", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(35, 45, 0), density=10)
		barx<-barplot(means, add=TRUE, col="white", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(70, 135, 90), density=10)
		#barx<-barplot(means, add=TRUE, col="white", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(105, 135, 90), density=10)
		#barx<-barplot(means, add=TRUE, col="white", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(140, 135, 90), density=10)
    axis(2)
		axis(1, at=c(0,7), labels=FALSE)
		#this adds the SE wiskers
		arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
	}
	
	else {
    palette(c("blue", "cyan3", "cornflowerblue", "cadetblue1",  "darkcyan", "aquamarine4", "chocolate1", "springgreen3","cornflowerblue"))
	  len=length(levels(group))
    col.list = 1:len
    col.list_dif = c(7, 7, 7, 8, 8, 8,9,9,9)
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE)
    barx<-barplot(means,  add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45, 45), density=8)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(135, 135), density=8)
   # barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(105, 135, 90), density=10)
   # barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(140, 135, 90), density=10)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group),  bty="n",cex=1.5, pch(8,5,0))
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.5, angle=c(45, 45), density=8)
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.5, angle=c(135, 135), density=8)
  #legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.5, angle=c(105, 135, 90), density=8)
  #legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.5, angle=c(140, 135, 90), density=8)
	} 
}

bar_graph.se_legendloc = function(means, er, xlab, ylab, ymax, ymin, group, location){
  if (group==0) {
    barx<-barplot(means, col="cornflowerblue", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    palette(c("blue", "cadetblue1", "cornflowerblue", "cyan3", "darkcyan", "aquamarine4", "royalblue4","cornflowerblue", "darkturquoise"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10, las=3)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, las=3)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend(location, legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

bar_graph_BW.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
    barx<-barplot(means, col="grey", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    palette(c("grey40", "grey", "grey100"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

bar_graph_BW.se_legendloc = function(means, er, xlab, ylab, ymax, ymin, group, location){
  if (group==0) {
    barx<-barplot(means, col="grey", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    palette(c("grey40", "grey", "grey100"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = 7:9
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45), density=10)
    barx<-barplot(means, add=TRUE, col=c(col.list), beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend(location, legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
    
  } 
}

texture_bar_graph_BW.se = function(means, er, xlab, ylab, ymax, ymin, group){
  if (group==0) {
    barx<-barplot(means, col="grey", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    barx<-barplot(means, add=TRUE, col="black", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(35, 45, 0), density=10)
    barx<-barplot(means, add=TRUE, col="black", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(70, 135, 90), density=10)
    barx<-barplot(means, add=TRUE, col="black", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(105, 135, 90), density=10)
    barx<-barplot(means, add=TRUE, col="black", ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE, lwd=1:2, angle=c(140, 135, 90), density=10)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
  }
  
  else {
    #palette(c("grey40", "grey", "grey100"))
    len=length(levels(group))
    col.list = 1:len
    col.list_dif = c(7, 7, 7, 8, 8, 8,9,9,9)
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col = 0, beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE)
##uncomment below for 2 groups  
    barx<-barplot(means,  add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(45, 0), density=5)
     barx<-barplot(means, add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(135, 90), density=5)
##uncomment below for 3 gropus
#     barx<-barplot(means,  add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(35, 45, 0), density=10)
#     barx<-barplot(means, add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(70, 135, 90), density=10)
#     barx<-barplot(means, add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(105, 135, 90), density=10)
#     barx<-barplot(means, add=TRUE, col="black", beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1, lwd=1:2, angle=c(140, 135, 90), density=10)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #this adds the SE wiskers
    arrows(barx, means+er, barx, means-er, angle=90, code=3, length=0.2)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
##uncommnet below for 2 groups
    legend("topright", legend=levels(group), bty="n", cex=2, pch(10,0))
    legend("topright", legend=levels(group), fill=c("black"), bty="n",cex=2, angle=c(45, 0), density=15)
    legend("topright", legend=levels(group), fill=c("black"), bty="n",cex=2, angle=c(135, 90), density=15)
##uncomment below for 3 groups    
#     legend("bottomright", legend=levels(group), fill=c("grey"),  bty="n",cex=1.5, pch(8,5,0))
#     legend("bottomright", legend=levels(group), fill=c("black"), bty="n",cex=1.5, angle=c(35, 45, 0), density=15)
#     legend("bottomright", legend=levels(group), fill=c("black"), bty="n",cex=1.5, angle=c(70, 135, 90), density=15)
#     legend("bottomright", legend=levels(group), fill=c("black"), bty="n",cex=1.5, angle=c(105, 135, 90), density=15)
#     legend("bottomright", legend=levels(group), fill=c("black"), bty="n",cex=1.5, angle=c(140, 135, 90), density=15)
  } 
}

##make bar graph without any standard error bars and is designed to be used in congunction with the means functions above.  In this case it will only work if your DV vector has 2 or less variables.  If graphing a 3-way interaction see other function for splitting data sets by factors

##--group=0 if only have 1 DV, if DV is multiple variables, group is the variable name for the grouping one
##if group =! 0, it means you have two DV's/a DV and a covariate. The first variable listed in your DV vector will be represtened by different colors in the legend. This will be the "group" variable and will create side by side bars. The second variable will have levels represented on x-axis. note: xpd=False restrains bars to graphic pane (can truncate lower part of graph)

bar_graph_BW.no.se = function(means, xlab, ylab, ymax, ymin, group){
  
  library(plotrix)
  if (group==0) {
    barx<-barplot(means, col="grey", pch=5, ylab=ylab, xlab=xlab, ylim=c(ymin, ymax), xpd=FALSE)
    axis(2)
    axis(1, at=c(0,7), labels=FALSE)
  }
  
  else {
    palette(c("grey40", "grey", "grey100"))
    angle_list=c(0, 0, 80, 170, 90)
    density_list=c(0, 0, 15, 20, 20)
    len=length(levels(group))
    col.list = 1:len
    par(fig=c(0, 0.8,0,1), mar=c(4,4,4,4))
    barx<-barplot(means, col=c(col.list),  beside=T, ylab=ylab, xlab=xlab, ylim=c(ymin ,ymax), xpd = FALSE, cex.axis=1, cex.lab=1)
    #axis(2)
    axis(1, at=c(0,20), labels=FALSE)
    #create space for legend
    par(new=T)
    par(fig=c(0, 0.8, 0, 1), mar=c(4, 4, 4, 0))
    plot(5,5,axes=FALSE, ann=FALSE, xlim=c(0,10),ylim=c(0,10), type="n")
    legend("topright", legend=levels(group), fill=c(col.list), bty="n",cex=1.2)
  } 
}

##This is an easier to use function for the ggplot comand for an interaction plot of one continuous and one categorical variable. Make sure xvar and yvar are both continous and cat.var is categorical.  Additionally, xname, yname, and tital must be in quotations.

int.plot_cont.cat=function(dat, yvar, xvar, cond, yname, xname, title){
  ggplot(dat, aes(x= xvar, y=yvar, color=cond),   environment=environment ()) + geom_point(shape=1) + scale_colour_hue(l=50) + geom_smooth(method=lm, se=FALSE, fullrange=F) + scale_y_continuous(name=yname) + scale_x_continuous(name=xname) + ggtitle(title)
}


##This interaction plot is the same as the previous, but with this one you will get the standard error space shaded around the lines of the graph.
int.plot_cont.cat_se=function(data.set, xvar, yvar, cat.var, xname, yname, title){
  ggplot(data.set, aes(x=xvar, y=yvar, color=cat.var), environment=environment ()) + geom_point(shape=1) + scale_colour_hue(l=50) + geom_smooth(method=lm, se=TRUE, fullrange=F) + scale_y_continuous(name=yname) + scale_x_continuous(name=xname) + ggtitle(title)
}
