#' Creates a stacked histogram (as opposed to side-by-side or overlapping) for categorical data
#'@param data is a data.frame or data.table containing the data
#'@param y is the variable to be plotted as a histogram as a quoted name
#'@param group is the quoted name of the grouping variable
#'@param breaks is a vector of breaks for the histogram
#'@param col is an optional specification of the colors to be used in order of unique(group)
#'@param axisStep is the spacing of x-axis labels
#'@return A stacked histogram color coded by the provided factor
#'
#'@export

multiHist<-function(data,y,group,breaks,col=NULL,axisStep=1,xlimit=NULL,logAxis=F,axisLim=NULL,...){
  groups<-unique(data[[group]])
  countNames<-list()
  for(g in 1:length(groups)){
    assign(paste0("hist",g),
           hist(data[[y]][which(data[[group]]==groups[g])],
                breaks=breaks,plot=F))
    assign(paste0("counts",g),get(paste0("hist",g))$count)
    countNames[[g]]<-as.name(paste0("counts",g))
  }
  if(is.null(col)){
    col<-palette()[1:length(groups)]
  }

  counts<-do.call(cbind,args=countNames,quote=F)
  if(is.null(xlimit)){
    barplot(t(counts),col=col,border=NA,space=0,...)
  } else {
    xlimit<-c(which.min(abs(xlimit[1]-hist1$breaks)),which.min(abs(xlimit[2]-hist1$breaks)))
    barplot(t(counts),col=col,border=NA,space=0,xlim=xlimit,...)}

  if(is.null(axisLim)){
    axisSeq<-seq(min(breaks),max(breaks),axisStep)
  } else {axisSeq<-seq(axisLim[1],axisLim[2],axisStep)}

  axisLabels<-axisSeq
  a<-lm(I(1:length(breaks))~breaks)
  axisAt<-predict(a,data.frame(breaks=axisSeq))-1

  if(logAxis) {axisLabels<-10^axisLabels}
  axis(1,axisAt,axisLabels)
  box(bty='l')
}

