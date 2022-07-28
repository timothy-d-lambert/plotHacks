#' Creates a color-coded proportion plot good for variables that can be sequentially cumulative (e.g., substrate bigger than x, bigger than x+x1,...)
#'@param cat Categorical data to be plotted (categories for y-variable)
#'@param prop Proprortional data (y-variable)
#'@param time Time variable (x-variable)
#'@param catOrder Order of the categorical variables as a vector in the correct order
#'@param col Colors for categories. Should be the same length as length(cat). Defaults to a gray-scale.
#'@return A plot of proportional data over time
#'
#'@export


catPropPlot<-function(cat,prop,time,catOrder,col=gray(seq(0,1,length.out=length(catOrder))),...){

  d<-data.table(cat=factor(cat,levels=catOrder,ordered=T),
                prop=prop,time=time)
  allTime<-crossing(unique(time),factor(catOrder,levels=catOrder,ordered=T)) %>%
    data.table() %>%
    setnames(c("time","cat")) %>%
    setkey(time,cat)

  setkey(d,time,cat)
  d<-d[allTime]

  d[is.na(prop),prop:=0]

  d[,cumProp:=cumsum(prop),time]

  yUpper<-d[,sum(prop),time] %>% .[,max(V1)]
  yLower<-0

  plot(prop~time,data=d,pch=NA,ylim=c(yLower,yUpper),...)

  for(ca in rev(catOrder)){
    points(cumProp~time,data=d[cat==ca],type='l')
    polygon(c(d[cat==ca,time],rev(d[cat==ca,range(time)])),
            c(d[cat==ca,cumProp],0,0),col = col[which(ca==catOrder)])
  }
}
