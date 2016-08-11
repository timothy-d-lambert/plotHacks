#' Plots error bars on a base R plot
#'@param x a numeric vector palcement of error bars on the x-axis
#'@param y a numeric vector of the y-origins of the error bars
#'@param upper.y A vector of the upper y limits for error bars
#'@param lower.y A vector of the lower y limits for error bars, set to upper.y by default
#'@param upper.x A vector of the upper x limits for error bars
#'@param lower.x A vector of the lower x limits for error bars, set to upper.x by default
#'@param length the length of the cap on the error bars
#'@param x.bar Logical indicating whether to include horizontal error bars
#'@param y.bar Logical indicating whether to include vertical error bars
#'@param interval.type If this is set to "dist", upper.y is set to y+upper.y
#'@details Puts error bars on a plot. The default has only y error bars, you must define the vectors x,y, and upper.y (your error vector) at a minimum. Lower and upper error can be set separately by also defining lower.y. To display x error bars set x.bar=T and upper.x to a vector containing your x error. type is set to either "dist" as in distance from the mean or "value" which is the actual lower/upper bound
#'@export

error.bar <- function(x, y, upper.y,
                      lower.y=upper.y,upper.x,lower.x=upper.x,
                      length=0.03,x.bar=F,y.bar=T,interval.type="dist",...){
if(length(x) != length(y) | length(y) !=length(lower.y) |
     length(lower.y) != length(upper.y))
{stop("vectors must be same length")}



if(y.bar==T){
  if(interval.type=="dist"){
  upper.y<-y+upper.y
  lower.y<-y-lower.y
}

  arrows(x,upper.y,x,lower.y, angle=90,
         code=3, length=length, ...)}
if(x.bar==T){
  if(interval.type=="dist"){
  upper.x<-x+upper.x
  lower.x<-x-lower.x
}
  arrows(upper.x,y,lower.x,y,angle=90,code=3,length=length,...)
}
}


#' Plots error as a polygon on a base R plot, maybe, I haven't really tested or used this much
#'@param x a numeric vector palcement of error bars on the x-axis
#'@param y a numeric vector of the y-origins of the error bars
#'@param upper.y A vector of the upper y limits for error bars
#'@param lower.y A vector of the lower y limits for error bars, set to upper.y by default
#'@param col Color of the polygon
#'@param border Color for the border of the polygon
#'@param interval.type If this is set to "dist", upper.y is set to y+upper.y
#'@details Puts error bars on a plot. The default has only y error bars, you must define the vectors x,y, and upper.y (your error vector) at a minimum. Lower and upper error can be set separately by also defining lower.y. To display x error bars set x.bar=T and upper.x to a vector containing your x error. type is set to either "dist" as in distance from the mean or "value" which is the actual lower/upper bound
#'@export

error.band<-function(x, y, upper.y,
  lower.y=upper.y,col='gray',border=NA,interval.type="dist",...){
  if(length(x) != length(y) | length(y) !=length(lower.y) |
     length(lower.y) != length(upper.y))
{stop("vectors must be same length")}
if(interval.type=="dist"){
  upper.y<-y+upper.y
  lower.y<-y-lower.y
}
  polygon(c(x,rev(x)),
      c(upper.y,rev(lower.y)),col=col,border=border,...)
}
