#'Puts text in the top left corner of a plot
#'@param text the text to print
#'@param position currently only topLeft
#'@param xadj position along the x-axis as a proportion of the axis span
#'@param yadj position along the y-axis as a proportion of the axis span
#'
#'@export
#'
panelLabel<-function(text,xadj=0.035,yadj=0.05,...){

  y<-par("usr")[4]-(par("usr")[4]-par("usr")[3])*yadj
  x<-(par("usr")[2]-par("usr")[1])*xadj+par("usr")[1]
  text(x,y,text,...)
}
