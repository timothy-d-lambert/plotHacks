#' Wrapper functions for fast creation of plot files with defaults I like
#' Just combines the grahic device functions png, tiff, and pdf with par to open a file for plotting
#'@param file The file to write to, this is the only necessary argument
#'@details Any argument from the respective grahic function or par can be passed, but the only thing that is strictly necessary is the file path, others are set with defaults I like.
#'@export

tiff.par<-function(file,res=600,width=6.5,height=5,units='in',bg=NA,mar=c(2.5,2.5,0,0),mgp=c(1.5,0.5,0),las=1,bty='l',col='black',compression="lzw",...){
  tiff(file=file,res=res,width=width,height=height,units=units,compression=compression,bg=bg)
  par(col=col,col.axis=col,col.lab=col,col.main=col,fg=col,bg=bg,mar=mar,mgp=mgp,las=las,bty=bty,...)}


#' Wrapper functions for fast creation of plot files with defaults I like
#' Just combines the grahic device functions png, tiff, and pdf with par to open a file for plotting
#'@param file The file to write to, this is the only necessary argument
#'@details Any argument from the respective grahic function or par can be passed, but the only thing that is strictly necessary is the file path, others are set with defaults I like.
#'@export

png.par<-function(file,res=200,width=6.5,height=5,units='in',bg=NA,mar=c(2.5,2.5,0,0),mgp=c(1.5,0.5,0),las=1,bty='l',col='black',...){
  png(file=file,res=res,width=width,height=height,units=units,bg=bg)
  par(col=col,col.axis=col,col.lab=col,bg=bg,mar=mar,mgp=mgp,las=las,bty=bty,...)}

#' Wrapper functions for fast creation of plot files with defaults I like
#' Just combines the grahic device functions png, tiff, and pdf with par to open a file for plotting
#'@param file The file to write to, this is the only necessary argument
#'@details Any argument from the respective grahic function or par can be passed, but the only thing that is strictly necessary is the file path, others are set with defaults I like.
#'@export

pdf.par<-function(file,width=6.5,height=5,mar=c(2.5,2.5,0,0),mgp=c(1.5,0.5,0),las=1,bty='l',col='black',lwd=1,...){
  pdf(file=file,width=width,height=height)
  par(col=col,col.axis=col,col.lab=col,fg=col,mar=mar,mgp=mgp,las=las,bty=bty,lwd=lwd,...)}

#' Wrapper functions for fast creation of plot files with defaults I like
#' Just combines the grahic device functions png, tiff, and pdf with par to open a file for plotting
#'@param file The file to write to, this is the only necessary argument
#'@details Any argument from the respective grahic function or par can be passed, but the only thing that is strictly necessary is the file path, others are set with defaults I like.
#'@export

svg.par<-function(file,width=6.5,height=5,bg=NA,mar=c(2.5,2.5,0,0),mgp=c(1.5,0.5,0),
              las=1,bty='l',col='black',...){
  svg(file=file,width=width,height=height,bg=bg)
  par(col=col,col.axis=col,col.lab=col,col.main=col,fg=col,bg=bg,mar=mar,mgp=mgp,las=las,bty=bty,...)}
