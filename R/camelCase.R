#' Converts text with separators to camel case (i.e., camelCase)
#'
#'@param x Vector of text to be converted
#'@param sep The symbol that separates words in the text
#'@details Eliminates separating character and capitalizes the first letter of each word except the first word. For example converts "This is an example" to thisIsAnExample.
#'@export

camelCase<-function(x,sep="_"){
  camel<-function(toCamel,sep){
    xSplit<-unlist(strsplit(toCamel,sep))
    if(length(xSplit)==1){
      xSplit<-paste0(tolower(substr(xSplit,1,1)),substr(xSplit,2,nchar(xSplit)))
      return(xSplit)}
    xSplit[1]<-tolower(xSplit[1])
    for(n in 2:length(xSplit)){
      xSplit[n]<-paste0(toupper(substr(xSplit[n],1,1)),
                        substr(xSplit[n],2,nchar(xSplit[n])))
    }
    camelled<-paste(xSplit,collapse="")
    return(camelled)
  }
  return(unlist(lapply(x,camel,sep=sep)))
}
