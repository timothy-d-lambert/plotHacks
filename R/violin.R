#' Creates a violin plot splitting the data into groups
#' This is just a slight variation on the vioplot function from the package 'vioplot' that I made so I could just pass a data variable and a grouping variable instead of splitting the data outside of the function, which is how vioplot works.
#'@param x is a quoted variable name that is the data that goes into violin plots
#'@param splitBy is a variable used to split the data into groups
#'@param data is a data.frame or data.able that contains the data
#'@param other parameters are stolen from vioplot
#'
#'@export

violin<-function (x,splitBy=NULL,data, range = 1.5, h = NULL, ylim = NULL, names = NULL,
          horizontal = FALSE, col = "magenta", border = "black", lty = 1,
          lwd = 1, rectCol = "black", colMed = "white", pchMed = 19,
          at, add = FALSE, wex = 1, drawRect = TRUE, orders=NULL,
          xlab=NULL,ylab=NULL,main=NULL)
{
  if(is.data.table(data)){data<-data.frame(data)}
  datas<-list()
  if(!is.null(splitBy)){

    groups<-unique(data[[splitBy]])

    if(is.null(orders)){
      groups<-groups[order(groups)]
    } else {groups<-groups[which(groups)==orders]}

    for(f in groups){
      datas[[as.character(f)]]<-data[data[[splitBy]]==f,x]
    }
  } else {datas[[x]]<-data[[x]]}
  n <- length(datas)
  if (missing(at))
    at <- 1:n
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  if (!(is.null(h)))
    args <- c(args, h = h)
  for (i in 1:n) {
    data <- datas[[i]]
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i],
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim),
                                     args))
    hscale <- 0.4/max(smout$estimate) * wex
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
  }
  if (!add) {
    xlim <- if (n == 1)
      at + c(-0.5, 0.5)
    else range(at) + min(diff(at))/2 * c(-1, 1)
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }

  label<-x
  if(!is.null(splitBy)){label<-groups}

  boxwidth <- 0.05 * wex
  if (!add)
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, label = label)
      if(is.null(ylab)){ylab<-x}
      if(is.null(xlab)){xlab<-splitBy}
      title(xlab=xlab,ylab=ylab,main=main)
    }
    box()
    for (i in 1:n) {
      polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])),
              c(base[[i]], rev(base[[i]])), col = col, border = border,
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd,
              lty = lty)
        rect(at[i] - boxwidth/2, q1[i], at[i] + boxwidth/2,
             q3[i], col = rectCol)
        points(at[i], med[i], pch = pchMed, col = colMed)
      }
    }
  }
  else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, label = label)
      if(!exists("ylab")){ylab<-x}
      if(!exists("xlab")){xlab<-splitBy}
      title(xlab=xlab,ylab=ylab,main=main)
    }
    box()
    for (i in 1:n) {
      polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]],
                                              rev(at[i] + height[[i]])), col = col, border = border,
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd,
              lty = lty)
        rect(q1[i], at[i] - boxwidth/2, q3[i], at[i] +
               boxwidth/2, col = rectCol)
        points(med[i], at[i], pch = pchMed, col = colMed)
      }
    }
  }
  invisible(list(upper = upper, lower = lower, median = med,
                 q1 = q1, q3 = q3))
}
