#' Changes the bounding box limits
#'
#' @param bbox
#' @param xlim
#' @param ylim
#'
#' @returns
#' @export
#'
#' @examples
extendBbox <- function(bbox, xlim = c(0, 1), ylim = c(0, 1)) {
  # require(tmap,sf)
  if (length(xlim) != 2 || length(ylim) != 2) {
    stop("xlim and ylim must be vectors of length 2")
  }
  extended_bbox <- bbox
  extended_bbox["xmin"] <- bbox["xmin"] + (bbox["xmax"] - bbox["xmin"]) * xlim[1]
  extended_bbox["xmax"] <- bbox["xmax"] + (bbox["xmax"] - bbox["xmin"]) * (xlim[2]-1)
  extended_bbox["ymin"] <- bbox["ymin"] + (bbox["ymax"] - bbox["ymin"]) * ylim[1]
  extended_bbox["ymax"] <- bbox["ymin"] + (bbox["ymax"] - bbox["ymin"]) * (ylim[2]-1)

  return(extended_bbox)
}
