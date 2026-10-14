#' Read a linescan with s3path
#'
#' @param s3name full https path or tif file name
#'
#' @return terra rast object
#' @keywords internal
#'
#' @examples
#' #
fire_read_scan <- function(s3name){

  #if only the file name is input, add in the path
  if(!stringr::str_detect(s3name,"https")){
    s3name <- paste0("https://s3.its.uow.edu.au/uow-rb01186-ros-model/scans.original.crs/",s3name)
  }

  if(!stringr::str_ends(s3name,".tif")){
    s3name <- paste0(s3name,".tif")
  }



  ras <- terra::rast(s3name)

  return(ras)

}
