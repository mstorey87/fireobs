#' Write R plots of geotiffs to file
#'
#' @param tif_path Path to geotiff
#' @param polygon Polygon to map, usually fire hsitory boundary
#' @param fire_bbox Fire bounding box polygon from fire_bbox_polygon()
#' @param dest_folder Path to write jpg plots
#'
#' @return write jpgs to file
#' @noRd
#'
#' @examples
#' #purrr::map(list.tifs,~fire_write_plot(.x,dat.fire.polygon,dat.bbox,outdir))
fire_write_plot <- function(tif_path,polygon,fire_bbox,dest_folder){

  #read geotiff
  image <- terra::rast(tif_path)

  #rename bands
    if(length(names(image))<3){
      image <- c(image,image,image)
      names(image) <- c("b1","b2","b3")
    }
    image <- terra::stretch(image)

    #ensure same crs
    polygon <- sf::st_transform(polygon,sf::st_crs(image))



    #create plot using tmap
    tmap_image <- tmap::tm_shape(image,bbox = fire_bbox)+
      tmap::tm_rgb(tmap::tm_mv(names(image)))+
      tmap::tm_shape(polygon)+
      tmap::tm_borders(col="blue")+
      tmap::tm_layout(asp = 0,crs=4326)

    #save to file
    out_image <- paste0(dest_folder,"\\quickview_",tools::file_path_sans_ext(basename(tif_path)),".jpg")
    tmap::tmap_save(tmap_image,out_image,outer.margins = c(0,0,0,0))




}
