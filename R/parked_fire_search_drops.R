#' Search for aerial drop points
#'
#' @param fire_bbox A polygon, usually fire bounding box, to search for images
#' @param start_time The first date for which to search for images YYYY-mm-dd
#' @param end_time The last date for which to search for images YYYY-mm-dd
#' @param return_geom TRUE return sf object, FALSE return data frame
#'
#' @return sf object
#' @export
#'
#' @examples
#' #dat.progs.sf <- fire_search_progs(fire_bbox=dat.bbox,start_time=datestart,end_time=dateend)
fire_search_drops <- function(fire_bbox=fire_bbox_polygon(),start_time,end_time){



  checkmate::assert(stringr::str_detect(class(start_time)[1],"POSIXct"),"Error: times must be posixct")
  checkmate::assert(stringr::str_detect(class(end_time)[1],"POSIXct"),"Error: times must be posixct")



  #ensure times are local
  my_tz <- fire_get_timezone(fire_bbox)
  start_time <- lubridate::with_tz(start_time,tz=my_tz)
  end_time <- lubridate::with_tz(end_time,tz=my_tz)


  #connect to database
  DB <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "cermb_fires",
                       user = "mstorey",
                       password = dbpassword,
                       host = "charus.ad.uow.edu.au",
                       port = 5432)

  #simplify fire polygon shape for intersect
  #mainly useful when fire polygon is complex

  fire_bbox <- fire_bbox %>%
    sf::st_transform(4283) %>%
    sf::st_concave_hull(ratio = 0.8) %>%
    sf::st_union() %>%
    sf::st_as_sf()


  #extract geometry of fire as text
  txt_geom <- sf::st_as_text(sf::st_geometry(fire_bbox),EWKT = T)

  #create date part of query
  txt_date <- paste0("dt_local between '",format(start_time,format="%Y-%m-%d %H:%M:%S"), "' AND '", format(end_time,format="%Y-%m-%d %H:%M:%S"),"'")

  #create whole SQL query
  #user input option to return results without geometry to save time
  #run query and get results

    myquery <- paste0("SELECT * FROM fires.drops WHERE st_intersects(fires.drops.geom,'",txt_geom,"') AND ", txt_date)
    x <- sf::st_read(dsn=DB,query=myquery) %>%
      sf::st_as_sf()







  #disconnect from database
  DBI::dbDisconnect(DB)

  if(nrow(x)==0){
    print("no results")
  }else{
    print(paste0(nrow(x)," results"))
    return(x)

  }



}
