#' Search for and download fire progression polygons in the UOW database
#'
#' @description
#' This function queries the University of Wollongong fire progression database for polygons representing mapped fire progression.
#' The query will return all fire progression polygons that **intersect** a given bounding polygon (`fire_bbox`) and fall within the specified local datetime range.
#' Optionally, you can choose to return either the full spatial geometry (`sf` object) or just a data frame of attributes for faster queries.
#'
#' Note: This function requires access to the CERMb fire progression Postgres database via VPN or internal network.
#'
#' @param fire_bbox A polygon (`sf` object). Usually the fire bounding box to search for overlapping fire progression polygons.
#' @param start_time The first date/time to search for (`POSIXct`, local timezone).
#' @param end_time The last date/time to search for (`POSIXct`, local timezone).
#' @param return_geom Logical. If `TRUE`, return an `sf` object with geometries. If `FALSE`, return a `data.frame` without geometry (faster).
#' @param dbpassword Database password for connecting to the fire progression database.
#' @param RDS Logical. If TRUE, connect to Amazon RDS database. If FALSE, connect to UOW database.
#'
#' @return An `sf` object (default) or a `data.frame` containing fire progression attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: search for fire polygons in Blue Mountains for 1 December 2019
#' library(sf)
#' library(fireobs)
#'
#' # Create a simple bounding box around part of the Blue Mountains
#' blue_mtns_bbox <- sf::st_as_sfc(
#'   sf::st_bbox(c(
#'     xmin = 150.2,
#'     ymin = -33.8,
#'     xmax = 150.5,
#'     ymax = -33.5
#'   )),
#'   crs = 4326
#' ) %>% sf::st_as_sf(crs = 4326)
#'
#' # Set search dates
#' start_time <- as.POSIXct("2019-12-01", tz = "Australia/Sydney")
#' end_time <- as.POSIXct("2019-12-02", tz = "Australia/Sydney")
#'
#' # Run search (needs working VPN and valid dbpassword)
#' # Replace 'mypassword' with your actual password:
#' fire_polys <- fire_search_progs(
#'   fire_bbox = blue_mtns_bbox,
#'   start_time = start_time,
#'   end_time = end_time,
#'   return_geom = TRUE,
#'   dbpassword = "mypassword"
#' )
#'
#' head(fire_polys)
#' }
fire_search_progs <- function(fire_bbox = fire_bbox_polygon(),
                              start_time,
                              end_time,
                              return_geom = TRUE,
                              dbpassword,
                              RDS=FALSE) {

  # Check inputs: start and end time must be POSIXct
  checkmate::assert(inherits(start_time, "POSIXct"), "Error: times must be POSIXct")
  checkmate::assert(inherits(end_time, "POSIXct"), "Error: times must be POSIXct")

  checkmate::assert(inherits(fire_bbox, "sf"))
  checkmate::assert(!is.na(sf::st_crs(fire_bbox)), "fire_bbox must have a valid CRS")

  # Get local timezone for the fire polygon
  my_tz <- fire_get_timezone(fire_bbox)
  start_time <- lubridate::with_tz(start_time, tz = my_tz)
  end_time <- lubridate::with_tz(end_time, tz = my_tz)

  # Connect to the CERMb fire progression Postgres database
  checkmate::assert(nzchar(dbpassword), "Error: dbpassword must not be empty")


  if(isTRUE(RDS)){
    # Database connection parameters
    host <- "database-1.c7qmuuwwinte.ap-southeast-2.rds.amazonaws.com"
    user <- "postgres"
    port <- 5432  # Default PostgreSQL port

    # Establish the connection
    DB <- DBI::dbConnect(RPostgres::Postgres(),
                          dbname = 'postgres',
                          host = host,
                          port = port,
                          user = user,
                          password = dbpassword)

     }

  if(!isTRUE(RDS)){

    DB <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = "cermb_fires",
      user = "mstorey",
      password = dbpassword,
      host = "charus.ad.uow.edu.au",
      port = 5432
    )

  }


  on.exit(DBI::dbDisconnect(DB), add = TRUE)

  # Simplify input polygon shape for faster spatial intersection
  fire_bbox <- fire_bbox %>%
    sf::st_transform(4283) %>%
    sf::st_convex_hull() %>% # Convert to convex hull for broader match
    sf::st_union() %>%
    sf::st_as_sf()

  # Convert polygon to WKT for SQL query
  txt_geom <- sf::st_as_text(sf::st_geometry(fire_bbox), EWKT = TRUE)

  # Create SQL date condition
  txt_date <- paste0(
    "dt_local between '",
    format(start_time, format = "%Y-%m-%d %H:%M:%S"),
    "' AND '",
    format(end_time, format = "%Y-%m-%d %H:%M:%S"),
    "'"
  )

  # Build SQL query to select with or without geometry
  if (return_geom == TRUE) {
    myquery <- paste0(
      "SELECT dt_utc, dt_local, prog_rowid, s3name, firetype, notes, sourceshp, sourcecontact, firename, season, sourcetype, aus_state, scanorgnl, geom FROM fires.progression_polygons WHERE st_intersects(fires.progression_polygons.geom,'",
      txt_geom, "') AND ", txt_date
    )
    x <- sf::st_read(dsn = DB, query = myquery) %>%
      sf::st_as_sf() %>%
      dplyr::arrange(dplyr::desc(dt_local))
  }

  if (return_geom == FALSE) {
    myquery <- paste0(
      "SELECT dt_utc, dt_local, prog_rowid, s3name, firetype, notes, sourceshp, sourcecontact, firename, season, sourcetype, aus_state, scanorgnl
       FROM fires.progression_polygons
       WHERE st_intersects(fires.progression_polygons.geom,'",
      txt_geom, "') AND ", txt_date
    )
    x <- DBI::dbGetQuery(DB, myquery)
  }

  # Disconnect from database
  #DBI::dbDisconnect(conn = DB)

  if (nrow(x) == 0) {
    message("no results")
    return(invisible(NULL))
  } else {
    message(paste0(nrow(x), " results"))
    return(x)
  }
}


#' #' Search for fire progression polygons
#' #'
#' #' @param fire_bbox A polygon, usually fire bounding box, to search for images
#' #' @param start_time The first date for which to search for images YYYY-mm-dd
#' #' @param end_time The last date for which to search for images YYYY-mm-dd
#' #' @param return_geom TRUE return sf object, FALSE return data frame
#' #'
#' #' @return sf object
#' #' @export
#' #'
#' #' @examples
#' #' #dat.progs.sf <- fire_search_progs(fire_bbox=dat.bbox,start_time=datestart,end_time=dateend)
#' fire_search_progs <- function(fire_bbox=fire_bbox_polygon(),start_time,end_time,return_geom=TRUE,dbpassword){
#'
#'
#'
#'   checkmate::assert(stringr::str_detect(class(start_time)[1],"POSIXct"),"Error: times must be posixct")
#'   checkmate::assert(stringr::str_detect(class(end_time)[1],"POSIXct"),"Error: times must be posixct")
#'
#'
#'
#'   #ensure times are local
#'   my_tz <- fire_get_timezone(fire_bbox)
#'   start_time <- lubridate::with_tz(start_time,tz=my_tz)
#'   end_time <- lubridate::with_tz(end_time,tz=my_tz)
#'
#'
#'   #connect to database
#'   # Establish the connection
#'
#'   # DB <- DBI::dbConnect(RPostgres::Postgres(),
#'   #                  dbname = 'postgres',
#'   #                  host = "database-3.cn2u4ig8wad8.ap-southeast-2.rds.amazonaws.com",
#'   #                  port = 5432,
#'   #                  user = "postgres",
#'   #                  password = dbpassword)
#'
#'   # #connect to database - the UOW fire progress database. needs a vpn connect to uow network,
#'   DB <- DBI::dbConnect(RPostgres::Postgres(),
#'                        dbname = "cermb_fires",
#'                        user = "mstorey",
#'                        password = dbpassword,
#'                        host = "charus.ad.uow.edu.au",
#'                        port = 5432)
#'
#'   #simplify fire polygon shape for intersect
#'   #mainly useful when fire polygon is complex
#'
#'   fire_bbox <- fire_bbox %>%
#'     sf::st_transform(4283) %>%
#'     #sf::st_concave_hull(ratio = 0.8) %>%
#'     sf::st_convex_hull() %>%
#'     sf::st_union() %>%
#'     sf::st_as_sf()
#'
#'
#'   #extract geometry of fire as text
#'   txt_geom <- sf::st_as_text(sf::st_geometry(fire_bbox),EWKT = T)
#'
#'   #create date part of query
#'   txt_date <- paste0("dt_local between '",format(start_time,format="%Y-%m-%d %H:%M:%S"), "' AND '", format(end_time,format="%Y-%m-%d %H:%M:%S"),"'")
#'
#'   #create whole SQL query
#'   #user input option to return results without geometry to save time
#'   #run query and get results
#'   if(return_geom==TRUE){
#'     myquery <- paste0("SELECT * FROM fires.progression_polygons WHERE st_intersects(fires.progression_polygons.geom,'",txt_geom,"') AND ", txt_date)
#'     x <- sf::st_read(dsn=DB,query=myquery) %>%
#'       sf::st_as_sf() %>%
#'       dplyr::arrange(dplyr::desc(dt_local))
#'   }
#'
#'   if(return_geom==FALSE){
#'     myquery <- paste0("SELECT dt_utc,dt_local,prog_rowid,s3name,firetype,progid,notes,sourceshp,firename,season,sourcetype,aus_state,scanname FROM fires.progression_polygons WHERE st_intersects(fires.progression_polygons.geom,'",txt_geom,"') AND ", txt_date)
#'     x <- DBI::dbGetQuery(DB,myquery)
#'   }
#'
#'
#'
#'
#'   #disconnect from database
#'   DBI::dbDisconnect(conn = DB)
#'
#'   if(nrow(x)==0){
#'     message("no results")
#'   }else{
#'     message(paste0(nrow(x)," results"))
#'     return(x)
#'
#'   }
#'
#'
#'
#' }
