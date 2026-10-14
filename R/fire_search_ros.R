#' Search for fire ROS lines by weather conditions
#'
#' @description
#' This function queries the University of Wollongong fire progression database for ROS lines derived from mapped fire progression.
#' The database will be searched by weather conditions (ranges), ROS range, location and time.
#' The result will be a list containing the ROS lines, BARRA weather data and progression polygons
#'
#' Note: This function requires access to the CERMb fire progression Postgres database via VPN or internal network.
#'
#' @param fire_bbox A polygon (`sf` object). Usually the fire bounding box to search for overlapping fire progression polygons.
#' @param start_time The first date/time to search for (`POSIXct`, local timezone).
#' @param end_time The last date/time to search for (`POSIXct`, local timezone).
#' @param ros ROS range for search query in km/h
#' @param temperature Temperature range for search query in C
#' @param rh Relative Humidity range for search query in %
#' @param windspeed Wind speed range for search query in km/h
#' @param progressions Logical. If `TRUE`, return an `sf` object with progressions. If `FALSE`, don't return progressions (to save processing time).
#' @param dbpassword Database password for connecting to the fire progression database.
#'
#' @return An `sf` object (default) or a `data.frame` containing fire progression attributes.
#' @export
#'
#' @examples
#' #
fire_search_ros <- function(fire_bbox = fire_bbox_polygon(),
                            start_time = as.POSIXct("1950-01-01"),
                            end_time = as.POSIXct("2050-01-01"),
                            ros=c(0,100),
                            temperature = c(0,100),
                            rh=c(0,100),
                            windspeed=c(0,100),
                            progressions = TRUE,
                            dbpassword) {

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

  #don't reconnect if already connected
  if(!exists("DB")){

    DB <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = "cermb_fires",
      user = "mstorey",
      password = dbpassword,
      host = "charus.ad.uow.edu.au",
      port = 5432
    )

    on.exit(DBI::dbDisconnect(DB), add = TRUE)
  }
  # Simplify input polygon shape for faster spatial intersection
  fire_bbox <- fire_bbox %>%
    sf::st_transform(4283) %>%
    sf::st_convex_hull() %>% # Convert to convex hull for broader match
    sf::st_union() %>%
    sf::st_as_sf()

  # Convert polygon to WKT for SQL query
  txt_geom <- sf::st_as_text(sf::st_geometry(fire_bbox), EWKT = TRUE)


  #convert to ms to match db
  windspeed <-  windspeed/3.6#stored in ms in db
  temperature <- temperature+273.15


  #make sql query for weather
  txt_wind <- glue::glue("sfcwind_mean BETWEEN {windspeed[1]} AND {windspeed[2]}")
  txt_temp <- glue::glue("tas_mean BETWEEN {temperature[1]} AND {temperature[2]}")
  txt_rh <- glue::glue("hurs_mean BETWEEN {rh[1]} AND {rh[2]}")
  txt_weath <- glue::glue("{txt_wind} AND {txt_temp} AND {txt_rh}")

  # Create SQL date condition
  txt_date <- paste0(
    "time_utc_hourly between '",
    format(start_time, format = "%Y-%m-%d %H:%M:%S"),
    "' AND '",
    format(end_time, format = "%Y-%m-%d %H:%M:%S"),
    "'"
  )

  # # Build SQL query to select with or without geometry
  # if (return_geom == TRUE) {
  #   myquery <- paste0(
  #     "SELECT dt_utc, dt_local, rowid, s3name, firetype, notes, sourceshp, firename, season, sourcetype, aus_state, scanorgnl, geom FROM fires.prog_polygons WHERE st_intersects(fires.prog_polygons.geom,'",
  #     txt_geom, "') AND ", txt_date
  #   )
  #   x <- sf::st_read(dsn = DB, query = myquery) %>%
  #     sf::st_as_sf() %>%
  #     dplyr::arrange(dplyr::desc(dt_local))
  # }

  #get weather
  myquery <- paste0(
    "SELECT lineid
       FROM fires.weather
       WHERE st_intersects(fires.weather.geom,'",
    txt_geom, "') AND ", txt_date, " AND ",txt_weath
  )
  x <- DBI::dbGetQuery(DB, myquery)

  lineids <- paste0(unique(x$lineid),collapse=",")
  #no get all weather for each lineid captured in above query
  myquery <- glue::glue("SELECT lineid, start_time_utc, end_time_utc, time_utc_hourly, time_round, sfcwind_mean, tas_mean, hurs_mean
                           FROM fires.weather
                           WHERE lineid IN ({lineids})")

  x <- DBI::dbGetQuery(DB, myquery)




  #get the lines via lineids in weather data

  myquery <- glue::glue("SELECT * FROM fires.lines WHERE lineid IN ({lineids})")

  lines.x <- sf::st_read(dsn = DB, query = myquery) %>%
    sf::st_as_sf()
  # %>%
  #   dplyr::select(start_time_utc,end_time_utc,start_time_local,end_time_local,analysis_filter,line_km_epsg4283,ros_kmh_epsg4283,
  #                 lineid,fire_id,fire_name,state,start_polyid,end_polyid)


  #filter by user ros
  lines.x <- lines.x %>%
    dplyr::filter(ros_kmh_epsg4283 >= ros[1] & ros_kmh_epsg4283 <= ros[2])

  x <- x %>%
    dplyr::filter(lineid %in% lines.x$lineid)

  if(progressions==T){
    #get progressions via progression  ids in line data
    progids <- paste0(unique(c(lines.x$start_polyid,lines.x$end_polyid)),collapse=",")
    myquery <- glue::glue("SELECT * FROM fires.prog_polygons WHERE rowid IN ({progids})")
    progs.x <- sf::st_read(dsn = DB, query = myquery) %>%
      sf::st_as_sf()

  }else{
    progs.x=NULL
  }


  # Disconnect from database
  #DBI::dbDisconnect(conn = DB)

  if (nrow(x) == 0) {
    message("no results")
    return(invisible(NULL))
  } else {
    message(paste0(nrow(lines.x), " ros lines"))
    return(list(weather=x,lines=lines.x,progressions=progs.x))
  }
}
