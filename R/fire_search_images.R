#' Search database for fire line scan polygons
#'
#' This function connects to the CERMB fire database and searches for line scan
#' bounding boxes that intersect the provided fire bounding polygon within a given
#' date-time range. It simplifies the input polygon to ensure the query runs efficiently.
#'
#' @param fire_bbox An `sf` polygon or multipolygon (e.g. fire boundary) to search for intersecting images.
#' @param start_time The start date-time (`POSIXct`) for the search window (e.g. "YYYY-mm-dd").
#' @param end_time The end date-time (`POSIXct`) for the search window (e.g. "YYYY-mm-dd").
#' @param dbpassword A character string with the database password.
#' @param RDS Logical. If TRUE, connect to Amazon RDS database. If FALSE, connect to UOW database.
#'
#' @return An `sf` object containing any line scan polygons found in the database.
#' If no results are found, a message is printed and `NULL` is returned.
#'
#' @export
#'
#' @examples
#' # Example: Search for images in a sample bounding box in the Blue Mountains fire area
#' library(sf)
#' library(lubridate)
#'
#' # Create a simple example polygon
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
#' # Define search period
#' start_time <- as.POSIXct("2019-12-01", tz = "Australia/Sydney")
#' end_time <- as.POSIXct("2019-12-03", tz = "Australia/Sydney")
#'
#' # Call function (replace 'mypassword' with your actual password)
#' # images <- fire_search_images(blue_mtns_bbox, start_time, end_time, dbpassword = "mypassword")
fire_search_images <- function(fire_bbox = fire_bbox_polygon(), start_time, end_time, dbpassword,RDS=FALSE) {

  # Check that start and end time are POSIXct
  checkmate::assert(inherits(start_time, "POSIXct"), "Error: times must be POSIXct")
  checkmate::assert(inherits(end_time, "POSIXct"), "Error: times must be POSIXct")

  # Check that fire_bbox is valid sf with CRS
  checkmate::assert(inherits(fire_bbox, "sf"))
  checkmate::assert(!is.na(sf::st_crs(fire_bbox)), "fire_bbox must have a valid CRS")

  # Ensure times are converted to local time zone based on fire_bbox location
  my_tz <- fire_get_timezone(fire_bbox)
  start_time <- lubridate::with_tz(start_time, tz = my_tz)
  end_time <- lubridate::with_tz(end_time, tz = my_tz)

  # Check that dbpassword is not empty
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

  # Simplify fire polygon shape (convex hull)
  # Useful for speeding up intersection checks with large/complex fire boundaries
  fire_bbox <- fire_bbox %>%
    sf::st_transform(4283) %>%
    # sf::st_concave_hull(ratio = 0.8) %>%
    sf::st_convex_hull() %>%
    sf::st_union() %>%
    sf::st_as_sf()

  sf::st_geometry(fire_bbox) <- "geometry"

  # Extract geometry as EWKT for SQL
  txt_geom <- sf::st_as_text(fire_bbox$geometry, EWKT = TRUE)

  # Create date range SQL text
  txt_date <- paste0(
    "datetimelocal BETWEEN '",
    format(start_time, format = "%Y-%m-%d %H:%M:%S"),
    "' AND '",
    format(end_time, format = "%Y-%m-%d %H:%M:%S"), "'"
  )

  # Build query: intersect polygon AND within date range
  myquery <- paste0(
    "SELECT * FROM fires.scanboundingboxes ",
    "WHERE st_intersects(fires.scanboundingboxes.geom, '", txt_geom, "') ",
    "AND ", txt_date
  )

  # Run query and return results as sf
  x <- sf::st_read(dsn = DB, query = myquery) %>%
    sf::st_as_sf()

  if (nrow(x) == 0) {
    print("no results")
  } else {
    print(paste0(nrow(x), " results"))
    return(x)
  }

}
