#' Get start and end coordinates, bearing, and length of an sf line
#'
#' @description
#' This function takes an `sf` object containing LINESTRING or MULTILINESTRING geometries
#' with a geographic coordinate reference system (CRS). It extracts for each line:
#' - The start coordinates (x, y)
#' - The end coordinates (x, y)
#' - The bearing (direction) in degrees from start to end (0-360°)
#' - The line length in kilometers
#'
#' Note: The function requires the input `sf` object to have a geographic CRS (e.g., EPSG:4326).
#' It will throw an error if the CRS is projected.
#'
#' @param sf_lines An `sf` object containing line geometries with geographic CRS.
#' @param reverse Logical; if TRUE, returned direction will be reversed.
#'
#' @return A data frame with columns for start/end coordinates, bearing (degrees), and line length in km.
#' Column names are suffixed with the EPSG code of the CRS.
#'
#' @export
#'
#' @examples
#' library(sf)
#' library(geosphere)
#'
#' # Define a start point (longitude, latitude)
#' start <- c(150.0, -33.0)
#' # Calculate end point 10 km away at bearing 45 degrees (NE)
#' end <- geosphere::destPoint(start, 45, 10000)
#' # Create a LINESTRING from start to end
#' line <- st_linestring(rbind(start, end))
#' # Create sf object with geographic CRS EPSG:4326
#' sf_line <- st_sf(geometry = st_sfc(line, crs = 4326))
#'
#' # Get line info: start/end coords, bearing, and length (km)
#' fire_get_line_info(sf_line,reverse=FALSE)
fire_get_line_info <- function(sf_lines,reverse=F) {

  checkmate::assert_class(sf_lines, "sf")
  crs1 <- sf::st_crs(sf_lines)
  checkmate::assert(
    stringr::str_starts(crs1$wkt, "GEOGCRS"),
    paste0("Error: sf_lines must have a geographic CRS (e.g., EPSG:4326). Detected: ", crs1$input)
  )

  # Initialize results list, one element per line
  results <- lapply(1:nrow(sf_lines), function(i) {
    # Extract geometry of current line
    line <- sf::st_geometry(sf_lines)[i]

    # Extract coordinates matrix of the line
    coords <- sf::st_coordinates(line)

    # Get start point  and end point
    start_point <- coords[1, 1:2]
    end_point <- coords[nrow(coords), 1:2]


    # If reverse = TRUE, swap start and end points
    if (reverse) {
      tmp <- start_point
      start_point <- end_point
      end_point <- tmp
    }


    # Calculate line length in kilometers
    dist <- as.numeric(sf::st_length(line)) / 1000

    # Calculate bearing from start to end
    direction <- geosphere::bearing(start_point, end_point)
    if (direction < 0) direction <- direction + 360  # Normalize to [0, 360]

    #check for zero length, and apply NA direction
    if(dist==0){
      direction=NA
    }

    # Return results as named vector
    return(c(
      start_x = start_point[1], start_y = start_point[2],
      end_x = end_point[1], end_y = end_point[2],
      direction = direction,
      dist = dist
    ))
  })

  # Combine list of vectors into a data.frame
  results_df <- do.call(rbind, results) %>% as.data.frame()

  # Rename columns to include CRS EPSG code suffix
  names(results_df) <- paste0(
    c("start_x", "start_y", "end_x", "end_y", "bearing", "line_km"),
    "_epsg", sf::st_crs(sf_lines)$epsg
  )

  return(results_df)
}

