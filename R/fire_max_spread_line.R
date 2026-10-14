#' Find lines of maximum fire spread between consecutive fire polygons
#'
#' @description
#' This function analyzes a series of fire progression polygons (sf objects) each timestamped with a POSIXct datetime field.
#' It identifies lines representing the maximum spatial spread of the fire between consecutive polygons in time.
#' The second polygon should ideally fully contain the first, but the function may still work if this is not exactly true.
#' Return line length, bearing and rate of spread. Rate of spread is calculated with projected CRS (suitable for Australia)
#'
#' Features:
#' - Optionally converts polygons to their convex hulls to speed up calculations (less accurate).
#' - Can return all spread lines or only the maximum spread line between polygon pairs.
#' - Optionally filters to only lines fully contained within the later polygon (slower and may not always find a solution - NOTE this option needs correcting, so is not included yet).
#' - Uses minimum and maximum time interval constraints (in minutes) between polygon pairs to control comparisons.
#' - Polygon boundaries are densified by adding extra vertices (every `densify_m` meters) to improve spatial accuracy of spread line detection.
#'
#' @param polygons An sf object of fire polygons with geometry and attribute data.
#' @param time_col Character; name of the POSIXct datetime column indicating the progression time.
#' @param id_col Character; name of a unique identifier column for the polygons, which will be retained in the output.
#' @param convex_hull Logical; if TRUE, polygons are converted to convex hulls before processing (faster, less precise).
#' @param max_only Logical; if TRUE, returns only the single maximum spread line per polygon pair.
# #' @param internal_only Logical; if TRUE, returns only spread lines fully contained within the later polygon.
#' @param min_minutes Numeric; minimum allowable time difference (minutes) between polygon pairs for analysis.
#' @param max_minutes Numeric; maximum allowable time difference (minutes) between polygon pairs for analysis.
#' @param densify_m Numeric; distance in meters at which to add extra vertices along polygon edges to increase spatial resolution.
#'
#' @return An sf object of line geometries representing fire spread lines, with attributes including start/end times, spread distance, rate of spread (km/h), and line bearing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(lubridate)
#'
#' # Projected CRS in metres
#' crs_proj <- 3112
#'
#' # Centre point (lon, lat)
#' center <- c(149.1, -35.8)
#'
#' # Simple ellipse polygon builder
#' create_simple_ellipse <- function(center_xy, a, b, n = 20, angle = 0) {
#'   t <- seq(0, 2 * pi, length.out = n)
#'   x <- a * cos(t)
#'   y <- b * sin(t)
#'
#'   theta <- angle * pi / 180
#'   rot_mat <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol = 2)
#'   coords <- cbind(x, y) %*% rot_mat
#'
#'   coords[, 1] <- coords[, 1] + center_xy[1]
#'   coords[, 2] <- coords[, 2] + center_xy[2]
#'
#'   coords <- rbind(coords, coords[1, ])   # close the ring
#'   st_polygon(list(coords))
#' }
#'
#' # Project the centre point
#' center_proj <- st_transform(st_sfc(st_point(center), crs = 4326), crs_proj)
#' center_proj_xy <- st_coordinates(center_proj)[1, 1:2]
#'
#' # Three nested ellipses, each growing along the 30-degree axis
#' poly1 <- create_simple_ellipse(center_proj_xy - 900,          a = 500,  b = 300,  angle = 30)  # 12:00
#' poly2 <- create_simple_ellipse(center_proj_xy,                a = 2000, b = 1000, angle = 30)  # 14:00
#' poly3 <- create_simple_ellipse(center_proj_xy + c(600, 350),  a = 3500, b = 1800, angle = 50)  # 16:00
#'
#' # sf object with three sequential polygons
#' fire_polygons <- st_sf(
#'   fire_id = rep("fire_1", 3),
#'   date = as.POSIXct(
#'     c("2023-01-01 12:00:00", "2023-01-01 14:00:00", "2023-01-01 16:00:00"),
#'     tz = "Australia/Sydney"
#'   ),
#'   geometry = st_sfc(poly1, poly2, poly3, crs = crs_proj)
#' )
#'
#' # Quick check that each polygon contains the previous one
#' st_within(fire_polygons[1, ], fire_polygons[2, ], sparse = FALSE)
#' st_within(fire_polygons[2, ], fire_polygons[3, ], sparse = FALSE)
#'
#' spread_lines <- fire_max_spread_line(
#'   polygons    = fire_polygons,
#'   time_col    = "date",
#'   id_col      = "fire_id",
#'   convex_hull = TRUE,
#'   max_only    = TRUE,
#'   min_minutes = 30,
#'   max_minutes = 360,
#'   densify_m   = 50
#' )
#'
#' print(fire_polygons)
#' mapview::mapview(fire_polygons, zcol = "date") + spread_lines
#' }
fire_max_spread_line <- function(polygons,
                                 time_col,
                                 id_col,
                                 convex_hull = TRUE,
                                 max_only = TRUE,
                                 #internal_only = FALSE,
                                 min_minutes = 20,
                                 max_minutes = 300,
                                 densify_m = 100) {

  #set this to false, i need to fix this part of the code later
  internal_only = FALSE

  # Ensure coordinate reference system (CRS) is projected for accurate distance measurements
  # (distances are calculated in the CRS units, so it must be a projected CRS in metres)
  crs_poly <- sf::st_crs(polygons)
  if (is.na(crs_poly)) {
    stop("polygons has no CRS. Set it with sf::st_set_crs(), then transform to a projected CRS in metres, ",
         "e.g. sf::st_transform(polygons, 3112).", call. = FALSE)
  }
  if (isTRUE(sf::st_is_longlat(crs_poly))) {
    stop("polygons uses a geographic (longitude/latitude) CRS: ", crs_poly$input, ". ",
         "Transform it to a projected CRS in metres first, e.g. sf::st_transform(polygons, 3112).", call. = FALSE)
  }

  # Copy relevant columns with standard names for easier handling
  polygons$time <- polygons[[time_col]]
  polygons$rowid <- polygons[[id_col]]

  # Check that the time column is of POSIXct class
  checkmate::assert_posixct(polygons$time)

  dat.poly <- polygons %>% sf::st_make_valid()

   # Add 'season' variable to group polygons by fire season (assumed to span half-years)
  dat.poly <- dat.poly %>%
    dplyr::mutate(
      semes = lubridate::semester(time),
      yr = lubridate::year(time),
      season = ifelse(semes == 1, paste0(yr - 1, "-", yr), paste0(yr, "-", yr + 1))
    ) %>%
    dplyr::select(dplyr::all_of(c("time", "rowid", "season"))) %>%
    dplyr::arrange(time)  # Sort by time ascending

  sf::st_geometry(dat.poly) <- "geom"  # Explicitly set geometry column name

  res <- list()

  # Iterate through each polygon to find max spread lines relative to previous polygons
  for (i in 1:nrow(dat.poly)) {
    print(paste("Processing polygon", i, "of", nrow(dat.poly)))

    dat.i <- dat.poly[i, ]
    time.i <- dat.i$time

    # Find prior polygons from the same season with time less than current polygon's time
    dat.prior.all <- dat.poly %>%
      dplyr::filter(time < time.i, season == dat.i$season) %>%
      dplyr::mutate(mins_diff = as.numeric(difftime(time.i, time, units = "mins")))

    # Filter prior polygons within time range and spatially near current polygon
    dat.prior.filtered <- dat.prior.all %>%
      dplyr::filter(mins_diff >= min_minutes & mins_diff <= max_minutes) %>%
      sf::st_filter(dat.i, .predicate = sf::st_is_within_distance, dist = units::set_units(1, "m"))

    # Skip if no suitable prior polygons found
    if (nrow(dat.prior.filtered) == 0 || !is.data.frame(dat.prior.filtered)) next

    # Select the most recent prior polygon
    dat.prior <- dat.prior.filtered %>%
      dplyr::filter(time == max(time)) %>%
      dplyr::summarise(time = unique(time), rowid = paste0(unique(rowid), collapse = ";"))

    # Optionally convert polygons to convex hulls to speed processing
    if (convex_hull) {
      dat.i <- dat.i %>% sf::st_convex_hull()
      dat.prior <- dat.prior %>% sf::st_convex_hull()
    }

    # Calculate spread lines between current and prior polygon boundaries
    dat.lines <- fire_RANN_nearest_points(
      dat.i,
      dat.prior,
      densify_m = densify_m,
      max_only = max_only,
      within_only = internal_only
    )

    #reverse lines to it reflects where spread started
    dat.lines <- sf::st_reverse(dat.lines)

    # Attach polygon attributes from current polygon to lines
    dat.lines <- cbind(dat.lines, sf::st_drop_geometry(dat.i))

    # Handle potential issues where prior polygon boundary points overlap other prior polygons (fire merging)
    dat.lines.start <- sf::st_cast(sf::st_geometry(dat.lines), "POINT")[1, ] %>%
      sf::st_as_sf() %>%
      sf::st_transform(3112) %>%
      sf::st_buffer(1) %>%
      sf::st_transform(sf::st_crs(dat.lines))

    # Find intersections with all prior polygons
    dat.prior.intersect <- suppressWarnings(sf::st_intersection(dat.lines.start, dat.prior.all))
    min_time_any <- min(dat.prior.intersect$time)
    min_time_any_diff <- as.numeric(difftime(dat.i$time, min_time_any, tz = "utc", units = "mins"))

    # Calculate total length of the spread line in meters
    dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))
    if (all(dat.lines$line_metres== 0) ) next

    # Calculate length of spread line inside the current polygon
    len.l.i.intersect <- seq_len(nrow(dat.lines)) %>%
      purrr::map_dbl(~ {
        intersection <- tryCatch(
          suppressWarnings(sf::st_intersection(dat.lines[.x, ], dat.i)),
          error = function(e) NULL
        )
        if (is.null(intersection) || nrow(intersection) == 0) {
          0
        } else {
          as.numeric(sf::st_length(intersection))
        }
      })

    dat.lines$line_metres_internal <- len.l.i.intersect

    # Percent of line length inside current polygon
    dat.lines$percent_internal <- round(dat.lines$line_metres_internal / dat.lines$line_metres * 100, 1)

    # Find IDs of polygons where the spread line starts
    polyid <- unlist(unique(sf::st_is_within_distance(dat.lines, dat.prior, dist = units::set_units(1, "m"))))
    start_rowids <- paste0(dat.prior$rowid[polyid], collapse = ";")

    # Add metadata about spread and rate of spread
    dat.lines <- dat.lines %>%
      dplyr::mutate(
        start_time = unique(dat.prior$time),
        end_time = unique(dat.i$time),
        start_pid = start_rowids,
        end_pid = unique(dat.i$rowid),
        timestep_mins = as.numeric(difftime(end_time, start_time, units = "mins")),
        line_km = line_metres / 1000,
        percent_internal = percent_internal,
        ros_kmh = line_km / (timestep_mins / 60),
        min_time_any = min_time_any,
        min_time_any_diff = min_time_any_diff
      ) %>%
      dplyr::select(
        start_time, end_time, start_pid, end_pid, line_km, percent_internal,
        timestep_mins, ros_kmh, min_time_any, min_time_any_diff
      )

    # Calculate start/end coordinates and bearing of each spread line
    # line_results <- lapply(1:nrow(dat.lines), function(i) {
    #   line <- sf::st_geometry(dat.lines)[i] %>% sf::st_transform(4283)
    #    coords <- sf::st_coordinates(line)
    #   # coords <- coords[nrow(coords):1, ]  # reverse order to get correct direction
    #
    #   start_point <- coords[1, ]
    #   end_point <- coords[nrow(coords), ]
    #
    #   bearing_deg <- stplanr::line_bearing(line %>% sf::st_as_sf(), bidirectional = FALSE)
    #   # Normalize bearing to opposite direction (180 degrees shifted)
    #   bearing_deg <- ifelse(bearing_deg < 180, bearing_deg + 180, bearing_deg - 180)
    #
    #   data.frame(
    #     start_x_gda94 = start_point[1], start_y_gda94 = start_point[2],
    #     end_x_gda94 = end_point[1], end_y_gda94 = end_point[2],
    #     bearing = bearing_deg
    #   )
    # })
    line_results <- fire_get_line_info(dat.lines %>%  sf::st_transform(4326))

    dat.lines <- cbind(dat.lines, line_results) %>% sf::st_as_sf()

    # Flag lines crossing polygons with prior times (may indicate fire merging with previous fires)
    dat.prior.all.2 <- dat.prior.all %>% dplyr::filter(mins_diff >= min_minutes)
    dat.lines <- dat.lines %>% dplyr::mutate(lineid=dplyr::row_number())
    dat.lines.intersect <-  suppressWarnings(sf::st_intersection(dat.lines, dat.prior.all.2)) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(len = as.numeric(sf::st_length(.))) %>%
      dplyr::filter(len > 0.01) %>%
      dplyr::select(lineid) %>%
      dplyr::mutate(prior_intersects=TRUE) %>%
      sf::st_drop_geometry()

    dat.lines <- dat.lines %>%
      dplyr::left_join(dat.lines.intersect,by="lineid") %>%
      dplyr::select(-lineid) %>%
      dplyr::mutate(prior_intersects=ifelse(is.na(prior_intersects),FALSE,prior_intersects))

    res[[i]] <- dat.lines
  }

  # Combine results from all iterations
  res.all <- do.call(rbind, res)

  return(res.all)
}
