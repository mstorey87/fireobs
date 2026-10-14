#' Find maximum fire spread line using nearest-neighbor search (RANN)
#'
#' @description
#' This function finds lines of maximum spread between two consecutive fire progression polygons
#' using a fast nearest-neighbor search powered by the **RANN** package.
#'
#' The function works by converting polygon boundaries to dense point sets and then connecting
#' the farthest pairs of points between the earlier (`poly.prior`) and later (`poly`) polygons.
#' Optionally, you can restrict lines to only those fully contained within the later polygon,
#' which is slower but sometimes useful for strict containment.
#'
#' @param poly An `sf` polygon of the later fire progression.
#' @param poly.prior An `sf` polygon of the earlier fire progression.
#' @param densify_m Numeric. Maximum spacing in meters between vertices when densifying the polygons' boundaries
#'   (e.g. 100). If 0, only the existing polygon vertices are used, which can overestimate the spread distance
#'   (a warning is given).
#' @param within_only Logical. If `TRUE`, return only spread lines that are fully contained within `poly` (slower).
#' @param max_only Logical. If `TRUE` (default), return only the single maximum spread line.
#' @param within_buffer_m Numeric. Buffer distance (meters) to exclude vertices near `poly.prior` when `within_only = TRUE`.
#'
#' @return An `sf` object of LINESTRING geometries representing maximum fire spread lines.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Create two simple overlapping squares
#' poly1 <- st_polygon(list(rbind(
#'   c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
#' )))
#' poly2 <- st_polygon(list(rbind(
#'   c(0.5,0.5), c(1.5,0.5), c(1.5,1.5), c(0.5,1.5), c(0.5,0.5)
#' )))
#'
#' poly_sf1 <- st_sf(geometry = st_sfc(poly1),crs=4326) |> st_transform(3112)
#' poly_sf2 <- st_sf(geometry = st_sfc(poly2),crs=4326)|> st_transform(3112)
#'
#' # Find maximum spread line
#' line_sf <- fire_RANN_nearest_points(
#'   poly = poly_sf2,
#'   poly.prior = poly_sf1,
#'   densify_m = 100,
#'   max_only = TRUE
#' )
#'
#' plot(st_geometry(poly_sf1), col = NA, border = "blue")
#' plot(st_geometry(poly_sf2), add = TRUE, col = NA, border = "red")
#' plot(st_geometry(line_sf), add = TRUE, col = "black", lwd = 2)
#' }
fire_RANN_nearest_points <- function(poly, poly.prior, densify_m, within_only = FALSE, max_only = TRUE, within_buffer_m = 10) {

  # Distances are calculated in the CRS units, so both polygons need the same projected CRS (metres)
  for (arg in c("poly", "poly.prior")) {
    crs_x <- sf::st_crs(get(arg))
    if (is.na(crs_x)) {
      stop(arg, " has no CRS. Set it with sf::st_set_crs(), then transform to a projected CRS in metres, ",
           "e.g. sf::st_transform(", arg, ", 3112).", call. = FALSE)
    }
    if (isTRUE(sf::st_is_longlat(crs_x))) {
      stop(arg, " uses a geographic (longitude/latitude) CRS: ", crs_x$input, ". ",
           "Transform it to a projected CRS in metres first, e.g. sf::st_transform(", arg, ", 3112).", call. = FALSE)
    }
  }
  if (sf::st_crs(poly) != sf::st_crs(poly.prior)) {
    stop("poly and poly.prior have different CRSs (", sf::st_crs(poly)$input, " and ", sf::st_crs(poly.prior)$input,
         "). Transform them to the same projected CRS.", call. = FALSE)
  }

  checkmate::assert_number(densify_m, lower = 0)
  if (densify_m == 0) {
    warning("densify_m = 0: only the existing polygon vertices are used, so the nearest point on the earlier ",
            "polygon may be a corner rather than the middle of an edge, and the spread distance can be ",
            "overestimated. Use densify_m > 0 (e.g. 100).", call. = FALSE)
  }

  # Helper: Convert polygon boundary to dense points
  fn_poly_to_points <- function(poly_x, densify_m) {
    if (densify_m > 0) {
      poly_x1 <- poly_x %>%
        sf::st_segmentize(dfMaxLength = densify_m) %>%
        sf::st_sf() %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = sf::st_crs(poly_x))
      return(poly_x1)
    } else {
      return(poly_x)
    }
  }

  # Create union geometry of both polygons (for within tests)
  poly.union <- sf::st_union(sf::st_geometry(poly), sf::st_geometry(poly.prior)) %>%
    sf::st_make_valid()

  # Convert both polygons to points
  points.A <- fn_poly_to_points(poly.prior, densify_m)
  points.B <- fn_poly_to_points(poly.union, densify_m)

  # --- OPTION 1: Faster method, allows crossing poly boundary ---
  if (within_only == FALSE) {
    # Extract coordinates
    coords2 <- sf::st_coordinates(points.B)[, 1:2]
    coords1 <- sf::st_coordinates(points.A)[, 1:2]

    # Find nearest neighbors (using RANN)
    nn_result <- RANN::nn2(coords1, coords2, k = 1)

    distances_df <- data.frame(
      point1_index = 1:nrow(coords2),
      point2_index = nn_result$nn.idx,
      distance = nn_result$nn.dists
    )

    if (max_only == TRUE) {
      distances_df <- distances_df %>%
        dplyr::filter(distance == max(distance)) %>%
        dplyr::slice(1)
    }

    # Create LINESTRING geometries
    line_geometries <- lapply(1:nrow(distances_df), function(i) {
      point1_coords <- coords2[distances_df$point1_index[i], ]
      point2_coords <- coords1[distances_df$point2_index[i], ]
      sf::st_linestring(rbind(point1_coords, point2_coords))
    })

    lines_sf <- sf::st_sf(geometry = sf::st_sfc(line_geometries, crs = sf::st_crs(points.A)))
    return(lines_sf)
  }

  # --- OPTION 2: Restrict lines to be fully within poly (slower) ---
  if (within_only == TRUE) {
    poly.diff <- sf::st_difference(sf::st_geometry(poly), sf::st_geometry(poly.prior)) %>% sf::st_union()
    poly.diff.buff <- sf::st_buffer(poly.diff, 1)

    mypred.withindist <- function(x, y) !sf::st_is_within_distance(x, y, dist = within_buffer_m)
    points.B.x <- points.B %>%
      sf::st_filter(poly.prior, .predicate = mypred.withindist)

    nn_result.top <- RANN::nn2(sf::st_coordinates(points.A)[, 1:2],
                               sf::st_coordinates(points.B.x)[, 1:2], k = 1)
    points.B.x$dist <- nn_result.top$nn.dists
    points.B.x <- points.B.x %>%
      dplyr::arrange(dplyr::desc(dist))

    coords2 <- sf::st_coordinates(points.B.x)[, 1:2]
    coords1_original <- sf::st_coordinates(points.A)[, 1:2]

    lines.out <- list()

    for (xi in 1:nrow(coords2)) {
      coords2.xi <- matrix(coords2[xi, ], nrow = 1)
      coords1 <- coords1_original

      lines_sf <- data.frame()

      while (nrow(lines_sf) == 0 && is.matrix(coords1)) {
        nn_result <- RANN::nn2(coords1, coords2.xi, k = 1)

        lines_sf_try <- sf::st_linestring(rbind(coords2.xi, coords1[nn_result$nn.idx[[1]], ]))
        crosses <- lengths(sf::st_crosses(lines_sf_try, poly.diff.buff)) > 0

        if (!crosses) {
          lines_sf <- lines_sf_try %>%
            sf::st_sfc(crs = sf::st_crs(points.A)) %>%
            sf::st_as_sf() %>%
            dplyr::mutate(dist = as.numeric(sf::st_length(.))) %>%
            dplyr::filter(dist == min(dist)) %>%
            dplyr::sample_n(1)

          lines.out[[xi]] <- lines_sf
          break
        } else {
          lines_sf <- data.frame()
          coords1 <- coords1[-nn_result$nn.idx[[1]], ]
        }
      }
    }

    x <- do.call(rbind, lines.out) %>% sf::st_as_sf()
    x.max <- x %>%
      dplyr::mutate(dist_m = as.numeric(sf::st_length(.))) %>%
      dplyr::filter(dist_m == max(dist_m))

    return(x.max)
  }

}


