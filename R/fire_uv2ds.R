#' Wind speed and direction from U and V
#'
#' @description
#' Converts eastward (u) and northward (v) wind components, e.g. BARRA \code{uas} and \code{vas},
#' to wind speed and meteorological wind direction.
#'
#' Notes:
#' \itemize{
#'   \item \code{dir} is the meteorological wind direction: the direction the wind is blowing
#'     \strong{from}, in degrees clockwise from north (0 = from the north, 90 = from the east,
#'     180 = from the south, 270 = from the west). This matches BOM weather station data
#'     (e.g. \code{winddir} from \code{fire_cermb_weather()}).
#'   \item This is the opposite of a fire spread bearing (the direction the fire travels
#'     \strong{towards}, as in \code{fire_max_spread_line()}). A wind pushing a fire towards
#'     bearing B has \code{dir} = (B + 180) \%\% 360.
#'   \item Calm wind (u = 0 and v = 0) has no direction, so \code{dir} is \code{NA}.
#'   \item \code{speed} is in the same units as \code{u} and \code{v} (m/s for BARRA).
#' }
#'
#' @param u Eastward wind component (positive = wind blowing towards the east), e.g. BARRA \code{uas}.
#' @param v Northward wind component (positive = wind blowing towards the north), e.g. BARRA \code{vas}.
#'
#' @return A two-column matrix: \code{dir} (wind direction in degrees, 0-360, direction the wind
#'   comes from) and \code{speed} (same units as \code{u} and \code{v}).
#' @export
#'
#' @examples
#' # Example U and V components
#' u <- c(3, -2, 0)
#' v <- c(4, 2, -5)
#'
#' # Calculate wind speed and direction
#' fire_uv2ds(u, v)
#'
#' # Returns a matrix:
#' #         dir speed
#' # [1,] 216.87  5.00   # blowing towards the north-east, so from the south-west
#' # [2,] 135.00  2.83   # blowing towards the north-west, so from the south-east
#' # [3,]   0.00  5.00   # blowing towards the south, so from the north
fire_uv2ds <- function(u, v) {

  if (length(u) != length(v)) stop("u and v must be the same length", call. = FALSE)

  rad2deg <- function(rad) {
    (rad * 180) / (pi)
  }

  ###### DIRECTION (meteorological: direction the wind comes FROM)
  # atan2(u, v) would give the direction the wind blows towards; negating both
  # components turns that around to the direction it comes from
  direction <- rad2deg(atan2(-u, -v)) %% 360

  ###### SPEED
  speed <- sqrt((u * u) + (v * v))

  # Calm wind has no direction
  direction[!is.na(speed) & speed == 0] <- NA

  ######
  res <- cbind(dir = direction, speed = speed)
  return(res)
}
