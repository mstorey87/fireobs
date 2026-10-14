#' Landsat tiles for Australia
#'
#' A subset of landsat tiles with Path and Row that intersect Australia
#' @format ## `Used in a function to find which landsat tiles intersect with a fire, then construct paths to search NCI`
#' An sf object with 404 rows and 4 columns:
#' \describe{
#'   \item{PATH}{Landsat Path Number}
#'   \item{ROW, iso3}{Landsat Row number}
#'   \item{WRSPR}{Path and Row string}
#'   ...
#' }
#' @source <https://www.usgs.gov/media/files/landsat-wrs-2-descending-path-row-shapefile>
"dat.landsat.pathrow"

#' Sentinel 2 tiles for Australia
#'
#' A subset of Sentinel 2 tiles with Path and Row that intersect Australia
#' @format ## `Used in a function to find which Sentinel 2 tiles intersect with a fire, then construct paths to search NCI`
#' An sf object with 2155 rows and 3 columns:
#' \describe{
#'   \item{Name}{Sentinel 2 Tile Name}
#'   ...
#' }
#' @source <https://sentiwiki.copernicus.eu/web/s2-mission>
"dat.sentinel2.pathrow"

#' Example fire boundary
#'
#' A fire polygon from NPWS fire history data
#' @format ## `An example fire boundary use for testing out some functions`
#' An sf object with 1 rows and 4 columns:
#' \describe{
#'   \item{FireName}{Name given to the fire}
#'   \item{StartDate}{Fire ignition date}
#'   \item{EndDate}{Fire declared extinguished date}
#'   ...
#' }
#' @source <https://datasets.seed.nsw.gov.au/dataset/fire-history-wildfires-and-prescribed-burns-1e8b6>
"dat.fire.polygon"

#' Timezone Names for Australian States
#'
#' Timezone Names for Australian States
#' @format ## `An example fire boundary use for testing out some functions`
#' \describe{
#'   \item{name}{State Name}
#'   \item{tz_name}{timezone name for posixct tz parameter}
#'   ...
#' }
"dat.timezone.names"

#' Australian state and territory boundaries
#'
#' Polygons of Australian states, territories and external island territories.
#' Used by [fire_get_timezone()] to find which state a fire is in, so the
#' matching time zone can be looked up in [dat.timezone.names].
#'
#' @format An `sf` object with 11 rows and 2 columns, CRS EPSG:3112
#'   (GDA94 / Geoscience Australia Lambert):
#' \describe{
#'   \item{name}{State or territory name. Must match `name` in [dat.timezone.names].}
#'   \item{geometry}{MULTIPOLYGON boundary.}
#' }
#' @source rnaturalearth package
"dat.aus"
