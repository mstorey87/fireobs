#' Search for Landsat and Sentinel-2 imagery from Geoscience Australia's STAC
#'
#' @description
#' Searches the Digital Earth Australia (DEA) STAC API for Landsat and Sentinel-2 images
#' that intersect with a given fire boundary polygon and fall within a specified date range.
#' The function returns a data frame containing cloud storage paths to the bands needed
#' for false-colour composites, which can then be passed to `fire_stac_download()`.
#'
#' All pages of results are retrieved, so large areas or long time windows are not truncated.
#' For details on the DEA STAC service, see:
#' [DEA Knowledge Base](https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/).
#'
#' @param fire_bbox An `sf` polygon (e.g., fire extent) used to spatially filter available imagery.
#' @param start_time Start of the search window: POSIXct (any time zone), or a Date / "YYYY-mm-dd"
#'   string, which is taken as midnight local time at the fire. Converted to UTC for the search.
#' @param end_time End of the search window, in the same forms as `start_time`.
#' @param collection_names Character vector of product IDs to search for. Defaults to Landsat 5, 7, 8, 9,
#'   and Sentinel-2A, 2B and 2C.
#'
#' @return A data frame with image band URLs, product names, acquisition timestamps,
#' local datetimes, and a tile/date identifier for file naming. If no images are found,
#' a message is given and `NULL` is returned (invisibly).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#' # Example bounding box: a square around an area of interest
#' fire_bbox <- st_as_sfc(st_bbox(c(xmin = 149.1, ymin = -35.4, xmax = 149.2, ymax = -35.3)), crs = 4326) %>%
#'   st_as_sf(crs=4326)
#'
#' # Define search time window
#' start_time <- as.POSIXct("2023-01-01", tz = "Australia/Sydney")
#' end_time <- as.POSIXct("2023-01-07", tz = "Australia/Sydney")
#'
#' # Run the search
#' stac_results <- fire_stac_search(fire_bbox, start_time, end_time)
#'
#' head(stac_results)
#'
#' outdir <- "C:/temp/landsat_sentinel"
#' dir.create(outdir)
#' fire_stac_download(fire_bbox, stac_results, outdir)
#' }
fire_stac_search <- function(
    fire_bbox,
    start_time,
    end_time,
    collection_names = c(
      "ga_ls7e_ard_3",
      "ga_ls8c_ard_3",
      "ga_ls9c_ard_3",
      "ga_ls5t_ard_3",
      "ga_s2am_ard_3",
      "ga_s2bm_ard_3",
      "ga_s2cm_ard_3"
    )
) {

  # Local time zone of the fire (for Date/character inputs and local time columns)
  my_tz <- fire_get_timezone(fire_bbox)

  # Accept POSIXct as is; convert Date or "YYYY-mm-dd" strings to POSIXct (midnight, local time at the fire)
  to_posixct <- function(x, arg) {
    if (inherits(x, "POSIXct")) return(x)
    # only these formats, so e.g. "01/12/2019" is rejected rather than read as year 0001
    x2 <- tryCatch(as.POSIXct(as.character(x), tz = my_tz,
                              tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M", "%Y-%m-%d")),
                   error = function(e) NA)
    if (length(x2) != 1 || is.na(x2)) {
      stop(arg, " must be a POSIXct date-time, a Date, or a 'YYYY-mm-dd' string.", call. = FALSE)
    }
    x2
  }
  start_time <- to_posixct(start_time, "start_time")
  end_time <- to_posixct(end_time, "end_time")
  if (end_time < start_time) stop("end_time must be after start_time", call. = FALSE)

  start_time <- lubridate::with_tz(start_time, tz = "UTC")
  end_time <- lubridate::with_tz(end_time, tz = "UTC")

  start_time <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")
  end_time <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")

  # Connect to DEA STAC API
  stac_source <- rstac::stac(
    "https://explorer.dea.ga.gov.au/stac"
  )

  # Search query
  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(fire_bbox, 4326)),
    collections = collection_names,
    datetime = paste0(start_time, "/", end_time),
    limit = 999
  )
  # Get all pages of results (a single request returns at most `limit` items)
  executed_stac_query <- rstac::get_request(stac_query) %>%
    rstac::items_fetch(progress = FALSE)

  # Extract assets from features
  dat.x <- purrr::map(executed_stac_query$features, ~ tibble::tibble(
    nbart_red = .x$assets$nbart_red$href,
    nbart_swir_1 = .x$assets$nbart_swir_1$href,
    nbart_swir_2 = .x$assets$nbart_swir_2$href,
    nbart_swir_3 = .x$assets$nbart_swir_3$href,
    nbart_nir = .x$assets$nbart_nir$href,
    nbart_nir_1 = .x$assets$nbart_nir_1$href,
    nbart_nir_2 = .x$assets$nbart_nir_2$href,
    product = .x$collection,
    datetime = .x$properties$datetime
  )) %>%
    dplyr::bind_rows()

  if (nrow(dat.x) == 0) {
    message("No images found for ", paste(collection_names, collapse = ", "),
            " between ", start_time, " and ", end_time, " (UTC).")
    return(invisible(NULL))
  }

  if (nrow(dat.x) > 0) {

    # Sentinel-2 products: select bands for false-colour composite
    dat.x.sentinel2 <- dat.x %>% dplyr::filter(stringr::str_starts(product, "ga_s2"))
    if (nrow(dat.x.sentinel2) > 0) {
      dat.x.sentinel2 <- dat.x.sentinel2 %>%
        dplyr::mutate(band1 = nbart_swir_3, band2 = nbart_swir_2, band3 = nbart_red)
    }

    # Landsat products: select bands for false-colour composite
    dat.x.landsat <- dat.x %>% dplyr::filter(stringr::str_starts(product, "ga_ls"))
    if (nrow(dat.x.landsat) > 0) {
      dat.x.landsat <- dat.x.landsat %>%
        dplyr::mutate(band1 = nbart_swir_2, band2 = nbart_nir, band3 = nbart_red)
    }

    dat.aws <- dplyr::bind_rows(dat.x.landsat, dat.x.sentinel2) %>%
      dplyr::select(band1, band2, band3, datetime, product) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("band"),
                                  ~ stringr::str_replace(.x, "s3://dea-public-data", "https://dea-public-data.s3.ap-southeast-2.amazonaws.com")
      ))

    dat.aws <- dat.aws %>%
      dplyr::mutate(
        datetimeutc = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
        datetimelocal = lubridate::with_tz(datetimeutc, tz = my_tz),
        datetimelocal_chr = format(datetimelocal, format = "%Y%m%d%H%M%S"),
        tile_dateutc = paste0(
          purrr::map_chr(stringr::str_split(basename(band1), "_"), 5), "_",
          purrr::map_chr(stringr::str_split(basename(band1), "_"), 6)
        )
      ) %>%
      dplyr::select(dplyr::matches("datetime|product|tile"), dplyr::everything()) %>%
      dplyr::arrange(datetimelocal)

    return(dat.aws)
  }
}
