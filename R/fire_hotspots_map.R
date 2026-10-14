#' Create geo-referenced hotspot and WMS images for a fire area
#'
#' @description
#' Downloads NASA GIBS WMS imagery layers and optionally overlays NASA FIRMS hotspots.
#' The function produces geotiffs clipped to the fire bounding box extent and saves
#' them to the specified folder. Hotspots geopackage also saved if requested.
#' Images cover the specified date range with daily granularity.
#'
#' @param fire_bbox Polygon sf object representing the fire area or region to search for images (expected in any CRS, reprojected internally to EPSG:4326)
#' @param start_time POSIXct start datetime of image search interval
#' @param end_time POSIXct end datetime of image search interval
#' @param mapkey NASA FIRMS API mapkey needed to download hotspot data. Get yours at https://firms.modaps.eosdis.nasa.gov/api/map_key/
#' @param dest_folder Local folder path to save output GeoTIFF images and hotspot geopackage
#' @param add_hotspots Logical (TRUE/FALSE) whether to download and overlay hotspots on the images
#' @param outwidth Numeric width in pixels for output TIFF images; height is scaled to maintain aspect ratio
#'
#' @return
#' Writes georeferenced GeoTIFF files to disk in the destination folder. Also writes hotspot geopackages.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Define a small polygon for the fire area (example coordinates, use your actual fire bbox)
#' blue_mountains_box <- st_polygon(list(rbind(
#'   c(149.32276, -32.60165),  # top-left (xmin, ymax)
#'   c(151.61254, -32.60165),  # top-right (xmax, ymax)
#'   c(151.61254, -34.63015),  # bottom-right (xmax, ymin)
#'   c(149.32276, -34.63015),  # bottom-left (xmin, ymin)
#'   c(149.32276, -32.60165)   # close polygon
#' ))) %>%
#'   st_sfc(crs = 4326)
#' # Define time range for images
#' start_time <- as.POSIXct("2019-11-21 00:00:00", tz = "Australia/Sydney")
#' end_time <- as.POSIXct("2019-11-23 00:00:00", tz = "Australia/Sydney")
#'
#' # Your NASA FIRMS mapkey (replace with your actual key)
#' my_mapkey <- "your_mapkey_here"
#'
#' # Folder to save outputs
#' output_dir <- tempdir()
#'
#' # Run function to download images with hotspots
#' fire_hotspots_map(
#'   fire_bbox = blue_mountains_box,
#'   start_time = start_time,
#'   end_time = end_time,
#'   mapkey = my_mapkey,
#'   dest_folder = output_dir,
#'   add_hotspots = TRUE,
#'   outwidth = 600
#' )
#' }
fire_hotspots_map <- function(fire_bbox, start_time, end_time, mapkey = NULL, dest_folder, add_hotspots = TRUE, outwidth = 500) {

  # Validate that start and end times are POSIXct
  checkmate::assert(stringr::str_detect(class(start_time)[1], "POSIXct"), "Error: times must be POSIXct")
  checkmate::assert(stringr::str_detect(class(end_time)[1], "POSIXct"), "Error: times must be POSIXct")
  checkmate::assert(!is.null(mapkey),"NASA FIRMS API mapkey needed to download hotspot data. Get yours at https://firms.modaps.eosdis.nasa.gov/api/map_key/")
  checkmate::assert(file.exists(dest_folder),"Error: dest_folder does not exist")

  # Reproject fire bbox to EPSG:4326, required for WMS and hotspot queries
  fire_bbox <- fire_bbox %>% sf::st_transform(4326)

  # Get timezone of fire bbox centroid to localize time inputs/outputs
  mytz <- fire_get_timezone(fire_bbox)

  # Convert input times to local timezone for filtering hotspots
  start_time <- lubridate::with_tz(start_time, tz = mytz)
  end_time <- lubridate::with_tz(end_time, tz = mytz)

  # Create sequence of image dates. GIBS daily images and FIRMS acq_date are UTC days.
  # Australian local time is ahead of UTC, so passes on a local date are in that UTC day
  # or the day before (e.g. Terra ~10:30 local = previous UTC day). Add one day before the start.
  start_date <- format(start_time, format = "%Y-%m-%d")
  end_date <- format(end_time, format = "%Y-%m-%d")
  date_seq <- seq(as.Date(start_date) - 1, as.Date(end_date), by = "1 day")

  # Optionally download hotspots from NASA FIRMS within date range and bbox
  if (add_hotspots) {
    add_hotspots_test <- TRUE

    hotspots <- fire_hotspots_search(
      fire_bbox,
      mapkey,
      min(date_seq),
      max(date_seq),
      dest_folder
    )
    message("hotspots downloaded")

    if (nrow(hotspots) > 0) {
      # Filter hotspots by local date and time range
      hotspots <- hotspots %>%
        dplyr::filter(
          datelocal %in% date_seq,
          datetimelocal > start_time & datetimelocal < end_time
        ) %>%
        sf::st_transform(4326)
    }

    # If no hotspots found, disable overlay
    if (nrow(hotspots) == 0) {
      add_hotspots_test <- FALSE
    }

  } else {
    hotspots <- NULL
  }

  # Define NASA GIBS WMS layers with infrared or thermal bands sensitive to active fire
  wms_layers <- c(
    "MODIS_Aqua_CorrectedReflectance_Bands721",
    "MODIS_Terra_CorrectedReflectance_Bands721",
    "VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1",
    "VIIRS_NOAA20_CorrectedReflectance_BandsM11-I2-I1",
    "MODIS_Aqua_Brightness_Temp_Band31_Night",
    "MODIS_Terra_Brightness_Temp_Band31_Night",
    "VIIRS_NOAA21_Brightness_Temp_BandI5_Night",
    "VIIRS_NOAA20_Brightness_Temp_BandI5_Night",
    "VIIRS_SNPP_Brightness_Temp_BandI5_Night"
  )

  # Loop over each date to download and process WMS imagery
  for (i in seq_along(date_seq)) {
    date_string <- paste0(date_seq[i], "/", date_seq[i])
    message(paste("Processing date:", date_string))

    for (wms in wms_layers) {
      # Temporary files for intermediate outputs (PNG and TIFF)
      outpng1 <- tempfile(fileext = ".png")
      outpng2 <- tempfile(fileext = ".png")
      outpng3 <- tempfile(fileext = ".png")
      outtif1 <- tempfile(fileext = ".tif")
      outtif2 <- tempfile(pattern = paste0("_", date_seq[i], "_", wms, "_"), fileext = ".tif")

      # Calculate output image dimensions based on fire_bbox aspect ratio
      bbox <- sf::st_bbox(fire_bbox)
      width <- bbox$xmax - bbox$xmin
      height <- bbox$ymax - bbox$ymin
      ratio <- height / width
      outheight <- round(outwidth * ratio)

      # Build WMS GetMap query URL and parameters
      wms_url <- 'https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi?'
      params <- list(
        SERVICE = "WMS",
        VERSION = "1.1.1",
        REQUEST = "GetMap",
        LAYERS = wms,
        SRS = "EPSG:4326",
        BBOX = paste0(bbox, collapse = ","),
        WIDTH = outwidth,
        HEIGHT = outheight,
        FORMAT = "image/png",
        TRANSPARENT = "TRUE",
        TIME = date_string
      )

      # Send WMS request
      response <- httr::GET(wms_url, query = params)

      # Check for valid PNG image response; if invalid, skip this layer/date
      if (grepl("image/png", httr::headers(response)[["content-type"]])) {
        img_data <- httr::content(response, "raw")
        writeBin(img_data, outpng1)
      } else {
        warning(paste0("Failed to download image for layer ", wms, " on ", date_string))
        next
      }

      # Convert PNG to terra raster, assign CRS and extent matching fire_bbox
      img <- png::readPNG(outpng1)
      r1 <- terra::rast(img)
      r1 <- c(r1[[1]], r1[[2]], r1[[3]])
      # readPNG gives 0-1; scale to 0-255 (stretching each band separately distorts the colours)
      r1 <- r1 * 255
      terra::crs(r1) <- sf::st_crs(4326)$wkt
      terra::ext(r1) <- terra::ext(sf::st_as_sf(fire_bbox))

      # Check if raster values are all NA, replace with default to avoid errors
      if (all(is.na(terra::values(r1)))) {
        terra::values(r1) <- 1
      }

      # Write intermediate raster to a TIFF file
      terra::writeRaster(r1, outtif1, overwrite = TRUE)

      # If hotspots are requested and available, overlay them on the image
      if (add_hotspots) {

        # If no hotspots available, just copy the image to output folder
        if (nrow(hotspots) == 0) {
          file.copy(outtif1, paste0(dest_folder, "\\x", basename(outtif2)), overwrite = TRUE)
          next
        }

        # Extract satellite abbreviation from WMS layer name for matching hotspots
        wms_lyr_name_abbrev <- stringr::str_split(wms, "_")[[1]]
        wms_lyr_name_abbrev <- tolower(paste0(wms_lyr_name_abbrev[1], "_", wms_lyr_name_abbrev[2]))

        # Filter hotspots based on day/night according to WMS layer.
        # Match by UTC date (acq_date), the same day as the GIBS image, so hotspots come from the pass shown
        if (stringr::str_detect(wms, "Night")) {
          hotspots2 <- hotspots %>%
            dplyr::filter(daynight == "N", acq_date == date_seq[i])
        } else {
          hotspots2 <- hotspots %>%
            dplyr::filter(daynight == "D", acq_date == date_seq[i])
        }

        # Add label and filter for matching satellite name, transform CRS
        hotspots2 <- hotspots2 %>%
          dplyr::filter(sat_name == wms_lyr_name_abbrev) %>%
          dplyr::mutate(label = paste0(sat_name, " ", datetimelocal)) %>%
          sf::st_transform(4326)

        # If no filtered hotspots, copy image and skip
        if (nrow(hotspots2) == 0) {
          file.copy(outtif1, paste0(dest_folder, "\\x", basename(outtif2)), overwrite = TRUE)
          next
        }

        # Plot raster and hotspots overlay to a transparent PNG
        hotspots2$datetimelocal <- as.factor(hotspots2$datetimelocal)
        n_levels <- length(levels(hotspots2$datetimelocal))
        if (n_levels < 3) {
          if (n_levels == 1) {
            color_palette <- c("red")
          } else if (n_levels == 2) {
            color_palette <- c("red", "blue")
          }
        } else {
          color_palette <- RColorBrewer::brewer.pal(n_levels, "Set1")
        }
        hotspots2$color <- color_palette[as.integer(hotspots2$datetimelocal)]

        grDevices::png(outpng2,
                       width = outwidth + (0.1 * outwidth),
                       height = outheight + (0.1 * outheight),
                       bg = "transparent",
                       units = "px"
        )

        graphics::par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))
        point_size <- (outwidth / 1000)

        terra::plotRGB(r1, axes = FALSE, mar = c(0, 0, 0, 0))
        plot(hotspots2["datetimelocal"], add = TRUE,
             col = hotspots2$color, pch = 16, cex = point_size
        )

        unique_colors <- levels(hotspots2$datetimelocal)
        label_size <- 0.7 * (outwidth / 500)
        for (color in unique_colors) {
          point <- hotspots2[hotspots2$datetimelocal == color, ]
          x_coord <- mean(sf::st_coordinates(point)[, 1])
          y_coord <- mean(sf::st_coordinates(point)[, 2])
          graphics::text(x_coord, y_coord, labels = color,
                         col = point$color[1], cex = label_size, pos = 4, font = 2
          )
        }

        grDevices::dev.off()

        # Process overlay image with magick and write final GeoTIFF with hotspots
        r <- magick::image_read(outpng2)
        r <- magick::image_trim(r, 1)
        r <- magick::image_resize(r, paste0(outwidth, "x", outheight, "!"))
        magick::image_write(r, outpng3)

        img <- png::readPNG(outpng3)
        x <- terra::rast(img)
        x <- c(x[[1]], x[[2]], x[[3]])
        terra::crs(x) <- sf::st_crs(4326)$wkt
        terra::ext(x) <- terra::ext(r1)
        # readPNG gives 0-1; scale to 0-255 (stretching each band separately distorts the colours)
        x <- x * 255

        # Output filename includes most common hotspot time for clarity
        count_table <- table(hotspots2$datetimelocal)
        most_common <- names(count_table)[which.max(count_table)][[1]]
        most_common <- format(as.POSIXct(as.character(most_common)), format = "%Y%m%d%H%M%S")
        outtif2 <- tempfile(pattern = paste0(most_common, "_", date_seq[i], "_", wms, "_"), fileext = ".tif")
        terra::writeRaster(x, outtif2, overwrite = TRUE)

        outtif3 <- paste0(dest_folder, "\\", basename(outtif2))
        if (!outtif2 == outtif3) file.copy(outtif2, outtif3, overwrite = TRUE)

      } else {
        # If hotspots not requested, just copy the image raster to destination folder
        terra::writeRaster(r1, outtif2, overwrite = TRUE)
        file.copy(outtif2, paste0(dest_folder, "\\", basename(outtif2)), overwrite = TRUE)
      }
    }
  }
}

