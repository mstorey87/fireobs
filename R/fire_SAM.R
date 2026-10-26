#' Map a fire polygon from an image using Segment Anything (SAM2)
#'
#' @description
#' Segments a fire area (e.g. the burnt or burning area in a linescan or satellite image) using
#' Meta's Segment Anything Model 2 (SAM2), run in Python through \pkg{reticulate}. You guide the
#' segmentation by drawing rough prompts over the image: polygons (their bounding boxes are used as
#' box prompts) and/or points placed inside the fire area. The resulting mask is converted to
#' \code{sf} polygons.
#'
#' Requirements and behaviour:
#' \itemize{
#'   \item A Python environment with \code{torch}, \code{opencv} (\code{cv2}), \code{numpy} and \code{sam2}
#'     available to \pkg{reticulate}. The SAM2 model is loaded on the first call in an R session
#'     (checkpoint downloaded if needed) and reused after that.
#'   \item The image should be a georeferenced 3-band (RGB) 8-bit GeoTIFF.
#'   \item Give at least one of \code{polygons_sf} or \code{points_sf}. If both are given, use one
#'     polygon with one or more points.
#'   \item Holes smaller than 1000 m² are filled in the output polygons.
#' }
#'
#' @param image_path Path to the GeoTIFF image to segment.
#' @param polygons_sf Optional \code{sf} polygon(s) roughly covering the fire area. The bounding box
#'   of each polygon is used as a SAM box prompt. Any CRS (transformed to the image CRS).
#' @param points_sf Optional \code{sf} point(s) placed inside the fire area, used as SAM
#'   foreground point prompts. Any CRS (transformed to the image CRS).
#' @param working_dir No longer used (the mask is no longer written to disk). Kept so existing
#'   calls still work.
#'
#' @returns An \code{sf} object of fire polygons in EPSG:4326, with columns \code{rowid} and \code{id}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' image_path <- "D:/temp/seg/linescan_example.tif"
#'
#' # Draw prompts interactively over the image
#' img <- terra::rast(image_path)
#' drawn <- mapedit::editMap(mapview::viewRGB(raster::stack(image_path)))$drawn
#'
#' polygons_sf <- drawn[st_geometry_type(drawn) %in% c("POLYGON", "MULTIPOLYGON"), ]
#' points_sf   <- drawn[st_geometry_type(drawn) == "POINT", ]
#' if (nrow(polygons_sf) == 0) polygons_sf <- NULL
#' if (nrow(points_sf) == 0) points_sf <- NULL
#'
#' # Segment the fire area
#' fire_poly <- fire_SAM(image_path, polygons_sf = polygons_sf, points_sf = points_sf)
#'
#' mapview::viewRGB(raster::stack(image_path)) + mapview::mapview(fire_poly)
#' }
fire_SAM <- function(image_path,polygons_sf=NULL,points_sf=NULL, working_dir=tempdir()) {
  # Will error if not sf or NULL
  if(!is.null(polygons_sf)) {
    checkmate::checkClass(polygons_sf, "sf")
  }
  if(!is.null(points_sf)) {
    checkmate::checkClass(points_sf, "sf")
  }


  if(!is.null(points_sf) & !is.null(polygons_sf)) {
    checkmate::assert((nrow(points_sf)>=1 & nrow(polygons_sf) == 1) , "Error: If using polygons and points, only one polygon with one or more points is allowed (not multiple points with multiple polygons)")
  }

  fire_load_sam_once()

  # Load image in Python and set it for SAM
  #reticulate::py$image_path <- image_path
  message(glue::glue("reading {image_path} (cv)"))
  reticulate::py_set_attr(reticulate::py,"image_path",image_path)
  reticulate::py_run_string("
image = cv2.imread(image_path)
print('cv2 read')
image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
print('cv2 color set')
predictor.set_image(image)
print('predictor image set')
")


  message(glue::glue("reading {image_path} (terra)"))


  #load rast for plotting in R
  # Load input image as raster and PNG for display
  img_rast <- terra::rast(image_path)



  message("transforming points or polygons")
  # Transform and extract one bbox per polygon
  if(!is.null(polygons_sf)){
    bbox_list <- polygons_sf %>%
      sf::st_transform(sf::st_crs(img_rast)) %>%
      split(seq_len(nrow(.))) %>%
      purrr::map(sf::st_bbox)
  }

  ##get point
  if(!is.null(points_sf)){
    point_list <- points_sf %>%
      sf::st_transform(sf::st_crs(img_rast)) %>%
      split(seq_len(nrow(.))) %>%
      purrr::map(sf::st_coordinates)
  }





  message(glue::glue("reading {image_path} (magick)"))
  #get info for conversion to pixel coordinates
  img <- magick::image_read(image_path)
  height <- magick::image_info(img)$height
  ext <- terra::ext(img_rast)
  res <- terra::res(img_rast)


  message("processing polygons (if drawn)")
  if(!is.null(polygons_sf)){
    # Convert each bbox to pixel coordinates and input_box format
    input_boxes <- purrr::map(bbox_list, function(bbox) {
      col_start <- floor((bbox["xmin"] - ext[1]) / res[1]) + 1
      col_end   <- ceiling((bbox["xmax"] - ext[1]) / res[1])
      row_start <- floor((ext[4] - bbox["ymax"]) / res[2]) + 1
      row_end   <- ceiling((ext[4] - bbox["ymin"]) / res[2])

      x_vals <- c(col_start, col_end)
      y_vals <- c(row_end, row_start)  # note: y flip

      input_box <- as.integer(c(
        min(x_vals),
        height - max(y_vals),  # flip Y for SAM
        max(x_vals),
        height - min(y_vals)
      ))


      #
      input_box_flipped <- input_box
      input_box_flipped[2] = height - input_box[4]
      input_box_flipped[4] = height - input_box[2]

      return(input_box_flipped)
    })


  }else{
    input_boxes <- list()
  }


  message("processing points (if drawn)")
  if(!is.null(points_sf)){








    point_list <- purrr::map(point_list, function(pt) {
      pt <- unlist(pt)
      col <- floor((pt[1] - ext[1]) / res[1]) + 1
      row <- floor((ext[4] - pt[2]) / res[2]) + 1  # flip Y

      x_pixel <- as.integer(col)
      y_pixel <- as.integer(height - row)  # SAM expects origin at top-left
      xy <- c(x_pixel, y_pixel)


      xy[2] = height - xy[2]

      return(xy)
    })
  }else{
    point_list=list()
  }


  #send to python
  #reticulate::py$point_list <-  do.call(rbind,point_list)
  #reticulate::py$input_label <- rep(1L, length(point_list))
  #reticulate::py$input_boxes <- do.call(rbind,input_boxes)

  message("seinding to python")
  reticulate::py_set_attr(reticulate::py,"point_list", do.call(rbind,point_list))
  reticulate::py_set_attr(reticulate::py,"input_label",rep(1L, length(point_list)))
  reticulate::py_set_attr(reticulate::py,"input_boxes",do.call(rbind,input_boxes))


  message("running sam")
  # Step 5: Run SAM and build mask
  reticulate::py_run_string("
height = image.shape[0]

#check if points have been drawn and how many

#redundant, but making sure point list and input label are non
if point_list is None or len(point_list) == 0:
    point_list = None
    input_label = None
elif len(point_list) == 1:
    input_label = np.array([input_label])
else:
    input_label = input_label



#check if boxes drawn
if input_boxes is None or len(input_boxes) == 0:
    input_boxes = None

#this flipping should be done in R first
#input_points_flipped = point_list.copy()
#input_points_flipped[:, 1] = height - input_points_flipped[:, 1]

masks, scores, logits = predictor.predict(
    point_coords=point_list,
    point_labels=input_label,
    box=input_boxes,
    multimask_output=True
)


if input_boxes is not None and len(input_boxes) > 0:

    if len(input_boxes) == 1:
        best_mask = masks[np.argmax(scores)]
        #output_rgba = np.zeros((best_mask.shape[0], best_mask.shape[1], 4), dtype=np.uint8)
        # output_rgba[best_mask > 0.5] = [254, 254, 254, 255]
        # output_rgba = np.flipud(output_rgba)
        # cv2.imwrite(outfile, output_rgba)
    else:

        best_mask = np.zeros_like(masks[0, 0], dtype=np.uint8)

        for i in range(len(input_boxes)):
            best_idx = np.argmax(scores[i])
            best_mask[masks[i, best_idx] > 0.5] = 255
elif input_boxes is None and point_list is not None:
    best_mask = masks[np.argmax(scores)]

best_mask = (best_mask > 0).astype(np.uint8)
")


message("load and vectorize mask")
# Step 6: Load and vectorize mask
# The mask comes straight from Python as a matrix (row 1 = top of the image), rather than via a
# non-georeferenced TIFF, because terra versions differ in whether they read those upside down.
mask_mat <- reticulate::py$best_mask
mask1 <- terra::rast(mask_mat, extent = terra::ext(img_rast), crs = terra::crs(img_rast))

pol1 <- terra::as.polygons(mask1) %>%
  sf::st_as_sf() %>%
  dplyr::rename(val = 1) %>%
  dplyr::filter(val > 0) %>%
  dplyr::select(-val) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_cast("POLYGON") %>%
  dplyr::mutate(rowid = as.character(dplyr::row_number()))

#fill any small holes
message("filling small holes")
area_thresh <- units::set_units(1000, m^2)
pol1 <- smoothr::fill_holes(pol1, threshold = area_thresh)

message("final processing")
pol2 <- sf::st_transform(pol1, 4326) %>%
  sf::st_simplify(dTolerance = 0.1) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(id=dplyr::row_number())



return(pol2)

}



# # Apply segmentation function to all images in folder
# files <- list.files("D:\\temp\\seg", full.names = TRUE, pattern = "\\.tif$")
# image_path <- files[[1]]
# im <- raster::stack(image_path) %>% raster::aggregate(5)
#
# m <- mapview::viewRGB(im)
# drawn <- mapedit::editMap(m)
# polygons_sf <- drawn$drawn %>%
#   dplyr::filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
#
# points_sf <- drawn$drawn %>%
#   dplyr::filter(st_geometry_type(geometry) %in% c("POINT"))
#
# if(nrow(polygons_sf)==0) polygons_sf <- NULL
# if(nrow(points_sf)==0) points_sf <- NULL
#
#
# p <- fn_run(image_path,polygons_sf,points_sf)
# mapview::mapview(p)+mapview::viewRGB(im)



