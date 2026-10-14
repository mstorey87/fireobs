#' Print a list of useful NASA GIBS WMS products
#'
#' @description
#' This helper function returns a character vector of recommended NASA GIBS (Global Imagery Browse Services)
#' product layer names for fire and smoke monitoring. These include MODIS and VIIRS bands for active fire detection,
#' true colour imagery for smoke, and brightness temperature layers for night-time spread monitoring.
#'
#' A short description with a link to more products is included at the end.
#'
#' @return A character vector of GIBS product names and a helpful note.
#' @keywords internal
#'
#' @examples
#' # Get recommended GIBS product names
#' fire_GIBS_products()
#'
#' # Use with a mapping tool or WMS request
#' products <- fire_GIBS_products()
#' print(products)
fire_GIBS_products <- function() {
  txt <- "MODIS Bands 721 and VIIRS bands M11-I2-I1 are the best for active fire visualisation.
  True colour can be used for smoke visualisation.
  Brightness temperature may be useful for night spread.
  More layers at https://wiki.earthdata.nasa.gov/display/GIBS/GIBS+Available+Imagery+Products#"

  product_names <- c(
    'MODIS_Aqua_CorrectedReflectance_Bands721',
    'MODIS_Terra_CorrectedReflectance_Bands721',
    'VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1',
    'VIIRS_NOAA20_CorrectedReflectance_BandsM11-I2-I1',
    'MODIS_Aqua_Brightness_Temp_Band31_Night',
    'MODIS_Terra_Brightness_Temp_Band31_Night',
    'VIIRS_NOAA21_Brightness_Temp_BandI5_Night',
    'VIIRS_NOAA20_Brightness_Temp_BandI5_Night',
    'VIIRS_SNPP_Brightness_Temp_BandI5_Night',
    'VIIRS_NOAA20_CorrectedReflectance_TrueColor',
    'VIIRS_NOAA21_CorrectedReflectance_TrueColor',
    'VIIRS_SNPP_CorrectedReflectance_TrueColor',
    'MODIS_Aqua_CorrectedReflectance_TrueColor',
    'MODIS_Terra_CorrectedReflectance_TrueColor',
    txt
  )

  return(product_names)
}
