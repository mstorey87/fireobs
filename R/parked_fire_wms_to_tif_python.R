#'Python based wms to tif
#'
#' @param layers gibs layer name
#' @param time date in YYYY-mm-dd/YYYY-mm-dd
#' @param bbox bbox coords
#' @param size output pixel sizes c(length,width)
#' @param png_file output png path
#' @param geotiff_file output tif path
#'
#' @return write tif
#' @export
#'
#' @examples
#' #
fire_wms_to_tif_python <- function(layers,time,bbox,size,png_file,geotiff_file){

  #use_python('C:/Users/mstorey/AppData/Local/anaconda3/envs/spyder-env/python.exe')
  reticulate::use_virtualenv('python3_env', required = T)
  # Source Python script
  reticulate::py_run_string("
from owslib.wms import WebMapService
import rasterio
from rasterio.transform import from_origin
from PIL import Image
import numpy as np

def fetch_and_save_geotiff(layers, time,  bbox, size, png_file, geotiff_file):
    # Connect to GIBS WMS Service
    wms = WebMapService('https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi?', version='1.1.1')

    # Configure request for the specified layers and time
    img = wms.getmap(layers=[layers],  # Layers (Bands for true color)
                     srs='epsg:4326',  # Map projection
                     bbox=bbox,  # Bounding box for the region of interest
                     size=size,  # Image size (width, height)
                     time=time,  # Time range (date of data)
                     format='image/png',  # Image format
                     transparent=True)  # Transparency for nodata

    # Save output PNG to a file
    with open(png_file, 'wb') as out:
        out.write(img.read())

    # Open the PNG image
    img = Image.open(png_file)

    # Get image dimensions
    width, height = img.size

    # Create the affine transform from the bounding box
    transform = from_origin(bbox[0], bbox[3], (bbox[2] - bbox[0]) / width, (bbox[3] - bbox[1]) / height)

    # Convert the image to a numpy array
    img_array = np.array(img)

    # Create a GeoTIFF with rasterio
    with rasterio.open(geotiff_file, 'w', driver='GTiff',
                       count=3, dtype='uint8',  # For RGB (3 bands)
                       width=width, height=height,
                       crs='EPSG:4326', transform=transform) as dst:
        dst.write(img_array[:, :, 0], 1)  # Red channel
        dst.write(img_array[:, :, 1], 2)  # Green channel
        dst.write(img_array[:, :, 2], 3)  # Blue channel
")



  # Call the Python function via reticulate
  reticulate::py$fetch_and_save_geotiff(layers, time, bbox, size, png_file, geotiff_file)










}





