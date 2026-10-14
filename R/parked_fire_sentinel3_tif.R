#' Convert a list of Sentinel-3 SEN3.zips to geotiffs
#'
#' @param fire_bbox Bounding box polygon which the output tifs will be cropped to.
#' @param sen3_folder path to folder where "SEN3.zips" are stored. All SEN3.zip in folder will be converted.
#'
#' @return Geotiffs written to disk
#' @export
#'
#' @examples
#' #fire_sentinel3_tif(fire_bbox = dat.fire.polygon,sen3_folder = outdir)
fire_sentinel3_tif <- function(fire_bbox,sen3_folder){


  #list all sentinel 3 zips
  my.zips<- list.files("C:\\temp\\tifs2",full.names = T,pattern = "SEN3.zip")

  #transform fire bbox to same crs as sentinel 3 data
  fire_bbox <- fire_bbox %>%
    sf::st_transform(4326)



  #get the time zone based on footprint centroid and add a local time field
  my_tz <- fire_get_timezone(fire_bbox)


  #loop through each zip, exctract and convert data
  for(i in 1:length(my.zips)){

    sen.zip<- my.zips[[i]]

    #unzip the folder
    temp.fold <- paste0(sen3_folder,"\\tempunzip")


    #extract radiances for selected bands to create false colour images
    myfiles <- c("/geodetic_an.nc","/S5_radiance_an.nc","/S6_radiance_an.nc","/S3_radiance_an.nc")
    myfiles <-paste0(stringr::str_replace(basename(sen.zip),".zip",""),myfiles)

    #extract date time from file name
    sen.fold <- paste0(temp.fold,"/",stringr::str_replace(basename(sen.zip),".zip",""))
    rastime <- lubridate::with_tz(as.POSIXct(substr(basename(sen.fold),17,31),format="%Y%m%dT%H%M%S",tz="UTC"),tz=my_tz)
    hour_of_day <- lubridate::hour(rastime)
    rastime_chr <- format(rastime,format="%Y%m%d%H%M%S")



    #create an output time name
    outname <- paste0(sen3_folder,"\\",rastime_chr,"_sentinel3.tif")


    #skip if file already exists
    #this is because there might be a repeated time between two zip folder names
    if(file.exists(outname)){
      print(paste0("skipping ",outname,", already exists"))


    }else{





      z <- tryCatch(utils::unzip(sen.zip,files=myfiles,exdir = temp.fold))

      if(is.null(z)){
        file.remove(sen.zip)

      }else{

        #extract pixel coordinates
        coords <- stars::read_ncdf(paste0(sen.fold,"\\geodetic_an.nc"), var = c("latitude_an", "longitude_an"))
        lats <- coords$latitude_an
        lons <- coords$longitude_an


        #extract radiances and apply coordinates. Crop to fire bbox
        #extract S6 thermal band first, because this can be used is day time 3 bands images or night time single band images
        S6 <- stars::read_ncdf( paste0(sen.fold,"\\S6_radiance_an.nc"), var = "S6_radiance_an")
        S6 <- S6$S6_radiance_an
        S6 <- stars::st_as_stars(S6)
        S6 <- stars::st_as_stars(S6, curvilinear = list(X1 = lons, X2 = lats))
        S6_warp = stars::st_warp(S6, crs = "EPSG:4326",threshold=0.1)

        #create rgb images the data is for day time

        if(hour_of_day > 7 & hour_of_day < 19){

          #read two other bands
          S5 <- stars::read_ncdf( paste0(sen.fold,"\\S5_radiance_an.nc"), var = "S5_radiance_an")
          S5 <- S5$S5_radiance_an
          S5 <- stars::st_as_stars(S5)
          S5 <- stars::st_as_stars(S5, curvilinear = list(X1 = lons, X2 = lats))

          S3 <- stars::read_ncdf( paste0(sen.fold,"\\S3_radiance_an.nc"), var = "S3_radiance_an")
          S3 <- S3$S3_radiance_an
          S3 <- stars::st_as_stars(S3)
          S3 <- stars::st_as_stars(S3, curvilinear = list(X1 = lons, X2 = lats))

          S5_warp = stars::st_warp(S5, crs = "EPSG:4326",threshold=0.1)
          S3_warp = stars::st_warp(S3, crs = "EPSG:4326",threshold=0.1)#, cellsize = 0.01, threshold = 0.02)
          s3_rgb <- c(S6_warp,S5_warp,S3_warp)
          s3_rgb <- c(S6_warp,S5_warp,S3_warp)
          s3_rgb <- stars::as(s3_rgb,"SpatRaster")
          s3_rgb <- terra::crop(s3_rgb,fire_bbox)
          out_ras <- s3_rgb
        }else{

          S6_warp <- stars::as(S6_warp,"SpatRaster")
          S6_warp <- terra::crop(S6_warp,fire_bbox)
          out_ras <- S6_warp


        }

        #stretch and write raster to disk
        out_ras <- terra::stretch(out_ras)
        print(paste0("writing ",outname))
        terra::writeRaster(out_ras,outname)


        #remove temporary zip folder
        unlink(temp.fold,recursive = T)
      }





    }




  }

}
