#' Download copernicus data
#'
#' @description
#' Download data from Copernicus.
#' Take the ouput of fire_copernicus_search(). Requires username and password to download the data  https://dataspace.copernicus.eu/
#'
#' @noRd
#' @param fire_bbox Polygon for cropping images. Same as used for fire_copernicus_search()
#' @param download_df Data frame that is the result of fire_copernicus_search()
#' @param dest_folder Path where the geotifs will be written
#' @param user_copernicus copernicus username
#' @param password_copernicus copernicus password
#'
#' @return Writes geotifs to disk
#' #@export
#'
#' @examples
#' #fire_download_copernicus(fire_bbox = dat.bbox,download_df = dat.sen3.filt,dest_folder = outdir)
fire_copernicus_download <-  function(fire_bbox,
                                      download_df,
                                      dest_folder,
                                      user_copernicus,
                                      password_copernicus){



  #define output paths
  download_df <- download_df %>%
    dplyr::mutate(outfile=paste0(dest_folder,"\\",filename,".zip"))

  #download from odata using curl commands
  #run this first and then add refresh token into code below

  #generate token to download the data from copernicus
  mytoken <- system(paste0('curl -s -X POST https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token -H "Content-Type: application/x-www-form-urlencoded" -d "username=',
                           user_copernicus,'" -d "password=',password_copernicus,'" -d "grant_type=password" -d "client_id=cdse-public"'), intern = T)



#extract refresh token from returned string
myrefreshtoken <- purrr::map_chr(stringr::str_split(mytoken,"refresh_token"),2)
myrefreshtoken <- purrr::map_chr(stringr::str_split(myrefreshtoken,":"),2)
myrefreshtoken <- purrr::map_chr(stringr::str_split(myrefreshtoken,","),1)
myrefreshtoken <- substr(myrefreshtoken,2,nchar(myrefreshtoken)-1)


for(i in 1:nrow(download_df)){
  print(paste0("trying ",download_df$filename[i]))

  if(!file.exists(download_df$outfile[i])){

    token_reset <- system(paste0('curl -s -X POST https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token -H "Content-Type: application/x-www-form-urlencoded" -d "grant_type=refresh_token" -d "refresh_token=',
                                 myrefreshtoken,'" -d "client_id=cdse-public"'), intern = T)

    token <- purrr::map_chr(stringr::str_split(token_reset,":"),2)
    token <- purrr::map_chr(stringr::str_split(token,","),1)
    token <- substr(token,2,nchar(token)-1)

    nchar(token)
    #if reset token times out and sends a short message, generate new token
    if(nchar(token) < 1000){
      mytoken <- system(paste0('curl -s -X POST https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token -H "Content-Type: application/x-www-form-urlencoded" -d "username=',
                               user_copernicus,'" -d "password=',password_copernicus,'" -d "grant_type=password" -d "client_id=cdse-public"'), intern = T)

      #Refresh token
      myrefreshtoken <- substr(purrr::map_chr(stringr::str_split(mytoken,"refresh_token"),2),4,754)

      token_reset <- system(paste0('curl -s -X POST https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token -H "Content-Type: application/x-www-form-urlencoded" -d "grant_type=refresh_token" -d "refresh_token=',
                                   myrefreshtoken,'" -d "client_id=cdse-public"'), intern = T)

      token <- purrr::map_chr(stringr::str_split(token_reset,":"),2)
      token <- purrr::map_chr(stringr::str_split(token,","),1)
      token <- substr(token,2,nchar(token)-1)


    }


    sys_command <- paste0('curl -H "Authorization: Bearer ',token,'" "',download_df$sen3_path[i],
                          '" --location-trusted --output ',download_df$outfile[i])



    system(sys_command)
  }

}
}
