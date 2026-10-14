#' Sample multiple BARRA variables for an sf object across times
#'
#' @description
#' This function takes an sf object with multiple rows (points, lines, or polygons) and samples multiple BARRA variables.
#' It loops over unique nc files and variables to minimize data downloads by only opening each NetCDF once (all netcdfs are stored in monthly files).
#' Large lists of variables or long time series may take a long time and can encounter network issues;
#' consider splitting your queries into smaller batches.
#'
#' @param dat An sf object containing spatial features (points, lines, or polygons) with a datetime column.
#' @param time_col_utc Name of the datetime column in `dat` (must be POSIXct in UTC).
#' @param barraid Character string specifying BARRA product: "R2" (12 km resolution) or "C2" (~4 km resolution).
#' @param varnames Character vector of BARRA variable names to sample. Examples include "sfcWind" (surface wind), "tas" (temperature),
#'        "hurs" (relative humidity), "vas" and "uas" (wind components). See
#'        http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf for full variable list.
#' @param timestep Time resolution to sample: "hourly", "daily", or "monthly".
#' @param extract_fun Summary function to apply when sampling polygon/line features (e.g., "mean", "sum", "min", "max").
#'
#' @return An sf object with the original data and additional columns for each sampled variable at each datetime.
#' @export
#'
#' @examples
#' \dontrun{
# library(sf)
# library(lubridate)
#
# # Example: create sample points near Canberra in 2019
# pts <- st_sfc(
#   st_point(c(149.1, -35.3)),
#   st_point(c(149.2, -35.25)),
#   crs = 4326
# )
#
# # Create sf object with datetimes
# sf_pts <- st_sf(
#   id = 1:2,
#   datetime_utc = as.POSIXct(c("2019-01-01 12:00:00", "2019-01-02 12:00:00"), tz = "UTC"),
#   geometry = pts
# )
#
# # Sample sfcWind and tas hourly from BARRA C2 product
# result <- fire_barra_sample_all(
#   dat = sf_pts,
#   time_col_utc = "datetime_utc",
#   barraid = "C2",
#   varnames = c("sfcWind", "tas"),
#   timestep = "hourly",
#   extract_fun = "mean"
# )
#' }
fire_barra_sample_all <- function(dat,time_col_utc,barraid="C2",varnames,timestep="hourly",extract_fun="mean"){



  ### helper function to retry in case of cut off connection message to BARRA2 thredds
  retry_call <- function(f, max_tries = 10, wait_seconds = 30, label = "") {
    for (k in seq_len(max_tries)) {
      out <- tryCatch(f(),
                      error   = function(e) e,
                      warning = function(w) w)   # treat warnings as failures too
      if (!inherits(out, "condition")) return(out)
      message(sprintf("Attempt %d/%d failed %s: %s",
                      k, max_tries, label, conditionMessage(out)))
      if (k < max_tries) Sys.sleep(wait_seconds)
    }
    NULL
  }


  #add time column with standard name
  dat$time <- dat[[time_col_utc]]


  #ensure time column is posxct
  checkmate::assert_posixct(dat$time)
  if (toupper(lubridate::tz(dat$time)) != "UTC") stop("datetimeutc must be in UTC")
  checkmate::assert_choice(timestep, c("hourly", "daily", "monthly"))
  checkmate::assert_string(extract_fun)

  #get the user to round the date times first
  checkmate::assert( all(lubridate::minute(dat$time)==0) & all(lubridate::second(dat$time)==0),"Error: Please round times to hourly (use lubridate::round_date)")

  # format data -------------------------------------------------------------



  #give unique rowid
  dat$input_rowid <- seq_len(nrow(dat))

  #add local time posix, convert to utc, round utc time (to match barra times)
  #year and month needed to create barra paths
  #transform to 4326 to match BARRA crs
  dat <- dat %>%
    dplyr::mutate(time_round=time,
                  yrmnth=format(time_round,format = "%Y%m")) %>%
    sf::st_transform(4326)




  # Check latest dates currently available in BARRA -------------------------





  #check currently available barra data by finding latest available data for sfcWind
  #make path to check matches input timestep - e.g. daily or hourly or monthly
  checkmate::assert(timestep %in% c("hourly","daily","monthly"),"Error: timestep must be hourly, daily or monthly")
  base_urls <- list(
    daily = "https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUST-04/BOM/ERA5/historical/hres/BARRA-C2/v1/day/sfcWind/latest/catalog.html",
    hourly = "https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUST-04/BOM/ERA5/historical/hres/BARRA-C2/v1/1hr/sfcWind/latest/catalog.html",
    monthly = "https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUST-04/BOM/ERA5/historical/hres/BARRA-C2/v1/mon/sfcWind/latest/catalog.html"
  )

  catalog_path <- base_urls[[timestep]]
  response <- httr::GET(catalog_path)
  content_xml <- httr::content(response, as = "text")

  # Collapse to one string (if it's a vector of lines)
  html_text <- paste(content_xml, collapse = "\n")

  # Use regular expression to find .nc filenames
  nc_files <- regmatches(html_text, gregexpr("[^\">/]+\\.nc", html_text))[[1]]
  nc_files <- nc_files[stringr::str_detect(nc_files,"BARRA")]

  #get year Month of each file
  nc_yearmonth <- as.numeric(substr(nc_files,nchar(nc_files)-8,nchar(nc_files)-3))
  min.nc <- as.character(min(nc_yearmonth))
  max.nc <- as.character(max(nc_yearmonth))

  #return message for max available year and month currently
  mes.1 <- paste0("BARRA currently available ",month.name[as.numeric(substr(min.nc,5,6))]," ",substr(min.nc,1,4)," to ",month.name[as.numeric(substr(max.nc,5,6))]," ",substr(max.nc,1,4))
  message(mes.1)
  message("Outside dates will return NA")




  # Split input data by year month ------------------------------------------





  #create a list of sf objects, split by year and month, because nc files are monthly. This is so each monthly file will only be loaded once.
  dat.split <- split(dat,dat$yrmnth)






  # Loop through year-month and BARRA variable combinations -----------------



  res.all.vars <- list()
  message.vars <- list()
  for(v in varnames){


    res.list <- list()

    for (i in 1:length(dat.split)) {


      #current iteration year and month
      yr.mnth.dat <- unique(names(dat.split)[i])

      #give message about year-month and BARRA var
      mes.2 <- paste0(month.name[as.numeric(substr(yr.mnth.dat,5,6))]," ",substr(yr.mnth.dat,1,4))
      message(paste0("sampling data row ",i ," for ",v," ",mes.2))


      #data for current iteration (all associated with same nc)
      #calculate nc path
      dat.i <- dat.split[[i]] %>%
        dplyr::mutate(ncpath=fire_barra_path(datetimeutc=time_round,barraid=barraid,timestep = timestep,varname=v)) %>%
        dplyr::select(input_rowid,time_round,ncpath)

      #check is sample data is within available barra range
      if(as.numeric(yr.mnth.dat) >= as.numeric(min.nc) & as.numeric(yr.mnth.dat) <= as.numeric(max.nc)){

        #connect to nc. will be same for all rows in same month
        #retry if something goes wrong
        nc_conn <- fire_tidync_safe(unique(dat.i$ncpath),max_tries = 20,wait_seconds = 30)

        #if nothing returned, skip all rest of datetimes for this variable and go to next variable
        #return NA for variable
        if(is.null(nc_conn)){
          #return NULL for variable
          res.list <- NULL
          message.vars[[v]] <- v
          break
        }

        #create a list of sf object associated with current nc, each with unique times
        dat.split.time <- split(dat.i,dat.i$time_round)

        #for each sf object of the same datetime, run the barra nc sampling function for current BARRA var (v)
        # res <- purrr::map(dat.split.time,~fire_barra_sample(nc_conn,unique(.x$time_round),.x,v,timestep = timestep,extract_fun=extract_fun))

        res <- purrr::map(dat.split.time, function(x) {
          out <- retry_call(
            function() fire_barra_sample(nc_conn, unique(x$time_round), x, v,
                                                        timestep = timestep,
                                                        extract_fun = extract_fun),
            max_tries = 10, wait_seconds = 30,
            label = paste(v, unique(x$time_round))
          )
          # if every attempt failed, return NA for this time instead of crashing the whole run
          if (is.null(out)) {
            out <- x
            out[[v]] <- NA_real_
          }
          out
        })

        res.list[[i]] <-  do.call(rbind,res) %>%
          sf::st_drop_geometry() %>%
          dplyr::select(-ncpath)
      }else{
        print(paste0("BARRA currently unavailable for ",mes.2))
        res.list[[i]] <- NA
      }
    }




    #add NA to list if there was an nc connection error. else return sample results.
    if (is.null(res.list)) {
      res.all.vars[[v]] <- NULL
    } else {
      res.all.vars[[v]] <- do.call(rbind, res.list)
    }

  }

  #remove any NULL caused by NC connection errors
  res.all.vars <- res.all.vars[!sapply(res.all.vars, is.null)]

  #join all remaining
  res.all <- purrr::reduce(res.all.vars, dplyr::left_join, by = c('input_rowid','time_round')) %>%
    dplyr::left_join(dat %>% dplyr::select(input_rowid),by="input_rowid") %>%
    sf::st_as_sf() %>%
    dplyr::arrange(input_rowid)


  if(length(message.vars)==0){
    message("All variables successfully sampled")
    mes.1 <- paste0("NA returned for any times after ",month.name[as.numeric(substr(max.nc,5,6))]," ",substr(max.nc,1,4)," because BARRA not available yet")
    message(mes.1)

  }else{
    message(paste0("Sampling of ", paste0(message.vars,collapse = " "), "failed. Others variables sampled successfully."))
    mes.1 <- paste0("NA returned for any times after ",month.name[as.numeric(substr(max.nc,5,6))]," ",substr(max.nc,1,4)," because BARRA not available yet")
    message(mes.1)


  }


  return(res.all)

}
