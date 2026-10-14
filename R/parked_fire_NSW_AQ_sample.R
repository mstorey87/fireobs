# library(dplyr)
# library(httr)
# library(jsonlite)
#
#
# #get site details
# path_site_details <- 'https://data.airquality.nsw.gov.au/api/Data/get_SiteDetails'
#
# df_site_details <- path_site_details %>%
#   GET(body=list(),
#       add_headers(`Content-Type`='application/json')) %>%
#   content(as="text",encoding = "UTF-8") %>%
#   fromJSON(flatten=TRUE) %>%
#   data.frame()
#
# sf_sites <- df_site_details %>%
#   filter(!is.na(Latitude)) %>%
#   st_as_sf(coords=c("Longitude","Latitude"),crs=4236)
#
#
# #get parameter details
# path_param_details <- 'https://data.airquality.nsw.gov.au/api/Data/get_ParameterDetails'
#
# df_param_details <- path_param_details %>%
#   GET(body=list(),
#       add_headers(`Content-Type`='application/json')) %>%
#   content(as="text",encoding = "UTF-8") %>%
#   fromJSON(flatten=TRUE) %>%
#   data.frame()
#
#
# ## search the data
# # dataframe to save observations
# df_hist_obs_all <- data.frame()
#
# # set start and end date
# start <-  '2020-05-01'
# end   <- '2020-05-02'
# sites <- c(206,300)
# params <- c("PM2.5","PM10","CO")
#
# theDate <- start
#
# # Historical Observations
# path_obs <- 'https://data.airquality.nsw.gov.au/api/Data/get_Observations'
#
# # create json call
# pc_json  <- paste0('{ "Parameters": [', paste0('"',params,'"',collapse = ","),'],
# "Sites": [', paste0(sites,collapse = ",") ,'],
# "StartDate": "', start, '",
#                  "EndDate": "',end, '",
#                  "Categories": ["Averages"],
#                  "SubCategories": [  ],
#                  "Frequency": ["Hourly average"]}')
#
# df_hist_obs <- path_obs %>%
#   POST(body = pc_json,
#        add_headers(c(
#          `accept` = 'application/json',
#          `Content-Type` = 'application/json'
#        ))) %>%
#   content(as = "text", encoding = "UTF-8") %>%
#   fromJSON(flatten = TRUE) %>%
#   as.data.table()
#
# # change column names
# names(df_hist_obs) <- c("Site_Id"
#                         ,"Date"
#                         ,"Hour"
#                         ,"HourDescription"
#                         ,"Value"
#                         ,"Parameter"
#                         ,"ParameterDescription"
#                         ,"Units"
#                         ,"UnitsDescription"
#                         ,"Category"
#                         ,"SubCategory"
#                         ,"Frequency")
#
# # trim parameter description
# df_hist_obs$ParameterDescription <- trimws(df_hist_obs$ParameterDescription)
#
