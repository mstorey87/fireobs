#' Safely load a remote NetCDF file with retries
#'
#' @description
#' Utility function to open a remote NetCDF dataset (typically from a THREDDS Data Server)
#' using the `tidync` package.
#' If the connection fails, it will automatically retry multiple times with a pause between attempts.
#' This is helpful for large or busy THREDDS servers that can occasionally drop connections.
#'
#' @param url Character string. The URL of the remote NetCDF file, typically ending in `.nc` and served via THREDDS.
#' @param max_tries Integer. The maximum number of attempts to try loading the file. Defaults to 5.
#' @param wait_seconds Numeric. How many seconds to wait between retry attempts. Defaults to 10.
#'
#' @returns A `tidync` object if the connection is successful, otherwise `NULL` if all attempts fail.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: Open a remote NetCDF file from NCI's THREDDS server with retry
#' nc_url <- "https://thredds.nci.org.au/thredds/dodsC/zv2/agcd/v2-0-3/precip/total/r001/01month/agcd_v2_precip_total_r001_monthly_2023.nc"
#' nc_data <- fire_tidync_safe(nc_url, max_tries = 3, wait_seconds = 5)
#'
#' if (!is.null(nc_data)) {
#'   print(nc_data)
#' } else {
#'   message("Could not open NetCDF after retries.")
#' }
#' }
fire_tidync_safe <- function(url, max_tries = 5, wait_seconds = 10) {
  for (i in seq_len(max_tries)) {
    result <- tryCatch(
      {
        tidync::tidync(url)
      },
      error = function(e) {
        message(sprintf("Attempt %d failed: %s", i, e$message))
        return(NULL)
      }
    )

    if (!is.null(result)) {
      message("Success on attempt ", i)
      return(result)
    } else if (i < max_tries) {
      message("Retrying in ", wait_seconds, " seconds...")
      Sys.sleep(wait_seconds)
    } else {
      message("Failed after ", max_tries, " attempts. Returning NULL.")
      return(NULL)
    }
  }
}
