#' List files from a THREDDS catalog URL filtered by a regex pattern
#'
#' @description
#' This internal utility function fetches the content of a THREDDS catalog HTML page for Himawari data from BOM,
#' extracts all file names listed on that page, and returns only those that match a
#' specified regular expression pattern. It is useful for programmatically listing netcdf files at a path for a specific time
#' e.g.`https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/2025/01/01/0000/catalog.html`
#'
#' If the catalog page cannot be read (e.g. it does not exist, or there is a network error), `NA` is returned
#' instead of an error, so the function can be used safely over many paths.
#'
#' @param catalog_url Character. The URL to the THREDDS catalog HTML page for a specific time (e.g., `https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/2025/01/01/0000/catalog.html`).
#' @param regex Character. A regular expression pattern used to filter the filenames returned. (e.g. `ABOM_OBS_` returns observations for each band)
#'
#' @return Character vector of matching file names from the catalog page. `character(0)` if the page
#'   was read but nothing matched `regex`, or `NA` if the page could not be read.
#' @export
#'
#' @examples
#' \dontrun{
#' fire_thredds_list(catalog_url="https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/2025/01/01/0000/catalog.html",
#'                   regex="ABOM_OBS_")
#' }
fire_thredds_list <- function(catalog_url, regex) {

  checkmate::assert_string(catalog_url)
  checkmate::assert_string(regex)

  tryCatch(
    {
      # Fetch with httr rather than rvest::read_html(url): read_html() leaves an R connection
      # behind for every page that does not exist (404), and R only allows 128 connections,
      # so long searches over missing time-steps eventually fail with
      # "all 128 connections are in use"
      resp <- httr::GET(catalog_url, httr::timeout(60))
      if (httr::status_code(resp) != 200) return(NA_character_)

      page_html <- httr::content(resp, as = "text", encoding = "UTF-8")
      page_text <- rvest::html_text(xml2::read_html(page_html))

      # Split the page text into lines and keep those that match the file pattern
      page_lines <- stringr::str_split(page_text, pattern = "[\r\n]+")[[1]]
      stringr::str_trim(stringr::str_subset(page_lines, pattern = regex))
    },
    error = function(e) {
      # e.g. page does not exist (404) or network error
      NA_character_
    }
  )
}
