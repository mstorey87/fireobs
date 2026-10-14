#' Read a pre-made fire scan image from database
#'
#' @param lineid ROS line id
#' @param dbpassword password for database
#'
#' @returns grob
#' @keywords internal
#'
#' @examples
#' #
fire_read_plot <- function(lineid,dbpassword){


  #don't reconnect if already connected
  if(!exists("DB")){

    DB <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = "cermb_fires",
      user = "mstorey",
      password = dbpassword,
      host = "charus.ad.uow.edu.au",
      port = 5432
    )

    #on.exit(DBI::dbDisconnect(DB), add = TRUE)
  }


  # --- 1. Read from database ---
  res <- DBI::dbGetQuery(DB,
                    "SELECT image FROM fire_line_images WHERE lineid = $1",
                    params = list(lineid)
  )

  # --- 2. Extract raw image data ---
  img_raw <- res$image[[1]]

  writeBin(img_raw, "temp.png")
  img_grob <- grid::rasterGrob(png::readPNG("temp.png"))
  return(img_grob)


}
