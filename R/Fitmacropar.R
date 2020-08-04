#' @title FitMacropar
#'
#' @description Launch a shiny app
#'
#' @export
Fitmacropar <- function(data_df, save_dir) {
  # Find directory of the package
  appDir <- system.file("myapp", package = "FitMacropar")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `FitMacropar`.", call. = FALSE)
  }

  # Server's arguments
  server_env <- environment(paste(appDir, "/server.R"))
  server_env$data.original <- data_df
  server_env$save_dir <- save_dir

  # Run app
  app <- shiny::runApp(appDir, display.mode = "normal")
  shiny::runApp(app)
}
