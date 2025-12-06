#' Launch Shiny App
#'
#' @description Launch interactive numerical methods explorer
#'
#' @export
run_shiny_app <- function() {
  app_dir <- system.file("shiny", package = "NumericalMethods")
  if (app_dir == "") {
    stop("Could not find Shiny app directory")
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}