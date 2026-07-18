#' @title Run the Application
#' @export
run_app <- function() {
  tablerApp(
    ui = app_ui(),
    server = app_server
  )
}
