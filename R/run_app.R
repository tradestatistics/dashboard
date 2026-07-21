#' @title Run the Application
#' @param ... Arguments passed on to [tabler::tablerApp()], e.g. `host`,
#'   `port`, `launch.browser`. Used by tabler-server to bind the
#'   loopback port it assigns instead of the app's own default port.
#' @export
run_app <- function(...) {
  # Read credentials from file excluded in .gitignore
  readRenviron("/tradestatistics/credentials.txt")

  tablerApp(
    ui = app_ui(),
    server = app_server,
    ...
  )
}
