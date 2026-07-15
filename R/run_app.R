#' @title Run the Application
#' @param ... Currently unused (kept for backwards-compatible signature).
#' @export
run_app <- function(...) {
  tablerApp(
    ui = app_ui(),
    server = app_server
  )
}
