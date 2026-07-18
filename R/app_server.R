#' @title The application server-side
#' @param input,output,session Internal parameters for Shiny. DO NOT REMOVE.
app_server <- function(input, output, session) {
  # Single shared SQL connection pool for the whole session ----
  con <- sql_con()

  close_con <- function() {
    if (!is.null(con) && dbIsValid(con)) {
      poolClose(con)
    }
  }

  session$onSessionEnded(close_con)

  # Safety net: onSessionEnded() only fires on a normal session/websocket
  # disconnect. If the R process itself is quit (e.g. Ctrl+C out of run_app()
  # then q()) without that event ever firing, the pool is instead swept up by
  # R's final garbage collection with a connection still checked out, which
  # is what produces the "Checked-out object deleted before being returned"
  # warning on exit. The finalizer is registered on `con` itself (not on
  # environment(), which has no other strong referents and would become
  # collectible - and thus close the pool mid-session - as soon as
  # app_server() returns): the module server closures below hold a real,
  # strong reference to `con` for as long as they're alive, so this only
  # fires once nothing is using the pool anymore or R exits.
  reg.finalizer(con, function(e) {
    if (dbIsValid(e)) poolClose(e)
  }, onexit = TRUE)

  # Modules ----
  mod_countries_server("co", con)
  mod_sectors_server("se", con)
  mod_cite_server("cite")

  # URL sync — action buttons are always excluded automatically
  syncUrl(session, exclude = c(
    "co-fmt", "se-fmt", "co-go", "se-go"
  ))
}
