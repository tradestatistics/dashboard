#' @title The application server-side
#' @param input,output,session Internal parameters for Shiny. DO NOT REMOVE.
app_server <- function(input, output, session) {
  # Modules ----
  mod_countries_server("co")
  mod_sectors_server("se")
  mod_cite_server("cite")

  # URL sync — action buttons are always excluded automatically
  syncUrl(session, exclude = c(
    "co-fmt", "se-fmt", "co-go", "se-go"
  ))
}
