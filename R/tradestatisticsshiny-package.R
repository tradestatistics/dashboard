#' @keywords internal
#' @import shiny
#' @import tabler
#' @import d3po
#' @importFrom cachem cache_disk
#' @importFrom shiny shinyOptions
#' @importFrom data.table `:=` .N .I .SD copy data.table fifelse frankv rbindlist setDT setnames setorder
#' @importFrom glue glue
#' @importFrom golem add_resource_path activate_js favicon bundle_resources with_golem_options
#' @importFrom htmlwidgets JS
#' @importFrom jsonlite toJSON
#' @importFrom pool dbPool dbIsValid poolClose
#' @importFrom rio export
#' @importFrom RPostgres Postgres
#' @importFrom shiny NS tagList HTML fluidRow selectInput sliderInput actionButton htmlOutput uiOutput h2 tags div moduleServer reactive eventReactive observe observeEvent renderText renderUI updateSelectizeInput downloadHandler req shinyApp
#' @importFrom shinyjs hide hidden show useShinyjs
#' @importFrom stats setNames
#' @importFrom tidyr pivot_longer
"_PACKAGE"

shinyOptions(
  cache = cache_disk(
    dir = "/tradestatistics/cache"
    # logfile = "/tradestatistics/log/cache.log"
  )
)

#' reporters_display
#'
#' Internal dataset for country codes.
#'
#' @docType data
#' @keywords datasets
#' @name reporters_display
NULL

#' sections_display
#'
#' Internal dataset for section/commodity codes.
#'
#' @docType data
#' @keywords datasets
#' @name sections_display
NULL

#' commodities
#'
#' Internal dataset for commodity codes (6,898 codes).
#'
#' @docType data
#' @keywords datasets
#' @name commodities
NULL

#' commodities_display
#'
#' Internal dataset for commodity codes (6,898 codes).
#'
#' @docType data
#' @keywords datasets
#' @name commodities_display
NULL

#' commodities_short_display
#'
#' Internal dataset for commodity codes (1,363 codes).
#'
#' @docType data
#' @keywords datasets
#' @name commodities_short_display
NULL
