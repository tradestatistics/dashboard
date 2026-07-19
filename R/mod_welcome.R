#' @title Welcome UI Function
#' @description A tabler Module.
#' @param id Internal parameters for Tabler.
mod_welcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      card(
        h1("Open Trade Statistics"),
        br(),
        br(),
        p("Open Trade Statistics started as a visualization project back in 2017."),
        p(HTML('The data used here is provided by the United States International Trade Commission (<a href="https://www.usitc.gov/">USITC</a>).')),
        p("Explore the country and sector profiles using the navigation menu."),
        br(),
        p("Check the R package to download the data displayed here:"),
        a(
          href = "https://github.com/ropensci/tradestatistics",
          target = "_blank",
          class = "btn btn-outline btn-dark",
          "R package"
        ),
        br(),
        br(),
        p("If this resource is useful to you, please consider donating. The dashboard and the SQL database/API behind it will remain open and free of charge but there is a hosting cost."),
        p(
          a(
            href = "https://www.buymeacoffee.com/pacha", target = "_blank",
            img(
              src = "https://raw.githubusercontent.com/pachadotdev/buymeacoffee-badges/main/bmc-black.svg",
              alt = "Buy me a coffee",
              style = "height:34px;"
            )
          )
        )
      )
    )
  )
}
