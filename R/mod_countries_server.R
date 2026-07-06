#' @title Country profile server-side function
#' @description A shiny Module.
#' @param id Internal parameter for Shiny.
mod_countries_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Connect to SQL ----
    con <- sql_con()

    session$onSessionEnded(function() {
      if (!is.null(con) && dbIsValid(con)) {
        poolClose(con)
      }
    })

    # User inputs ----
    inp_y <- reactive({
      y <- c(min(input$y[1], input$y[2]), max(input$y[1], input$y[2]))
      return(y)
    })

    inp_r <- reactive({
      input$r
    }) # reporter
    inp_p <- reactive({
      input$p
    }) # partner
    inp_d <- reactive({
      input$d
    }) # adjust dollar
    inp_fmt <- reactive({
      input$fmt
    }) # format

    tbl_agg <- "itpde_imp_exp"
    tbl_dtl <- "itpde"

    # Human-readable reporter/partner names for glue templates. Fallback to
    # the code when no display name is available.
    rname <- eventReactive(input$go, {
      out <- names(tradestatisticsshiny::reporters_display[tradestatisticsshiny::reporters_display == inp_r()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_r())
      }
      out
    })

    pname <- eventReactive(input$go, {
      out <- names(tradestatisticsshiny::reporters_display[tradestatisticsshiny::reporters_display == inp_p()])
      if (length(out) == 0 || is.na(out) || nchar(out) == 0) {
        return(inp_p())
      }
      out
    })

    title <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }: Multilateral trade { min(inp_y()) } - { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } and { r_add_the(pname()) } { pname() }: Bilateral trade { min(inp_y()) } - { max(inp_y()) }")
      }
    })

    # Visualize ----

    ## Data ----

    df_agg <- reactive({
      session$sendCustomMessage("showProgress", list(text = "Loading data..."))

      years <- inp_y()
      reporter <- inp_r()
      partner <- inp_p()
      year_in <- paste(as.integer(years), collapse = ",")
      r <- gsub("'", "''", reporter)

      if (partner == "ALL") {
        d_exp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_value_usd_exp
           FROM %s WHERE year IN (%s) AND exporter_iso3_dynamic = '%s'
           GROUP BY year",
          tbl_agg, year_in, r
        )))
        d_imp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_value_usd_imp
           FROM %s WHERE year IN (%s) AND importer_iso3_dynamic = '%s'
           GROUP BY year",
          tbl_agg, year_in, r
        )))
      } else {
        p <- gsub("'", "''", partner)
        d_exp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_value_usd_exp
           FROM %s WHERE year IN (%s) AND exporter_iso3_dynamic = '%s' AND importer_iso3_dynamic = '%s'
           GROUP BY year",
          tbl_agg, year_in, r, p
        )))
        d_imp <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT year, SUM(trade) AS trade_value_usd_imp
           FROM %s WHERE year IN (%s) AND importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s'
           GROUP BY year",
          tbl_agg, year_in, r, p
        )))
      }

      d <- merge(d_exp, d_imp, by = "year", all = TRUE)
      d[is.na(trade_value_usd_exp), trade_value_usd_exp := 0]
      d[is.na(trade_value_usd_imp), trade_value_usd_imp := 0]
      return(d)
    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    df_dtl <- reactive({
      years <- inp_y()
      reporter <- inp_r()
      partner <- inp_p()
      year_in <- paste(as.integer(years), collapse = ",")
      r <- gsub("'", "''", reporter)

      # Get sector reference data
      sectors_ref <- setDT(pool::dbGetQuery(con,
        "select industry_descr, industry_id from itpd_industries"
      ))

      if (partner == "ALL") {
        d <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT * FROM %s WHERE year IN (%s) AND (importer_iso3_dynamic = '%s' OR exporter_iso3_dynamic = '%s')",
          tbl_dtl, year_in, r, r
        )))
      } else {
        p <- gsub("'", "''", partner)
        d <- setDT(pool::dbGetQuery(con, sprintf(
          "SELECT * FROM %s WHERE year IN (%s) AND (
            (importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s') OR
            (importer_iso3_dynamic = '%s' AND exporter_iso3_dynamic = '%s')
          )",
          tbl_dtl, year_in, r, p, p, r
        )))
      }

      d <- merge(d, sectors_ref, by = "industry_id")
      return(d)
    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    ## Trade ----

    ### Tables ----

    # Consolidated trade values calculation for efficiency
    trade_values <- eventReactive(input$go, {
      yrs <- c(min(inp_y()), max(inp_y()))
      df_agg()[year %in% yrs, .(year, trade_value_usd_exp, trade_value_usd_imp)]
    })

    exp_val_min_yr <- eventReactive(input$go, {
      trade_values()[year == min(inp_y()), trade_value_usd_exp]
    })

    exp_val_max_yr <- eventReactive(input$go, {
      trade_values()[year == max(inp_y()), trade_value_usd_exp]
    })

    imp_val_min_yr <- eventReactive(input$go, {
      trade_values()[year == min(inp_y()), trade_value_usd_imp]
    })

    imp_val_max_yr <- eventReactive(input$go, {
      trade_values()[year == max(inp_y()), trade_value_usd_imp]
    })

    exp_val_min_yr_2 <- eventReactive(input$go, {
      show_dollars(exp_val_min_yr())
    })

    exp_val_max_yr_2 <- eventReactive(input$go, {
      show_dollars(exp_val_max_yr())
    })

    imp_val_min_yr_2 <- eventReactive(input$go, {
      show_dollars(imp_val_min_yr())
    })

    imp_val_max_yr_2 <- eventReactive(input$go, {
      show_dollars(imp_val_max_yr())
    })

    exports_growth <- eventReactive(input$go, {
      growth_rate(
        exp_val_max_yr(), exp_val_min_yr(), inp_y()
      )
    })

    exports_growth_2 <- eventReactive(input$go, {
      show_percentage(abs(exports_growth()))
    })

    exports_growth_increase_decrease <- eventReactive(input$go, {
      ifelse(exports_growth() >= 0, "increased", "decreased")
    })

    exports_growth_increase_decrease_2 <- eventReactive(input$go, {
      ifelse(exports_growth() >= 0, "increase", "decrease")
    })

    imports_growth <- eventReactive(input$go, {
      growth_rate(
        imp_val_max_yr(), imp_val_min_yr(), inp_y()
      )
    })

    imports_growth_2 <- eventReactive(input$go, {
      show_percentage(abs(imports_growth()))
    })

    imports_growth_increase_decrease <- eventReactive(input$go, {
      ifelse(imports_growth() >= 0, "increased", "decreased")
    })

    imports_growth_increase_decrease_2 <- eventReactive(input$go, {
      ifelse(imports_growth() >= 0, "increase", "decrease")
    })

    trd_rankings <- eventReactive(input$go, {
      min_max_y <- c(min(inp_y()), max(inp_y()))
      year_in <- paste(as.integer(min_max_y), collapse = ",")
      r <- gsub("'", "''", inp_r())

      # Exports: reporter is exporter_iso3_dynamic, partner is importer_iso3_dynamic
      d_exp <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT year, importer_iso3_dynamic AS partner, SUM(trade) AS trade_value_usd_exp
         FROM %s WHERE year IN (%s) AND exporter_iso3_dynamic = '%s'
         GROUP BY year, importer_iso3_dynamic",
        tbl_agg, year_in, r
      )))

      # Imports: reporter is importer_iso3_dynamic, partner is exporter_iso3_dynamic
      d_imp <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT year, exporter_iso3_dynamic AS partner, SUM(trade) AS trade_value_usd_imp
         FROM %s WHERE year IN (%s) AND importer_iso3_dynamic = '%s'
         GROUP BY year, exporter_iso3_dynamic",
        tbl_agg, year_in, r
      )))

      d <- merge(d_exp, d_imp, by = c("year", "partner"), all = TRUE)
      d[is.na(trade_value_usd_exp), trade_value_usd_exp := 0]
      d[is.na(trade_value_usd_imp), trade_value_usd_imp := 0]
      setnames(d, "partner", "exporter_iso3_dynamic")

      d[, trd_value_usd_bal := trade_value_usd_exp + trade_value_usd_imp]
      d[, bal_rank := frankv(trd_value_usd_bal, order = -1L, ties.method = "dense"), by = .(year)]
      d[, exp_share := trade_value_usd_exp / sum(trade_value_usd_exp, na.rm = TRUE), by = .(year)]
      d[, imp_share := trade_value_usd_imp / sum(trade_value_usd_imp, na.rm = TRUE), by = .(year)]

      return(d)
    })

    # Helper function to get ranking with tie information
    get_ranking_with_ties <- function(year_val) {
      if (inp_p() == "ALL") {
        return("N/A") # No ranking for multilateral trade
      }

      rankings_data <- trd_rankings()[year == year_val]
      partner_iso_val <- inp_p()

      partner_rank <- rankings_data[exporter_iso3_dynamic == partner_iso_val, bal_rank]

      if (length(partner_rank) == 0 || is.na(partner_rank)) {
        return("N/A")
      }

      # Check for ties
      tied_count <- rankings_data[bal_rank == partner_rank & exporter_iso3_dynamic != partner_iso_val, .N]

      if (tied_count > 0) {
        return(paste0(
          partner_rank, " (tied with ", tied_count, " other",
          ifelse(tied_count == 1, "", "s"), ")"
        ))
      } else {
        return(as.character(partner_rank))
      }
    }

    trd_rankings_no_min_yr <- eventReactive(input$go, {
      get_ranking_with_ties(min(inp_y()))
    })

    trd_rankings_no_max_yr <- eventReactive(input$go, {
      get_ranking_with_ties(max(inp_y()))
    })

    trd_rankings_remained <- eventReactive(input$go, {
      min_rank <- trd_rankings_no_min_yr()
      max_rank <- trd_rankings_no_max_yr()

      if (min_rank == "N/A" || max_rank == "N/A") {
        return("was")
      }

      # Extract just the numeric part for comparison (remove tie information)
      min_rank_num <- as.numeric(gsub(" \\(.*\\)", "", min_rank))
      max_rank_num <- as.numeric(gsub(" \\(.*\\)", "", max_rank))

      ifelse(
        min_rank_num == max_rank_num,
        "remained",
        "moved to"
      )
    })

    trd_rankings_exp_share_min_yr <- eventReactive(input$go, {
      result <- trd_rankings()[year == min(inp_y()) & exporter_iso3_dynamic == inp_p(), exp_share]
      if (length(result) == 0 || is.na(result)) return(0)
      return(result)
    })

    trd_rankings_exp_share_min_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_exp_share_min_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_exp_share_max_yr <- eventReactive(input$go, {
      result <- trd_rankings()[year == max(inp_y()) & exporter_iso3_dynamic == inp_p(), exp_share]
      if (length(result) == 0 || is.na(result)) return(0)
      return(result)
    })

    trd_rankings_exp_share_max_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_exp_share_max_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_imp_share_min_yr <- eventReactive(input$go, {
      result <- trd_rankings()[year == min(inp_y()) & exporter_iso3_dynamic == inp_p(), imp_share]
      if (length(result) == 0 || is.na(result)) return(0)
      return(result)
    })

    trd_rankings_imp_share_min_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_imp_share_min_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    trd_rankings_imp_share_max_yr <- eventReactive(input$go, {
      result <- trd_rankings()[year == max(inp_y()) & exporter_iso3_dynamic == inp_p(), imp_share]
      if (length(result) == 0 || is.na(result)) return(0)
      return(result)
    })

    trd_rankings_imp_share_max_yr_2 <- eventReactive(input$go, {
      share_val <- trd_rankings_imp_share_max_yr()
      if (is.na(share_val) || share_val <= 0) {
        return("N/A")
      }
      show_percentage(share_val)
    })

    ### GDP Context Functions ----

    # Get GDP data for the reporter country
    gdp_data <- eventReactive(input$go, {
      r <- gsub("'", "''", inp_r())
      year_in <- paste(as.integer(inp_y()), collapse = ",")

      gdp_exp <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT year, MAX(gdp_pwt_cur_o) AS gdp_exp
         FROM dgd WHERE iso3_dynamic_o = '%s' AND year IN (%s)
         GROUP BY year",
        r, year_in
      )))

      gdp_imp <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT year, MAX(gdp_pwt_cur_d) AS gdp_imp
         FROM dgd WHERE iso3_dynamic_d = '%s' AND year IN (%s)
         GROUP BY year",
        r, year_in
      )))

      merge(gdp_exp, gdp_imp, by = "year", all = TRUE)
    })

    # Calculate trade as percentage of GDP for exports
    exp_gdp_pct_min_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data()[year == min(inp_y()), gdp_exp]

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      exp_val <- exp_val_min_yr()
      if (is.na(exp_val) || exp_val <= 0) {
        return(NA)
      }

      return(round((exp_val / gdp_val) * 100, 2))
    })

    exp_gdp_pct_max_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data()[year == max(inp_y()), gdp_exp]

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      exp_val <- exp_val_max_yr()
      if (is.na(exp_val) || exp_val <= 0) {
        return(NA)
      }

      return(round((exp_val / gdp_val) * 100, 2))
    })

    # Calculate trade as percentage of GDP for imports
    imp_gdp_pct_min_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data()[year == min(inp_y()), gdp_imp]

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      imp_val <- imp_val_min_yr()
      if (is.na(imp_val) || imp_val <= 0) {
        return(NA)
      }

      return(round((imp_val / gdp_val) * 100, 2))
    })

    imp_gdp_pct_max_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data()[year == max(inp_y()), gdp_imp]

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      imp_val <- imp_val_max_yr()
      if (is.na(imp_val) || imp_val <= 0) {
        return(NA)
      }

      return(round((imp_val / gdp_val) * 100, 2))
    })

    # Get total exports for bilateral context (when partner != "ALL")
    total_exp_val_min_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        return(exp_val_min_yr())
      }

      min_year <- min(inp_y())
      reporter <- inp_r()

      d <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year = %d AND exporter_iso3_dynamic = '%s'",
        tbl_agg, as.integer(min_year), gsub("'", "''", reporter)
      )))

      return(d[, sum(trade, na.rm = TRUE)])
    })

    total_exp_val_max_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        return(exp_val_max_yr())
      }

      max_year <- max(inp_y())
      reporter <- inp_r()

      d <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year = %d AND exporter_iso3_dynamic = '%s'",
        tbl_agg, as.integer(max_year), gsub("'", "''", reporter)
      )))

      return(d[, sum(trade, na.rm = TRUE)])
    })

    # Calculate total exports as percentage of GDP for bilateral context
    total_exp_gdp_pct_min_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data()[year == min(inp_y()), gdp_exp]

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      total_exp_val <- total_exp_val_min_yr()
      if (is.na(total_exp_val) || total_exp_val <= 0) {
        return(NA)
      }

      return(round((total_exp_val / gdp_val) * 100, 2))
    })

    total_exp_gdp_pct_max_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data()[year == max(inp_y()), gdp_exp]

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      total_exp_val <- total_exp_val_max_yr()
      if (is.na(total_exp_val) || total_exp_val <= 0) {
        return(NA)
      }

      return(round((total_exp_val / gdp_val) * 100, 2))
    })

    # Get total imports for bilateral context (when partner != "ALL")
    total_imp_val_min_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        return(imp_val_min_yr())
      }

      min_year <- min(inp_y())
      reporter <- inp_r()

      d <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year = %d AND importer_iso3_dynamic = '%s'",
        tbl_agg, as.integer(min_year), gsub("'", "''", reporter)
      )))

      return(d[, sum(trade, na.rm = TRUE)])
    })

    total_imp_val_max_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        return(imp_val_max_yr())
      }

      max_year <- max(inp_y())
      reporter <- inp_r()

      d <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year = %d AND importer_iso3_dynamic = '%s'",
        tbl_agg, as.integer(max_year), gsub("'", "''", reporter)
      )))

      return(d[, sum(trade, na.rm = TRUE)])
    })

    # Calculate total imports as percentage of GDP for bilateral context
    total_imp_gdp_pct_min_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data()[year == min(inp_y()), gdp_imp]

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      total_imp_val <- total_imp_val_min_yr()
      if (is.na(total_imp_val) || total_imp_val <= 0) {
        return(NA)
      }

      return(round((total_imp_val / gdp_val) * 100, 2))
    })

    total_imp_gdp_pct_max_yr <- eventReactive(input$go, {
      gdp_val <- gdp_data()[year == max(inp_y()), gdp_imp]

      if (length(gdp_val) == 0 || is.na(gdp_val) || gdp_val <= 0) {
        return(NA)
      }

      total_imp_val <- total_imp_val_max_yr()
      if (is.na(total_imp_val) || total_imp_val <= 0) {
        return(NA)
      }

      return(round((total_imp_val / gdp_val) * 100, 2))
    })

    ### Text/Visual elements ----

    trd_smr_txt_exp <- eventReactive(input$go, {
      # Base text - more concise and readable
      base_text <- if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }'s exports to the world { exports_growth_increase_decrease() } from { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) } ({ exports_growth_2() } annual { exports_growth_increase_decrease_2() }).")
      } else {
        # Split into two shorter sentences for better readability
        main_sentence <- glue("{ r_add_upp_the(rname()) } { rname() }'s exports to { r_add_the(pname()) } { pname() } { exports_growth_increase_decrease() } from { exp_val_min_yr_2() } in { min(inp_y()) } to { exp_val_max_yr_2() } in { max(inp_y()) } ({ exports_growth_2() } annual { exports_growth_increase_decrease_2() }).")
        ranking_sentence <- glue("{ r_add_upp_the(pname()) } { pname() } ranked No. { trd_rankings_no_min_yr() } in { min(inp_y()) } ({ trd_rankings_exp_share_min_yr_2() } of exports) and { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } ({ trd_rankings_exp_share_max_yr_2() }).")
        paste(main_sentence, ranking_sentence)
      }

      # Add GDP context only if we have valid data
      gdp_context <- ""
      min_pct <- exp_gdp_pct_min_yr()
      max_pct <- exp_gdp_pct_max_yr()

      if (!is.na(min_pct) && !is.na(max_pct) && min_pct > 0 && max_pct > 0) {
        if (inp_p() == "ALL") {
          # Multilateral: concise GDP context
          gdp_context <- glue(" These exports were { min_pct }% of { r_add_the(rname()) } { rname() }'s GDP in { min(inp_y()) } and { max_pct }% in { max(inp_y()) }.")
        } else {
          # Bilateral: show bilateral exports as % of GDP, and total if significantly different
          total_min_pct <- total_exp_gdp_pct_min_yr()
          total_max_pct <- total_exp_gdp_pct_max_yr()

          if (!is.na(total_min_pct) && !is.na(total_max_pct) && total_min_pct > 0 && total_max_pct > 0) {
            gdp_context <- glue(" This trade was { min_pct }% of { r_add_the(rname()) } { rname() }'s GDP in { min(inp_y()) } and { max_pct }% in { max(inp_y()) } (total exports: { total_min_pct }% and { total_max_pct }%).")
          }
        }
      }

      paste0(base_text, gdp_context)
    })

    trd_smr_txt_imp <- eventReactive(input$go, {
      # Base text - more concise and readable
      base_text <- if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }'s imports from the world { imports_growth_increase_decrease() } from { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) } ({ imports_growth_2() } annual { imports_growth_increase_decrease_2() }).")
      } else {
        # Split into two shorter sentences for better readability
        main_sentence <- glue("{ r_add_upp_the(rname()) } { rname() }'s imports from { r_add_the(pname()) } { pname() } { imports_growth_increase_decrease() } from { imp_val_min_yr_2() } in { min(inp_y()) } to { imp_val_max_yr_2() } in { max(inp_y()) } ({ imports_growth_2() } annual { imports_growth_increase_decrease_2() }).")
        ranking_sentence <- glue("{ r_add_upp_the(pname()) } { pname() } ranked No. { trd_rankings_no_min_yr() } in { min(inp_y()) } ({ trd_rankings_imp_share_min_yr_2() } of imports) and { trd_rankings_remained() } No. { trd_rankings_no_max_yr() } in { max(inp_y()) } ({ trd_rankings_imp_share_max_yr_2() }).")
        paste(main_sentence, ranking_sentence)
      }

      # Add GDP context only if we have valid data
      gdp_context <- ""
      min_pct <- imp_gdp_pct_min_yr()
      max_pct <- imp_gdp_pct_max_yr()

      if (!is.na(min_pct) && !is.na(max_pct) && min_pct > 0 && max_pct > 0) {
        if (inp_p() == "ALL") {
          # Multilateral: concise GDP context
          gdp_context <- glue(" These imports were { min_pct }% of GDP in { min(inp_y()) } and { max_pct }% in { max(inp_y()) }.")
        } else {
          # Bilateral: show bilateral imports as % of GDP, and total if significantly different
          total_min_pct <- total_imp_gdp_pct_min_yr()
          total_max_pct <- total_imp_gdp_pct_max_yr()

          if (!is.na(total_min_pct) && !is.na(total_max_pct) && total_min_pct > 0 && total_max_pct > 0) {
            gdp_context <- glue(" This trade was { min_pct }% of GDP in { min(inp_y()) } and { max_pct }% in { max(inp_y()) } (total imports: { total_min_pct }% and { total_max_pct }%).")
          }
        }
      }

      paste0(base_text, gdp_context)
    })

    trd_exc_columns_title <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("{ r_add_upp_the(rname()) } { rname() }: Imports and Exports { min(inp_y()) } - { max(inp_y()) }")
      } else {
        glue("{ r_add_upp_the(rname()) } { rname() } and { r_add_the(pname()) } { pname() }: Bilateral trade { min(inp_y()) } - { max(inp_y()) }")
      }
    })

    trd_exc_columns_agg <- reactive({
      d_vals <- trade_values()

      d <- rbindlist(list(
        data.table(year = d_vals$year, trade = round(d_vals$trade_value_usd_exp / 1e9, 2), flow = "Exports"),
        data.table(year = d_vals$year, trade = round(d_vals$trade_value_usd_imp / 1e9, 2), flow = "Imports")
      ))
      d[, `:=`(year = as.character(year), color = fifelse(flow == "Exports", "#67c090", "#26667f"))]

      d3po(d) |>
        po_bar(
          daes(
            x = .data$year,
            y = .data$trade,
            group = .data$flow,
            color = .data$color,
            stack = FALSE
          )
        ) |>
        po_labels(
          x = "Year",
          y = "Trade Value (USD billion)",
          title = trd_exc_columns_title()
        ) |>
        po_format(
          y = format(.data$trade, big.mark = " ", scientific = FALSE)
        ) |>
        # po_tooltip("{flow}: {trade} B") |>
        po_tooltip(JS(
          "function(value, row) {
            if (!row) return '';
            var grp = (row.flow != null) ? row.flow : (row.group != null ? row.group : '');
            var val = (value != null && !isNaN(value)) ? Number(value) : (row.trade != null && !isNaN(row.trade) ? Number(row.trade) : 0);
            var groupPrefix = grp ? (grp + ': ') : '';
            return groupPrefix + (val || 0) + ' B' ;
          }"
        ))

    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    ## Exports ----

    ### Visual elements ----

    exp_tt_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("Exports of { r_add_the(rname()) } { rname() } to the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product")
      } else {
        glue("Exports of { r_add_the(rname()) } { rname() } to { r_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
      }
    })

    # Export column chart titles
    exp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Export Destinations in { min(inp_y()) }")
    })

    exp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Export Destinations in { max(inp_y()) }")
    })

    # Export column charts
    exp_col_min_yr_usd <- reactive({
      min_year <- min(inp_y())
      reporter <- inp_r()

      countries_ref <- setDT(pool::dbGetQuery(con, "SELECT dynamic_code AS country_iso, country AS country_name FROM dgd_countries"))

      # Exports: reporter is the exporter_iso3_dynamic; importer_iso3_dynamic is the destination
      d_raw <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year = %d AND exporter_iso3_dynamic = '%s'",
        tbl_dtl, as.integer(min_year), gsub("'", "''", reporter)
      )))
      d <- merge(d_raw, countries_ref, by.x = "importer_iso3_dynamic", by.y = "country_iso")
      d <- d[, .(trade_value_usd_exp = sum(trade, na.rm = TRUE)), by = .(country_name)]
      d <- d[trade_value_usd_exp > 0]

      if (inp_p() == "ALL") {
        setorder(d, -trade_value_usd_exp)
        top4 <- d[seq_len(min(4L, .N)), country_name]
        d[!(country_name %in% top4), country_name := "Rest of the world"]
        d <- d[, .(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = TRUE)), by = .(country_name)]

        rest <- d[country_name == "Rest of the world"][, n := 5L]
        others <- d[country_name != "Rest of the world"][order(-trade_value_usd_exp)][, n := .I]
        d <- rbindlist(list(rest, others), fill = TRUE)
        d[, country_name := paste(n, country_name, sep = " - ")]
        d[, trade_value_usd_exp := round(trade_value_usd_exp / 1e9, 2)]
        d[, color := "#67c090"]
        d[, n := NULL]
      } else {
        pname_val <- pname()
        setorder(d, -trade_value_usd_exp)
        d[, n := .I]
        d[n > 4L & country_name != pname_val, country_name := "Rest of the world"]
        d[, n := NULL]
        d <- d[, .(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = TRUE)), by = .(country_name)]
        d[, color := fifelse(country_name == pname_val, "#d04e66", "#67c090")]

        rest <- d[country_name == "Rest of the world"][, n := 5L]
        others <- d[country_name != "Rest of the world"][order(-trade_value_usd_exp)][, n := .I]
        d <- rbindlist(list(rest, others), fill = TRUE)
        d[, country_name := paste(n, country_name, sep = " - ")]
        d[, trade_value_usd_exp := round(trade_value_usd_exp / 1e9, 2)]
        d[, n := NULL]
      }

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_exp,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(
          title = exp_col_min_yr_usd_tt(),
          y = "Country",
          x = "Trade Value (USD Billion)"
        ) |>
        po_format(x = format(.data$trade_value_usd_exp, big.mark = " ", scientific = FALSE)) |>
        po_tooltip("{country_name}: {trade_value_usd_exp} B")

    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    exp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())
      reporter <- inp_r()

      countries_ref <- setDT(pool::dbGetQuery(con, "SELECT dynamic_code AS country_iso, country AS country_name FROM dgd_countries"))

      # Exports: reporter is the exporter_iso3_dynamic; importer_iso3_dynamic is the destination
      d_raw <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year = %d AND exporter_iso3_dynamic = '%s'",
        tbl_dtl, as.integer(max_year), gsub("'", "''", reporter)
      )))
      d <- merge(d_raw, countries_ref, by.x = "importer_iso3_dynamic", by.y = "country_iso")
      d <- d[, .(trade_value_usd_exp = sum(trade, na.rm = TRUE)), by = .(country_name)]
      d <- d[trade_value_usd_exp > 0]

      if (inp_p() == "ALL") {
        setorder(d, -trade_value_usd_exp)
        top4 <- d[seq_len(min(4L, .N)), country_name]
        d[!(country_name %in% top4), country_name := "Rest of the world"]
        d <- d[, .(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = TRUE)), by = .(country_name)]

        rest <- d[country_name == "Rest of the world"][, n := 5L]
        others <- d[country_name != "Rest of the world"][order(-trade_value_usd_exp)][, n := .I]
        d <- rbindlist(list(rest, others), fill = TRUE)
        d[, country_name := paste(n, country_name, sep = " - ")]
        d[, trade_value_usd_exp := round(trade_value_usd_exp / 1e9, 2)]
        d[, color := "#67c090"]
        d[, n := NULL]
      } else {
        pname_val <- pname()
        setorder(d, -trade_value_usd_exp)
        d[, n := .I]
        d[n > 4L & country_name != pname_val, country_name := "Rest of the world"]
        d[, n := NULL]
        d <- d[, .(trade_value_usd_exp = sum(trade_value_usd_exp, na.rm = TRUE)), by = .(country_name)]
        d[, color := fifelse(country_name == pname_val, "#d04e66", "#67c090")]

        rest <- d[country_name == "Rest of the world"][, n := 5L]
        others <- d[country_name != "Rest of the world"][order(-trade_value_usd_exp)][, n := .I]
        d <- rbindlist(list(rest, others), fill = TRUE)
        d[, country_name := paste(n, country_name, sep = " - ")]
        d[, trade_value_usd_exp := round(trade_value_usd_exp / 1e9, 2)]
        d[, n := NULL]
      }

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_exp,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(title = exp_col_max_yr_usd_tt(), y = "Country", x = "Trade Value (USD Billion)") |>
        po_format(x = format(.data$trade_value_usd_exp, big.mark = " ", scientific = FALSE)) |>
        po_tooltip("{country_name}: {trade_value_usd_exp} B")

    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    exp_tt_min_yr <- eventReactive(input$go, {
      glue("Export Composition in { min(inp_y()) }")
    })

    exp_tm_dtl_min_yr <- reactive({
      reporter <- inp_r()
      d <- p_aggregate_by_section(
        df_dtl()[year == min(inp_y()) & exporter_iso3_dynamic == reporter],
        col = "trade", con = con
      )
      d2 <- p_colors(d, con = con)
      p_treemap(d, d2, title = exp_tt_min_yr())
    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    exp_tt_max_yr <- eventReactive(input$go, {
      glue("Export Composition in { max(inp_y()) }")
    })

    exp_tm_dtl_max_yr <- reactive({
      reporter <- inp_r()
      d <- p_aggregate_by_section(
        df_dtl()[year == max(inp_y()) & exporter_iso3_dynamic == reporter],
        col = "trade", con = con
      )
      d2 <- p_colors(d, con = con)
      p_treemap(d, d2, title = exp_tt_max_yr())
    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    ## Imports ----

    ### Visual elements ----

    imp_tt_yr <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("Imports of { r_add_the(rname()) } { rname() } from the rest of the World in { min(inp_y()) } and { max(inp_y()) }, by product")
      } else {
        glue("Imports of { r_add_the(rname()) } { rname() } from { r_add_the(pname()) } { pname() } in { min(inp_y()) } and { max(inp_y()) }, by product")
      }
    })

    imp_tt_min_yr <- eventReactive(input$go, {
      glue("Import Composition in { min(inp_y()) }")
    })

    # Import column chart titles
    imp_col_min_yr_usd_tt <- eventReactive(input$go, {
      glue("Import Origins in { min(inp_y()) }")
    })

    imp_col_max_yr_usd_tt <- eventReactive(input$go, {
      glue("Import Origins in { max(inp_y()) }")
    })

    # Import column charts
    imp_col_min_yr_usd <- reactive({
      min_year <- min(inp_y())
      reporter <- inp_r()

      countries_ref <- setDT(pool::dbGetQuery(con, "SELECT dynamic_code AS country_iso, country AS country_name FROM dgd_countries"))

      # Imports: reporter is the importer_iso3_dynamic; exporter_iso3_dynamic is the origin
      d_raw <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year = %d AND importer_iso3_dynamic = '%s'",
        tbl_dtl, as.integer(min_year), gsub("'", "''", reporter)
      )))
      d <- merge(d_raw, countries_ref, by.x = "exporter_iso3_dynamic", by.y = "country_iso")
      d <- d[, .(trade_value_usd_imp = sum(trade, na.rm = TRUE)), by = .(country_name)]
      d <- d[trade_value_usd_imp > 0]

      if (inp_p() == "ALL") {
        setorder(d, -trade_value_usd_imp)
        top4 <- d[seq_len(min(4L, .N)), country_name]
        d[!(country_name %in% top4), country_name := "Rest of the world"]
        d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]

        rest <- d[country_name == "Rest of the world"][, n := 5L]
        others <- d[country_name != "Rest of the world"][order(-trade_value_usd_imp)][, n := .I]
        d <- rbindlist(list(rest, others), fill = TRUE)
        d[, country_name := paste(n, country_name, sep = " - ")]
        d[, trade_value_usd_imp := round(trade_value_usd_imp / 1e9, 2)]
        d[, color := "#26667f"]
        d[, n := NULL]
      } else {
        pname_val <- pname()
        setorder(d, -trade_value_usd_imp)
        d[, n := .I]
        d[n > 4L & country_name != pname_val, country_name := "Rest of the world"]
        d[, n := NULL]
        d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]
        d[, color := fifelse(country_name == pname_val, "#d04e66", "#26667f")]

        rest <- d[country_name == "Rest of the world"][, n := 5L]
        others <- d[country_name != "Rest of the world"][order(-trade_value_usd_imp)][, n := .I]
        d <- rbindlist(list(rest, others), fill = TRUE)
        d[, country_name := paste(n, country_name, sep = " - ")]
        d[, trade_value_usd_imp := round(trade_value_usd_imp / 1e9, 2)]
        d[, n := NULL]
      }

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(title = imp_col_min_yr_usd_tt(), y = "Country", x = "Trade Value (USD Billion)") |>
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE)) |>
        po_tooltip("{country_name}: {trade_value_usd_imp} B")

    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    imp_col_max_yr_usd <- reactive({
      max_year <- max(inp_y())
      reporter <- inp_r()

      countries_ref <- setDT(pool::dbGetQuery(con, "SELECT dynamic_code AS country_iso, country AS country_name FROM dgd_countries"))

      # Imports: reporter is the importer_iso3_dynamic; exporter_iso3_dynamic is the origin
      d_raw <- setDT(pool::dbGetQuery(con, sprintf(
        "SELECT * FROM %s WHERE year = %d AND importer_iso3_dynamic = '%s'",
        tbl_dtl, as.integer(max_year), gsub("'", "''", reporter)
      )))
      d <- merge(d_raw, countries_ref, by.x = "exporter_iso3_dynamic", by.y = "country_iso")
      d <- d[, .(trade_value_usd_imp = sum(trade, na.rm = TRUE)), by = .(country_name)]
      d <- d[trade_value_usd_imp > 0]

      if (inp_p() == "ALL") {
        setorder(d, -trade_value_usd_imp)
        top4 <- d[seq_len(min(4L, .N)), country_name]
        d[!(country_name %in% top4), country_name := "Rest of the world"]
        d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]

        rest <- d[country_name == "Rest of the world"][, n := 5L]
        others <- d[country_name != "Rest of the world"][order(-trade_value_usd_imp)][, n := .I]
        d <- rbindlist(list(rest, others), fill = TRUE)
        d[, country_name := paste(n, country_name, sep = " - ")]
        d[, trade_value_usd_imp := round(trade_value_usd_imp / 1e9, 2)]
        d[, color := "#26667f"]
        d[, n := NULL]
      } else {
        pname_val <- pname()
        setorder(d, -trade_value_usd_imp)
        d[, n := .I]
        d[n > 4L & country_name != pname_val, country_name := "Rest of the world"]
        d[, n := NULL]
        d <- d[, .(trade_value_usd_imp = sum(trade_value_usd_imp, na.rm = TRUE)), by = .(country_name)]
        d[, color := fifelse(country_name == pname_val, "#d04e66", "#26667f")]

        rest <- d[country_name == "Rest of the world"][, n := 5L]
        others <- d[country_name != "Rest of the world"][order(-trade_value_usd_imp)][, n := .I]
        d <- rbindlist(list(rest, others), fill = TRUE)
        d[, country_name := paste(n, country_name, sep = " - ")]
        d[, trade_value_usd_imp := round(trade_value_usd_imp / 1e9, 2)]
        d[, n := NULL]
      }

      d3po(d) |>
        po_bar(
          daes(
            y = .data$country_name,
            x = .data$trade_value_usd_imp,
            color = .data$color,
            sort = "asc-y"
          )
        ) |>
        po_labels(title = imp_col_max_yr_usd_tt(), y = "Country", x = "Trade Value (USD Billion)") |>
        po_format(x = format(.data$trade_value_usd_imp, big.mark = " ", scientific = FALSE)) |>
        po_tooltip("{country_name}: {trade_value_usd_imp} B")

    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    imp_tm_dtl_min_yr <- reactive({
      reporter <- inp_r()
      d <- p_aggregate_by_section(
        df_dtl()[year == min(inp_y()) & importer_iso3_dynamic == reporter],
        col = "trade", con = con
      )
      d2 <- p_colors(d, con = con)
      p_treemap(d, d2, title = imp_tt_min_yr())
    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    imp_tt_max_yr <- eventReactive(input$go, {
      glue("Import Composition in { max(inp_y()) }")
    })

    imp_tm_dtl_max_yr <- reactive({
      reporter <- inp_r()
      d <- p_aggregate_by_section(
        df_dtl()[year == max(inp_y()) & importer_iso3_dynamic == reporter],
        col = "trade", con = con
      )
      d2 <- p_colors(d, con = con)
      out <- p_treemap(d, d2, title = imp_tt_max_yr())
      session$sendCustomMessage("hideProgress", list())
      return(out)
    }) |>
      bindCache(inp_y(), inp_r(), inp_p(), inp_d()) |>
      bindEvent(input$go)

    ## Dynamic / server side selectors ----

    observeEvent(input$r, {
      updateSelectizeInput(session, "p",
        choices = sort(tradestatisticsshiny::reporters_display[
          tradestatisticsshiny::reporters_display != input$r
        ]),
        selected = "ALL",
        server = TRUE
      )
    })

    ## Download ----

    dwn_stl <- eventReactive(input$go, {
      "Download country data"
    })

    dwn_txt <- eventReactive(input$go, {
      "Select the correct format for your favourite language or software of choice. The dashboard can export to CSV/TSV/XLSX for Excel or any other software, but also to SAV (SPSS) and DTA (Stata)."
    })

    dwn_fmt <- eventReactive(input$go, {
      selectInput(
        ns("fmt"),
        "Format",
        choices = available_formats(),
        selected = NULL,
        selectize = TRUE
      )
    })

    ## Outputs ----

    ## Titles ----

    output$title <- renderText({
      title()
    })

    ## Country profile ----

    ### Trade ----

    output$trd_stl <- eventReactive(input$go, {
      if (inp_p() == "ALL") {
        glue("Total multilateral Exports and Imports")
      } else {
        glue("Total bilateral Exports and Imports")
      }
    })

    output$trd_stl_exp <- eventReactive(input$go, {
      "Exports"
    })
    output$trd_stl_imp <- eventReactive(input$go, {
      "Imports"
    })

    output$trd_smr_exp <- renderText(trd_smr_txt_exp())
    output$trd_smr_imp <- renderText(trd_smr_txt_imp())

    output$trd_exc_columns_agg <- render_d3po({
      trd_exc_columns_agg()
    })

    ### Exports ----

    output$exp_tt_yr <- renderText(exp_tt_yr())

    # Export column chart outputs
    output$exp_col_min_yr_usd_tt <- renderText(exp_col_min_yr_usd_tt())
    output$exp_col_min_yr_usd <- render_d3po({
      exp_col_min_yr_usd()
    })

    output$exp_col_max_yr_usd_tt <- renderText(exp_col_max_yr_usd_tt())
    output$exp_col_max_yr_usd <- render_d3po({
      exp_col_max_yr_usd()
    })

    output$exp_tt_min_yr <- renderText(exp_tt_min_yr())
    output$exp_tm_dtl_min_yr <- render_d3po({
      exp_tm_dtl_min_yr()
    })
    output$exp_tt_max_yr <- renderText(exp_tt_max_yr())
    output$exp_tm_dtl_max_yr <- render_d3po({
      exp_tm_dtl_max_yr()
    })

    ### Imports ----

    output$imp_tt_yr <- renderText(imp_tt_yr())

    # Import column chart outputs
    output$imp_col_min_yr_usd_tt <- renderText(imp_col_min_yr_usd_tt())
    output$imp_col_min_yr_usd <- render_d3po({
      imp_col_min_yr_usd()
    })

    output$imp_col_max_yr_usd_tt <- renderText(imp_col_max_yr_usd_tt())
    output$imp_col_max_yr_usd <- render_d3po({
      imp_col_max_yr_usd()
    })

    output$imp_tt_min_yr <- renderText(imp_tt_min_yr())
    output$imp_tm_dtl_min_yr <- render_d3po({
      imp_tm_dtl_min_yr()
    })
    output$imp_tt_max_yr <- renderText(imp_tt_max_yr())
    output$imp_tm_dtl_max_yr <- render_d3po({
      imp_tm_dtl_max_yr()
    })

    ## Download ----

    output$dwn_dtl_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_detailed.{ inp_fmt() }")
      },
      content = function(filename) {
        export(df_dtl(), filename)
      }
    )

    output$dwn_agg_pre <- downloadHandler(
      filename = function() {
        glue("{ inp_r() }_{ inp_p() }_{ min(inp_y()) }_{ max(inp_y()) }_aggregated.{ inp_fmt() }")
      },
      content = function(filename) {
        export(df_agg(), filename)
      }
    )

    output$dwn_stl <- renderText({
      dwn_stl()
    })
    output$dwn_txt <- renderText({
      dwn_txt()
    })
    output$dwn_fmt <- renderUI({
      dwn_fmt()
    })

    output$dwn_dtl <- renderUI({
      req(input$go)
      downloadButton(ns("dwn_dtl_pre"), label = "Detailed data")
    })

    output$dwn_agg <- renderUI({
      req(input$go)
      downloadButton(ns("dwn_agg_pre"), label = "Aggregated data")
    })

    # Hide boxes until viz is ready ----

    ## observe the button being pressed
    observeEvent(input$go, {
      if (input$go > 0) {
        show(id = "title_section")
        show(id = "aggregated_trade")
        show(id = "detailed_trade_exp")
        show(id = "detailed_trade_imp")
        show(id = "download_data")
      }
    })
  })
}
