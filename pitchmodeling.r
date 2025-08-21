# modules/mod_stuffplus.R - Two Player Comparison Version (FIXED)
# =====================================================================
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(ggforce)
library(httr)
library(jsonlite)
library(glue)
library(purrr)
library(DBI)
library(RPostgres)

# ----------  UI -------------------------------------------------------
stuffPlusUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Professional CSS styling
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');

        body {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
          color: #1a1a1a;
          background-color: #f1f5f9;
        }

        .main-container {
          max-width: 1600px;
          margin: 0 auto;
          padding: 20px;
        }

        .header-section {
          background: linear-gradient(135deg, #000 0%, #1a1a1a 100%);
          padding: 32px;
          margin-bottom: 20px;
          border: none;
          border-radius: 0;
          box-shadow: none;
          display: flex;
          flex-direction: row;
          align-items: center;
          gap: 20px;
        }

        .page-title {
          font-size: 28px;
          font-weight: 600;
          letter-spacing: 0.5px;
          margin: 0;
          color: #FFDE00;
        }


        .comparison-container {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(0, 1fr));
          gap: 20px;
        }

        .mode-toggle {
          display: flex;
          gap: 8px;
          margin-left: 8px;
        }

        .mode-toggle .btn {
          flex: none;
          background: none !important;
          color: #fff;
          border: none;
          border-radius: 0;
          box-shadow: none !important;
          padding: 2px 6px;
          transition: color 0.2s ease, border-color 0.2s ease;
          cursor: pointer;
        }
        .mode-toggle .btn:hover,
        .mode-toggle .btn:focus {
          color: #6b7280;
          background: none !important;
          box-shadow: none !important;
        }
        .mode-toggle .btn.active,
        .mode-toggle .btn.active:hover,
        .mode-toggle .btn.active:focus {
          color: #FFDE00;
          background: none !important;
          box-shadow: none !important;
          border-bottom: 2px solid #FFDE00;
        }

        .filter-header {
          display: flex;
          gap: 8px;
          margin-bottom: 16px;
          background: #f8f9fa;
          padding: 12px;
          font-weight: 600;
          border: 1px solid #e5e7eb;
          box-shadow: 0 1px 2px rgba(0,0,0,0.05);
        }

        .filter-header .btn {
          flex: none;
          background: none !important;
          color: #4b5563;
          border: none;
          border-radius: 0;
          box-shadow: none !important;
          padding: 2px 6px;
          transition: color 0.2s ease, border-color 0.2s ease;
          cursor: pointer;
        }
        .filter-header .btn:hover,
        .filter-header .btn:focus {
          color: #6b7280;
          background: none !important;
          box-shadow: none !important;
        }
        .filter-header .btn.active,
        .filter-header .btn.active:hover,
        .filter-header .btn.active:focus {
          color: #FFDE00;
          background: none !important;
          box-shadow: none !important;
          border-bottom: 2px solid #FFDE00;
        }

        .player-panel {
          background: #fff;
          border: 1px solid #e5e7eb;
          border-radius: 0;
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
          padding: 24px;
          overflow: visible;
          display: flex;
          flex-direction: column;
          transition: box-shadow 0.2s ease;
        }

        .player-panel:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.08);
        }

        .filters {
          padding: 16px;
          border-bottom: 1px solid #e5e7eb;
        }

        .filter-section {
          margin-bottom: 20px;
          background: #fafbfc;
          border: 1px solid #e5e7eb;
          padding: 20px;
          transition: box-shadow 0.2s ease;
        }

        .filter-section:hover {
          box-shadow: 0 2px 6px rgba(0,0,0,0.04);
        }

        .filter-title {
          font-size: 12px;
          font-weight: 600;
          text-transform: uppercase;
          color: #888;
          margin-bottom: 8px;
          letter-spacing: 0.5px;
        }

        .filter-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
          gap: 12px;
        }

        .year-filter-group {
          display: flex;
          align-items: center;
          gap: 6px;
        }

        .selectize-dropdown {
          z-index: 9999 !important;
        }

        .filter-item {
          display: flex;
          flex-direction: column;
        }

        .filter-label {
          font-size: 10px;
          font-weight: 600;
          color: #666;
          text-transform: uppercase;
          margin-bottom: 4px;
          letter-spacing: 0.5px;
        }

        .selectize-input,
        .picker-input .btn {
          border: 2px solid #ccc !important;
          border-radius: 4px !important;
          padding: 6px 12px !important;
          min-height: 36px !important;
          font-size: 13px !important;
          background: #fff !important;
          box-shadow: 0 1px 2px rgba(0,0,0,0.05) !important;
          transition: border-color 0.2s ease, box-shadow 0.2s ease;
        }

        .selectize-input:hover,
        .picker-input .btn:hover {
          border-color: #6b7280 !important;
        }

        .selectize-input.focus,
        .picker-input .btn:focus {
          border-color: #4b5563 !important;
          box-shadow: 0 0 0 2px rgba(75,85,99,0.25) !important;
        }

        .player-content {
          padding: 20px;
        }

        .results-header {
          display: flex;
          justify-content: space-between;
          align-items: baseline;
          margin-bottom: 16px;
          padding-bottom: 12px;
          border-bottom: 1px solid #e5e5e5;
        }

        .player-name {
          font-size: 20px;
          font-weight: 600;
          margin: 0;
        }

        .pitch-count {
          font-size: 13px;
          color: #666;
          font-weight: 500;
        }

        .section-title {
          font-size: 18px;
          font-weight: 700;
          color: #111;
          margin: 0 0 12px 0;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }

        .data-table-container {
          margin-top: 8px;
          overflow-x: auto;
        }

        .plot-row {
          display: flex;
          gap: 10px;
        }

        .stuffplus-plot-wrapper,
        .breaks-plot-wrapper,
        .usage-plot-wrapper {
          width: 33.33%;
          margin-bottom: 8px;
        }

        table.dataTable {
          font-size: 9px;
          border-collapse: collapse;
          width: 100%;
          table-layout: auto;
        }

        table.dataTable thead th {
          background-color: transparent;
          border-bottom: 2px solid #e5e5e5;
          font-weight: 600;
          color: #666;
          padding: 4px 2px;
          text-align: center;
          font-size: 8px;
          text-transform: uppercase;
          letter-spacing: 0.3px;
        }

        table.dataTable tbody td {
          border-bottom: 1px solid #f0f0f0;
          padding: 3px 1px;
          text-align: center;
          color: #1a1a1a;
          font-size: 9px;
          white-space: normal;
          word-wrap: break-word;
        }

        table.dataTable tbody tr {
          transition: background-color 0.2s ease;
        }

        table.dataTable tbody tr:hover {
          background-color: #fafafa;
        }

        table.dataTable tbody tr:last-child td {
          border-bottom: none;
        }

        .no-results {
          text-align: center;
          padding: 60px 20px;
          color: #666;
        }

        .no-results h3 {
          font-size: 16px;
          font-weight: 600;
          margin-bottom: 8px;
          color: #1a1a1a;
        }

        .no-results p {
          font-size: 13px;
        }

        .dataTables_wrapper .dataTables_length,
        .dataTables_wrapper .dataTables_filter,
        .dataTables_wrapper .dataTables_info,
        .dataTables_wrapper .dataTables_paginate {
          display: none;
        }

        @media (max-width: 1200px) {
          .comparison-container {
            grid-template-columns: 1fr;
          }

          .player-panel:first-child {
            margin-bottom: 20px;
          }
        }
        "))
    ),
    
    div(class = "main-container",
        # Header
        div(class = "header-section",
            h1("P3 Pitch Modeling", class = "page-title"),
            div(class = "mode-toggle",
                radioGroupButtons(
                  inputId = ns("player_mode"),
                  label = NULL,
                  choices = c("Single Athlete" = "single", "Two Athletes" = "two"),
                  selected = "two",
                  justified = FALSE,
                  individual = TRUE,
                  status = "default"
                )
            )
        ),
        
        # Two-player comparison layout
        div(class = "comparison-container",
            # Player 1 Panel
            div(class = "player-panel",
                div(class = "filters",
                    div(class = "filter-header",
                        radioGroupButtons(
                          inputId = ns("filter_toggle1"),
                          label = NULL,
                          choices = c("MLB", "P3"),
                          selected = "MLB",
                          justified = FALSE,
                          individual = TRUE,
                          status = "default"
                        )
                    ),
                    selectizeInput(ns("player1_search"), label = NULL,
                                   choices = NULL,
                                   options = list(
                                     placeholder = "Search pitcher...",
                                     maxOptions = 1000
                                   )),
                    conditionalPanel(
                      condition = sprintf("input['%s'] === 'MLB'", ns('filter_toggle1')),
                      div(class = "filter-section",
                          span("Season Analysis", class = "filter-title"),
                          div(class = "filter-grid",
                              div(class = "filter-item",
                                  span(class = "filter-label", "Season Pitch Metrics:"),
                                  uiOutput(ns("year_filter_ui1"))
                              ),
                          )
                      ),
                      div(class = "filter-section",
                          span("Game Analysis", class = "filter-title"),
                          div(class = "filter-grid",
                              div(class = "filter-item",
                                  span(class = "filter-label", "Game Pitch Metrics:"),
                                  uiOutput(ns("date_filter_ui1"))
                              )
                          )
                      ),
                      div(class = "filter-section",
                          span("Performance Logs", class = "filter-title"),
                          div(class = "filter-grid",
                              div(class = "filter-item",
                                  span(class = "filter-label", "Game Logs:"),
                                  uiOutput(ns("logs_year_filter_ui1"))
                              ),
                              div(class = "filter-item",
                                  span(class = "filter-label", "Logs Range:"),
                                  uiOutput(ns("logs_range_filter_ui1"))
                              ),
                              div(class = "filter-item",
                                  span(class = "filter-label", "Season Stats:"),
                                  uiOutput(ns("stats_year_filter_ui1"))
                              )
                          )
                      )
                    ),
                    conditionalPanel(
                      condition = sprintf("input['%s'] === 'P3'", ns('filter_toggle1')),
                      div(class = "filter-section",
                          span("Date", class = "filter-title"),
                          div(class = "filter-grid",
                              div(class = "filter-item",
                                  span(class = "filter-label", "Date:"),
                                  uiOutput(ns("p3_date_picker_ui1"))
                              )
                          )
                      ),
                      div(class = "filter-section",
                          span("Pitch Filter", class = "filter-title"),
                          div(class = "filter-grid",
                              div(class = "filter-item",
                                  span(class = "filter-label", "Pitches:"),
                                  uiOutput(ns("p3_pitch_type_ui1"))
                              )
                          )
                      )
                    )
                ),
                div(class = "player-content",
                    uiOutput(ns("player1_content"))
                )
            ),
            
            # Player 2 Panel
            conditionalPanel(
              condition = sprintf("input['%s'] === 'two'", ns('player_mode')),
              div(class = "player-panel",
                  div(class = "filters",
                      div(class = "filter-header",
                          radioGroupButtons(
                            inputId = ns("filter_toggle2"),
                            label = NULL,
                            choices = c("MLB", "P3"),
                            selected = "MLB",
                            justified = FALSE,
                            individual = TRUE,
                            status = "default"
                          )
                      ),
                      selectizeInput(ns("player2_search"), label = NULL,
                                     choices = NULL,
                                     options = list(
                                       placeholder = "Search pitcher...",
                                       maxOptions = 1000
                                     )),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'MLB'", ns('filter_toggle2')),
                        div(class = "filter-section",
                            span("Season Analysis", class = "filter-title"),
                            div(class = "filter-grid",
                                div(class = "filter-item",
                                    span(class = "filter-label", "Season Pitch Metrics:"),
                                    uiOutput(ns("year_filter_ui2"))
                                ),
                            )
                        ),
                        div(class = "filter-section",
                            span("Game Analysis", class = "filter-title"),
                            div(class = "filter-grid",
                                div(class = "filter-item",
                                    span(class = "filter-label", "Game Pitch Metrics:"),
                                    uiOutput(ns("date_filter_ui2"))
                                )
                            )
                        ),
                        div(class = "filter-section",
                            span("Performance Logs", class = "filter-title"),
                            div(class = "filter-grid",
                                div(class = "filter-item",
                                    span(class = "filter-label", "Game Logs:"),
                                    uiOutput(ns("logs_year_filter_ui2"))
                                ),
                                div(class = "filter-item",
                                    span(class = "filter-label", "Logs Range:"),
                                    uiOutput(ns("logs_range_filter_ui2"))
                                ),
                                div(class = "filter-item",
                                    span(class = "filter-label", "Season Stats:"),
                                    uiOutput(ns("stats_year_filter_ui2"))
                                )
                            )
                        )
                      ),
                      conditionalPanel(
                        condition = sprintf("input['%s'] === 'P3'", ns('filter_toggle2')),
                        div(class = "filter-section",
                            span("Date", class = "filter-title"),
                            div(class = "filter-grid",
                                div(class = "filter-item",
                                    span(class = "filter-label", "Date:"),
                                    uiOutput(ns("p3_date_picker_ui2"))
                                )
                            )
                        ),
                        div(class = "filter-section",
                            span("Pitch Filter", class = "filter-title"),
                            div(class = "filter-grid",
                                div(class = "filter-item",
                                    span(class = "filter-label", "Pitches:"),
                                    uiOutput(ns("p3_pitch_type_ui2"))
                                )
                            )
                        )
                      )
                  ),
                  div(class = "player-content",
                      uiOutput(ns("player2_content"))
                  )
              )
            )
        )
    )
  )
}

# ----------  Server ---------------------------------------------------
stuffPlusServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    library(googledrive)
    ## put this once, right after library(googledrive)
    ## one-time helper (put near the top of the module)
    # ---- Database connection ----------------------------------------
    db_host <- "pitch-modeling.cvoesgo8szjk.us-east-2.rds.amazonaws.com"
    db_port <- 5432
    db_name <- "postgres"
    db_user <- "premier3"
    db_pass <- "premier3"

    con <- dbConnect(
      RPostgres::Postgres(),
      host = db_host, port = db_port, dbname = db_name,
      user = db_user, password = db_pass, sslmode = "require"
    )

    params <- dbReadTable(con, "stuff_plus_scaling") %>%
      mutate(year = as.integer(year))

    swing_code <- c('foul_bunt', 'foul', 'hit_into_play', 'swinging_strike', 'foul_tip',
                    'swinging_strike_blocked', 'missed_bunt', 'bunt_foul_tip')
    whiff_code <- c('swinging_strike', 'foul_tip', 'swinging_strike_blocked')

    # ---- API helper functions --------------------------------------
    get_pitcher_game_logs_api <- function(player_id, season = 2025) {
      url <- glue(
        "https://statsapi.mlb.com/api/v1/people/{player_id}/stats",
        "?stats=gameLog&group=pitching&season={season}&language=en"
      )
      tryCatch({
        resp <- GET(url)
        parsed <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
        if ("stats" %in% names(parsed) && length(parsed$stats) > 0) {
          if ("splits" %in% names(parsed$stats) && length(parsed$stats$splits) > 0) {
            df <- parsed$stats$splits[[1]]
            df$player_id <- player_id
            return(df)
          }
        }
        NULL
      }, error = function(e) {
        message("Error fetching logs: ", e$message)
        NULL
      })
    }
    
    get_pitcher_season_stats_api <- function(player_id, season = 2025) {
      url <- glue(
        "https://statsapi.mlb.com/api/v1/people/{player_id}/stats",
        "?stats=season&group=pitching&season={season}&language=en"
      )
      tryCatch({
        resp <- GET(url)
        parsed <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
        if ("stats" %in% names(parsed) && length(parsed$stats) > 0) {
          if ("splits" %in% names(parsed$stats) && length(parsed$stats$splits) > 0) {
            df <- parsed$stats$splits[[1]]
            df$player_id <- player_id
            return(df)
          }
        }
        NULL
      }, error = function(e) {
        message("Error fetching season stats: ", e$message)
        NULL
      })
    }
    
    all_pitches <- dbReadTable(con, "stuff_plus_predictions") %>%
      mutate(year = as.integer(year)) %>%
      left_join(params, by = "year") %>%
      mutate(
        stuff_plus = 100 - 10 * (predicted_target - mean_predicted_target) / sd_predicted_target,
        swing = description %in% swing_code,
        whiff = description %in% whiff_code,
        in_zone = zone < 10,
        out_zone = zone > 10,
        chase = zone > 10 & swing == TRUE,
        pfx_z = pfx_z * 12,
        pfx_x = pfx_x * 12,
        pitch_type = as.factor(pitch_type),
        name_parts = strsplit(player_name, ", "),
        formatted_name = sapply(name_parts, function(x) {
          if (length(x) == 2) paste(x[2], x[1]) else x[1]
        }),
        game_date_formatted = format(as.Date(game_date), "%b %d, %Y")
      ) %>%
      select(-mean_predicted_target, -sd_predicted_target, -n_pitches, -name_parts)

    mlb_players <- all_pitches %>%
      distinct(pitcher, formatted_name) %>%
      arrange(formatted_name)
    mlb_choices <- setNames(as.character(mlb_players$pitcher), mlb_players$formatted_name)

    p3_data_all <- dbReadTable(con, "P3_Predictions") %>%
      mutate(
        pitch_id = row_number(),
        date = as.Date(date),
        formatted_name = paste(firstname, lastname),
        game_date = date,
        game_date_formatted = format(date, "%b %d, %Y"),
        year = as.integer(format(date, "%Y")),
        release_spin_rate = as.numeric(release_spin_rate),
        release_extension = as.numeric(release_extension),
        pfx_x = as.numeric(pfx_x_inches),
        pfx_z = as.numeric(pfx_z_inches),
        plate_loc_side = as.numeric(plate_loc_side) * 12,
        plate_loc_height = as.numeric(plate_loc_height) * 12,
        in_zone = 0,
        out_zone = 0,
        chase = 0,
        swing = 0,
        whiff = 0
      )

    p3_players <- p3_data_all %>%
      distinct(pitcher_id, formatted_name) %>%
      arrange(formatted_name)
    p3_choices <- setNames(p3_players$pitcher_id, p3_players$formatted_name)

    dbDisconnect(con)
    
    # ---- 2. Reactive values for both players -------------------------
    player1_data <- reactiveVal(NULL)
    player2_data <- reactiveVal(NULL)
    
    # ---- 3. Search functionality for both players --------------------
    # Player 1
    updateSelectizeInput(session, "player1_search",
                         choices = mlb_choices,
                         server = TRUE)

    observeEvent(input$filter_toggle1, {
      choices <- if (input$filter_toggle1 == "MLB") mlb_choices else p3_choices
      updateSelectizeInput(session, "player1_search",
                           choices = choices,
                           selected = "",
                           server = TRUE)
      player1_data(NULL)
    })

    observeEvent(input$player1_search, {
      req(input$player1_search)
      if (input$filter_toggle1 == "MLB") {
        data <- all_pitches %>%
          filter(pitcher == as.numeric(input$player1_search))
      } else {
        data <- p3_data_all %>%
          filter(pitcher_id == input$player1_search)
      }

      if (nrow(data) > 0) {
        player1_data(data)
      } else {
        player1_data(NULL)
      }
    })
    
    # Player 2
    updateSelectizeInput(session, "player2_search",
                         choices = mlb_choices,
                         server = TRUE)

    observeEvent(input$filter_toggle2, {
      choices <- if (input$filter_toggle2 == "MLB") mlb_choices else p3_choices
      updateSelectizeInput(session, "player2_search",
                           choices = choices,
                           selected = "",
                           server = TRUE)
      player2_data(NULL)
    })

    observeEvent(input$player2_search, {
      req(input$player2_search)
      if (input$filter_toggle2 == "MLB") {
        data <- all_pitches %>%
          filter(pitcher == as.numeric(input$player2_search))
      } else {
        data <- p3_data_all %>%
          filter(pitcher_id == input$player2_search)
      }

      if (nrow(data) > 0) {
        player2_data(data)
      } else {
        player2_data(NULL)
      }
    })
    
    # ---- 4. Player 1 UI outputs --------------------------------------
    output$player1_content <- renderUI({
      ns <- session$ns
      
      if (is.null(player1_data())) {
        if (!is.null(input$player1_search) && nchar(input$player1_search) > 0) {
          div(class = "no-results",
              h3("No results found"),
              p("Try searching with a different name.")
          )
        } else {
          div(class = "no-results",
              h3("Select Player 1"),
              p("Enter a pitcher's name in the search box above.")
          )
        }
      } else {
        data <- player1_data()
        player_name <- unique(data$formatted_name)[1]
        
        if (input$filter_toggle1 == "MLB") {
          tagList(
            div(class = "results-header",
                h2(player_name, class = "player-name"),
                span(class = "pitch-count",
                     textOutput(ns("pitch_count1"), inline = TRUE))
            ),
            uiOutput(ns("season_summary_ui1")),
            uiOutput(ns("game_summary_ui1")),
            uiOutput(ns("game_logs_ui1")),
            uiOutput(ns("season_stats_ui1"))
          )
        } else {
          tagList(
            div(class = "results-header",
                h2(player_name, class = "player-name"),
                span(class = "pitch-count",
                     textOutput(ns("p3_pitch_count1"), inline = TRUE))
            ),
            uiOutput(ns("p3_summary_ui1"))
          )
        }
      }
    })
    
    # ---- 5. Player 2 UI outputs --------------------------------------
    output$player2_content <- renderUI({
      ns <- session$ns
      
      if (is.null(player2_data())) {
        if (!is.null(input$player2_search) && nchar(input$player2_search) > 0) {
          div(class = "no-results",
              h3("No results found"),
              p("Try searching with a different name.")
          )
        } else {
          div(class = "no-results",
              h3("Select Player 2"),
              p("Enter a pitcher's name in the search box above.")
          )
        }
      } else {
        data <- player2_data()
        player_name <- unique(data$formatted_name)[1]
        
        if (input$filter_toggle2 == "MLB") {
          tagList(
            div(class = "results-header",
                h2(player_name, class = "player-name"),
                span(class = "pitch-count",
                     textOutput(ns("pitch_count2"), inline = TRUE))
            ),
            uiOutput(ns("season_summary_ui2")),
            uiOutput(ns("game_summary_ui2")),
            uiOutput(ns("game_logs_ui2")),
            uiOutput(ns("season_stats_ui2"))
          )
        } else {
          tagList(
            div(class = "results-header",
                h2(player_name, class = "player-name"),
                span(class = "pitch-count",
                     textOutput(ns("p3_pitch_count2"), inline = TRUE))
            ),
            uiOutput(ns("p3_summary_ui2"))
          )
        }
      }
    })
    
    # ---- 6. Filter UIs for Player 1 ---------------------------------
    output$year_filter_ui1 <- renderUI({
      req(player1_data())
      ns <- session$ns
      
      years <- sort(unique(player1_data()$year))
      
      div(class = "year-filter-group",
          pickerInput(
            inputId = ns("year_filter1"),
            label = NULL,
            choices = years,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 2",
              `count-selected-text` = "{0} seasons",
              size = 10
            )
          ),
          checkboxInput(ns("season_split1"), label = "Summary per season", value = FALSE)
      )
    })
    
    output$stats_year_filter_ui1 <- renderUI({
      req(player1_data())
      ns <- session$ns
      
      years <- sort(unique(player1_data()$year))
      
      pickerInput(
        inputId = ns("stats_year_filter1"),
        label = NULL,
        choices = years,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          size = 6
        )
      )
    })
    
    output$logs_year_filter_ui1 <- renderUI({
      req(player1_data())
      ns <- session$ns
      
      years <- sort(unique(player1_data()$year))
      
      pickerInput(
        inputId = ns("logs_year_filter1"),
        label = NULL,
        choices = years,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          size = 6
        )
      )
    })
    
    output$logs_range_filter_ui1 <- renderUI({
      ns <- session$ns
      selectInput(ns("logs_range1"), label = NULL,
                  choices = c("All Games" = 0,
                              "Last 10 Games" = 10,
                              "Last 20 Games" = 20),
                  selected = 0)
    })
    
    output$date_filter_ui1 <- renderUI({
      req(player1_data())
      ns <- session$ns
      
      dates <- player1_data() %>%
        select(game_date, game_date_formatted) %>%
        distinct() %>%
        arrange(desc(game_date))
      
      pickerInput(
        inputId = ns("date_filter1"),
        label = NULL,
        choices = setNames(dates$game_date, dates$game_date_formatted),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} games",
          size = 10,
          `live-search` = TRUE,
          `none-selected-text` = "Select games"
        )
      )
    })
    
    output$p3_date_picker_ui1 <- renderUI({
      req(player1_data())
      ns <- session$ns
      dates <- player1_data() %>%
        select(date) %>%
        distinct() %>%
        arrange(desc(date))
      
      pickerInput(
        inputId = ns("p3_dates1"),
        label = NULL,
        choices = setNames(dates$date, format(dates$date, "%b %d, %Y")),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} games",
          size = 10,
          `live-search` = TRUE,
          `none-selected-text` = "Select games"
        )
      )
    })
    
    output$p3_pitch_type_ui1 <- renderUI({
      req(player1_data())
      req(input$p3_dates1)
      ns <- session$ns
      df <- player1_data() %>%
        filter(as.Date(date) %in% as.Date(input$p3_dates1)) %>%
        mutate(label = paste0(row_number(), "# ", pitch_type, " ",
                              round(as.numeric(release_speed), 1)))
      pickerInput(
        inputId = ns("p3_pitch_ids1"),
        label = NULL,
        choices = setNames(df$pitch_id, df$label),
        selected = df$pitch_id,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, size = 10)
      )
    })
    
    # ---- 7. Filter UIs for Player 2 ---------------------------------
    output$year_filter_ui2 <- renderUI({
      req(player2_data())
      ns <- session$ns
      
      years <- sort(unique(player2_data()$year))
      
      div(class = "year-filter-group",
          pickerInput(
            inputId = ns("year_filter2"),
            label = NULL,
            choices = years,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `selected-text-format` = "count > 2",
              `count-selected-text` = "{0} seasons",
              size = 10
            )
          ),
          checkboxInput(ns("season_split2"), label = "Summary per season", value = FALSE)
      )
    })
    
    output$date_filter_ui2 <- renderUI({
      req(player2_data())
      ns <- session$ns
      
      dates <- player2_data() %>%
        select(game_date, game_date_formatted) %>%
        distinct() %>%
        arrange(desc(game_date))
      
      pickerInput(
        inputId = ns("date_filter2"),
        label = NULL,
        choices = setNames(dates$game_date, dates$game_date_formatted),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} games",
          size = 10,
          `live-search` = TRUE,
          `none-selected-text` = "Select games"
        )
      )
    })
    
    output$p3_date_picker_ui2 <- renderUI({
      req(player2_data())
      ns <- session$ns
      dates <- player2_data() %>%
        select(date) %>%
        distinct() %>%
        arrange(desc(date))
      
      pickerInput(
        inputId = ns("p3_dates2"),
        label = NULL,
        choices = setNames(dates$date, format(dates$date, "%b %d, %Y")),
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = FALSE,
          `selected-text-format` = "count > 2",
          `count-selected-text` = "{0} games",
          size = 10,
          `live-search` = TRUE,
          `none-selected-text` = "Select games"
        )
      )
    })
    
    output$p3_pitch_type_ui2 <- renderUI({
      req(player2_data())
      req(input$p3_dates2)
      ns <- session$ns
      df <- player2_data() %>%
        filter(as.Date(date) %in% as.Date(input$p3_dates2)) %>%
        mutate(label = paste0(row_number(), "# ", pitch_type, " ",
                              round(as.numeric(release_speed), 1)))
      pickerInput(
        inputId = ns("p3_pitch_ids2"),
        label = NULL,
        choices = setNames(df$pitch_id, df$label),
        selected = df$pitch_id,
        multiple = TRUE,
        options = list(`actions-box` = TRUE, size = 10)
      )
    })
    
    
    output$stats_year_filter_ui2 <- renderUI({
      req(player2_data())
      ns <- session$ns
      
      years <- sort(unique(player2_data()$year))
      
      pickerInput(
        inputId = ns("stats_year_filter2"),
        label = NULL,
        choices = years,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          size = 6
        )
      )
    })
    
    output$logs_year_filter_ui2 <- renderUI({
      req(player2_data())
      ns <- session$ns
      
      years <- sort(unique(player2_data()$year))
      
      pickerInput(
        inputId = ns("logs_year_filter2"),
        label = NULL,
        choices = years,
        selected = NULL,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          size = 6
        )
      )
    })
    
    output$logs_range_filter_ui2 <- renderUI({
      ns <- session$ns
      selectInput(ns("logs_range2"), label = NULL,
                  choices = c("All Games" = 0,
                              "Last 10 Games" = 10,
                              "Last 20 Games" = 20),
                  selected = 0)
    })
    
    # ---- 8. Data helpers for both players ----------------------------
    get_season_data1 <- reactive({
      req(player1_data())
      data <- player1_data()
      
      if (!is.null(input$year_filter1) && length(input$year_filter1) > 0) {
        data <- data %>% filter(year %in% input$year_filter1)
      } else {
        data <- data[0, ]
      }
      data
    })
    
    get_season_data2 <- reactive({
      req(player2_data())
      data <- player2_data()
      
      if (!is.null(input$year_filter2) && length(input$year_filter2) > 0) {
        data <- data %>% filter(year %in% input$year_filter2)
      } else {
        data <- data[0, ]
      }
      data
    })
    
    get_game_data1 <- reactive({
      data <- player1_data()
      if (!is.null(input$date_filter1) && length(input$date_filter1) > 0) {
        data <- data %>% filter(game_date %in% input$date_filter1)
      } else {
        data <- NULL
      }
      data
    })
    
    get_game_data2 <- reactive({
      data <- player2_data()
      if (!is.null(input$date_filter2) && length(input$date_filter2) > 0) {
        data <- data %>% filter(game_date %in% input$date_filter2)
      } else {
        data <- NULL
      }
      data
    })
    
    get_p3_filtered_data1 <- reactive({
      req(player1_data())
      data <- player1_data()
      if (!is.null(input$p3_dates1) && length(input$p3_dates1) > 0) {
        data <- data %>% filter(as.Date(date) %in% as.Date(input$p3_dates1))
      }
      if (!is.null(input$p3_pitch_ids1) && length(input$p3_pitch_ids1) > 0) {
        data <- data %>% filter(pitch_id %in% input$p3_pitch_ids1)
      }
      data
    })
    
    get_p3_filtered_data2 <- reactive({
      req(player2_data())
      data <- player2_data()
      if (!is.null(input$p3_dates2) && length(input$p3_dates2) > 0) {
        data <- data %>% filter(as.Date(date) %in% as.Date(input$p3_dates2))
      }
      if (!is.null(input$p3_pitch_ids2) && length(input$p3_pitch_ids2) > 0) {
        data <- data %>% filter(pitch_id %in% input$p3_pitch_ids2)
      }
      data
    })
    
    game_logs_data1 <- reactive({
      req(player1_data())
      if (is.null(input$logs_year_filter1) || length(input$logs_year_filter1) == 0) {
        return(NULL)
      }
      player_id <- unique(player1_data()$pitcher)[1]
      df <- bind_rows(lapply(input$logs_year_filter1,
                             function(y) get_pitcher_game_logs_api(player_id, y)))
      if (!is.null(df) && !is.null(input$logs_range1) && as.numeric(input$logs_range1) > 0) {
        date_col <- df[[intersect(c("gameDate", "date", "Date"), names(df))[1]]]
        df <- df %>% arrange(desc(as.Date(date_col))) %>% head(as.numeric(input$logs_range1))
      }
      df
    })
    
    game_logs_data2 <- reactive({
      req(player2_data())
      if (is.null(input$logs_year_filter2) || length(input$logs_year_filter2) == 0) {
        return(NULL)
      }
      player_id <- unique(player2_data()$pitcher)[1]
      df <- bind_rows(lapply(input$logs_year_filter2,
                             function(y) get_pitcher_game_logs_api(player_id, y)))
      if (!is.null(df) && !is.null(input$logs_range2) && as.numeric(input$logs_range2) > 0) {
        date_col <- df[[intersect(c("gameDate", "date", "Date"), names(df))[1]]]
        df <- df %>% arrange(desc(as.Date(date_col))) %>% head(as.numeric(input$logs_range2))
      }
      df
    })
    
    season_stats_data1 <- reactive({
      req(player1_data())
      if (is.null(input$stats_year_filter1) || length(input$stats_year_filter1) == 0) {
        return(NULL)
      }
      player_id <- unique(player1_data()$pitcher)[1]
      bind_rows(lapply(input$stats_year_filter1,
                       function(y) get_pitcher_season_stats_api(player_id, y)))
    })
    
    season_stats_data2 <- reactive({
      req(player2_data())
      if (is.null(input$stats_year_filter2) || length(input$stats_year_filter2) == 0) {
        return(NULL)
      }
      player_id <- unique(player2_data()$pitcher)[1]
      bind_rows(lapply(input$stats_year_filter2,
                       function(y) get_pitcher_season_stats_api(player_id, y)))
    })
    
    # ---- 9. Pitch counts ---------------------------------------------
    output$pitch_count1 <- renderText({
      data <- get_season_data1()
      paste(format(nrow(data), big.mark = ","), "pitches")
    })
    
    output$pitch_count2 <- renderText({
      data <- get_season_data2()
      paste(format(nrow(data), big.mark = ","), "pitches")
    })
    
    output$p3_pitch_count1 <- renderText({
      data <- get_p3_filtered_data1()
      paste(format(nrow(data), big.mark = ","), "pitches")
    })
    
    output$p3_pitch_count2 <- renderText({
      data <- get_p3_filtered_data2()
      paste(format(nrow(data), big.mark = ","), "pitches")
    })
    
    # ---- 10. Summary function ----------------------------------------
    summarize_player_data <- function(data) {
      data %>%
        group_by(pitch_type) %>%
        summarise(
          Count = n(),
          `Velo` = round(mean(release_speed, na.rm = TRUE), 1),
          `iVB` = round(mean(pfx_z, na.rm = TRUE), 1),
          `HB` = round(mean(pfx_x, na.rm = TRUE), 1),
          `Spin` = round(mean(release_spin_rate, na.rm = TRUE), 0),
          `vRel` = round(mean(release_pos_z, na.rm = TRUE), 1),
          `hRel` = round(mean(release_pos_x, na.rm = TRUE), 1),
          `Ext` = round(mean(release_extension, na.rm = TRUE), 1),
          `Stuff+` = round(mean(stuff_plus, na.rm = TRUE), 0),
          `Zone%` = round(mean(in_zone, na.rm = TRUE) * 100, 1),
          `Chase%` = ifelse(sum(out_zone, na.rm = TRUE) > 0,
                            round(sum(chase, na.rm = TRUE) / sum(out_zone, na.rm = TRUE) * 100, 1),
                            0),
          `Whiff%` = ifelse(sum(swing, na.rm = TRUE) > 0,
                            round(sum(whiff, na.rm = TRUE) / sum(swing, na.rm = TRUE) * 100, 1),
                            0),
          .groups = 'drop'
        ) %>%
        arrange(desc(Count)) %>%
        bind_rows(
          data %>%
            summarise(
              pitch_type = "All",
              Count = n(),
              `Velo` = NA,
              `iVB` = NA,
              `HB` = NA,
              `Spin` = NA,
              `vRel` = NA,
              `hRel` = NA,
              `Ext` = round(mean(release_extension, na.rm = TRUE), 1),
              `Stuff+` = round(mean(stuff_plus, na.rm = TRUE), 0),
              `Zone%` = round(mean(in_zone, na.rm = TRUE) * 100, 1),
              `Chase%` = ifelse(sum(out_zone, na.rm = TRUE) > 0,
                                round(sum(chase, na.rm = TRUE) / sum(out_zone, na.rm = TRUE) * 100, 1),
                                0),
              `Whiff%` = ifelse(sum(swing, na.rm = TRUE) > 0,
                                round(sum(whiff, na.rm = TRUE) / sum(swing, na.rm = TRUE) * 100, 1),
                                0)
            )
        ) %>%
        rename(Type = pitch_type)
    }
    
    # ---- 11. Create compact table function ---------------------------
    create_compact_table <- function(data) {
      summary_data <- summarize_player_data(data)
      
      # Format for display
      summary_data <- summary_data %>%
        mutate(
          Count = format(Count, big.mark = ","),
          across(c(`Velo`, `iVB`, `HB`, `vRel`, `hRel`, `Ext`), 
                 ~ifelse(is.na(.), "-", as.character(.))),
          across(c(`Spin`, `Stuff+`), 
                 ~ifelse(is.na(.), "-", format(., big.mark = ","))),
          across(c(`Zone%`, `Chase%`, `Whiff%`), 
                 ~paste0(., "%"))
        )
      
      datatable(
        summary_data,
        options = list(
          dom = "t",
          ordering = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = "8%", targets = 0),
            list(width = "8%", targets = 1),
            list(width = "7%", targets = c(2:8)),
            list(width = "8%", targets = 9),
            list(width = "9%", targets = c(10:12))
          )
        ),
        rownames = FALSE,
        escape = FALSE
      ) %>%
        formatStyle(
          columns = 1:ncol(summary_data),
          fontSize = '9px'
        ) %>%
        formatStyle(
          "Type",
          fontWeight = styleEqual("All", "bold")
        )
    }
    
    # ---- 11b. Define global pitch color palette ----------------------
    pitch_colors <- c(
      "FF" = "#ff6384",     # Four-seam (red)
      "SI" = "#ff9f40",     # Sinker (orange)
      "FC" = "#ffcc56",     # Cutter (yellow)
      "SL" = "#4bc0c0",     # Slider (cyan)
      "CU" = "#9966ff",     # Curveball (purple)
      "CH" = "#36a2eb",     # Changeup (blue)
      "FS" = "#ff99cc",     # Splitter (pink)
      "KC" = "#99ccff",     # Knuckle curve (light blue)
      "KN" = "#ff6600",     # Knuckleball (dark orange)
      "CS" = "#66ff99",     # Slow curve (light green)
      "EP" = "#cc99ff",     # Eephus (light purple)
      "FO" = "#ffcc99",     # Forkball (light orange)
      "SC" = "#99ffcc",     # Screwball (mint)
      "SV" = "#ff9999"      # Sweeper (light red)
    )
    
    # ---- 11c. Rolling Stuff Plus by pitch function ----------------------
    create_stuffplus_plot <- function(player_df) {
      if (is.null(player_df) || nrow(player_df) == 0) {
        return(ggplot() + 
                 annotate("text", x = 1, y = 1, label = "No data available", 
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = "50 Pitch Rolling Stuff Plus"))
      }
      
      # Check if we have the required columns
      if (!"stuff_plus" %in% names(player_df) || !"pitch_type" %in% names(player_df)) {
        return(ggplot() + 
                 annotate("text", x = 1, y = 1, label = "Missing required data columns", 
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = "50 Pitch Rolling Stuff Plus"))
      }
      
      pitch_order <- player_df %>%
        count(pitch_type, sort = TRUE) %>%
        pull(pitch_type)
      
      # Create the rolling average data
      pitch_means <- player_df %>%
        arrange(game_date, pitch_number) %>%
        mutate(pitch_type = factor(pitch_type, levels = pitch_order)) %>%
        group_by(pitch_type) %>%
        filter(n() >= 50) %>%  # Only include pitch types with 50+ pitches
        mutate(
          pitch_idx = row_number(),
          avg_stuff = stats::filter(
            stuff_plus,
            rep(1 / 50, 50),
            sides = 1
          )
        ) %>%
        ungroup() %>%
        filter(!is.na(avg_stuff))
      
      # Check if any data remains after filtering
      if (nrow(pitch_means) == 0) {
        return(ggplot() + 
                 annotate("text", x = 1, y = 1, label = "Need 50+ pitches per pitch type", 
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = "50 Pitch Rolling Stuff Plus"))
      }
      
      # Create the plot
      p <- ggplot(pitch_means, aes(x = pitch_idx, y = avg_stuff, colour = pitch_type)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 0.8) +
        scale_colour_manual(values = pitch_colors, na.value = "grey50") +
        scale_y_continuous(limits = c(70, 130), 
                           breaks = seq(70, 130, by = 10)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(title = "50 Pitch Rolling Stuff Plus", 
             x = "Pitches", 
             y = "Stuff Plus") +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
          panel.grid.minor = element_line(color = "grey92", linewidth = 0.3),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
      
      return(p)
    }
    
    # ---- 11d. Pitch Breaks scatter plot function ------------------------
    create_pitch_breaks_plot <- function(player_df) {
      if (is.null(player_df) || nrow(player_df) == 0) return(NULL)
      
      # Get pitcher handedness (should be consistent for one player)
      pitcher_handedness <- player_df$p_throws[1]
      
      plot_data <- player_df %>%
        mutate(
          # Adjust horizontal break based on handedness
          # For consistent "Arm Side" (left) and "Glove Side" (right) view
          horizontal_break = case_when(
            p_throws == "R" ~ pfx_x,     # Righties: use as-is
            p_throws == "L" ~ -pfx_x,    # Lefties: flip sign
            TRUE ~ pfx_x
          ),
          vertical_break = pfx_z
        ) %>%
        filter(!is.na(horizontal_break), !is.na(vertical_break))
      
      if (nrow(plot_data) == 0) {
        return(ggplot() + 
                 annotate("text", x = 0, y = 0, label = "No break data available", 
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = "Pitch Breaks"))
      }
      
      ggplot(plot_data, aes(x = horizontal_break, y = vertical_break, color = pitch_type)) +
        geom_point(size = 1.5, alpha = 0.7) +
        geom_hline(yintercept = 0, color = "black", size = 0.5) +
        geom_vline(xintercept = 0, color = "black", size = 0.5) +
        scale_colour_manual(values = pitch_colors, na.value = "grey50") +
        scale_x_continuous(
          limits = c(-25, 25),
          breaks = seq(-20, 20, by = 10),
          labels = c("20", "10", "0", "10", "20")
        ) +
        scale_y_continuous(
          limits = c(-25, 25),
          breaks = seq(-20, 20, by = 10)
        ) +
        # Add single arm side and glove side labels
        annotate("text", x = -15, y = -23, label = "Arm Side", 
                 size = 3, hjust = 0.5, fontface = "bold") +
        annotate("text", x = 15, y = -23, label = "Glove Side", 
                 size = 3, hjust = 0.5, fontface = "bold") +
        labs(
          title = "Pitch Breaks",
          x = "Horizontal Break (in)",
          y = "Induced Vertical Break (in)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          panel.grid.major = element_line(color = "grey85", size = 0.5),
          panel.grid.minor = element_line(color = "grey92", size = 0.3),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          axis.text.x = element_text(size = 8),
          legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    }
    
    # ---- 11e. Pitch location plot by batter side --------------------
    create_pitch_location_plot <- function(player_df, batter_side = "R") {
      if (is.null(player_df) || nrow(player_df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No data available",
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = ifelse(batter_side == "R",
                                     "RHB Location (Catcher's View)",
                                     "LHB Location (Catcher's View)")))
      }
      
      plot_data <- player_df %>%
        filter(stand == batter_side,
               !is.na(plate_x), !is.na(plate_z))
      
      if (nrow(plot_data) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No location data",
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = ifelse(batter_side == "R",
                                     "RHB Location (Catcher's View)",
                                     "LHB Location (Catcher's View)")))
      }
      
      summary_data <- plot_data %>%
        group_by(pitch_type) %>%
        summarise(
          mean_x = mean(plate_x, na.rm = TRUE),
          mean_z = mean(plate_z, na.rm = TRUE),
          sd_x = sd(plate_x, na.rm = TRUE),
          sd_z = sd(plate_z, na.rm = TRUE),
          .groups = "drop"
        )
      
      zone_width <- 17
      zone_height <- 26
      zone_left <- -zone_width / 2
      zone_right <- zone_width / 2
      zone_bottom <- 18
      zone_top <- zone_bottom + zone_height
      
      ggplot(summary_data, aes(x = mean_x, y = mean_z, colour = pitch_type, fill = pitch_type)) +
        ggforce::geom_ellipse(aes(x0 = mean_x, y0 = mean_z, a = sd_x, b = sd_z, angle = 0),
                              alpha = 0.2, colour = NA) +
        geom_point(size = 2) +
        geom_rect(xmin = zone_left, xmax = zone_right,
                  ymin = zone_bottom, ymax = zone_top,
                  colour = "black", fill = NA, linewidth = 0.5) +
        scale_colour_manual(values = pitch_colors, na.value = "grey50") +
        scale_fill_manual(values = pitch_colors, na.value = "grey50") +
        coord_fixed(xlim = c(-2, 2), ylim = c(0, 5)) +
        labs(title = ifelse(batter_side == "R",
                            "RHB Location (Catcher's View)",
                            "LHB Location (Catcher's View)"),
             x = "Plate X (ft)", y = "Plate Z (ft)") +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          panel.grid.major = element_line(color = "grey85", size = 0.5),
          panel.grid.minor = element_line(color = "grey92", size = 0.3),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          axis.text.x = element_text(size = 8),
          legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    }
    
    create_p3_movement_plot <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      plot_data <- df %>%
        mutate(
          horizontal_break = ifelse(p_throws == "L", -pfx_x, pfx_x),
          vertical_break = pfx_z
        ) %>%
        filter(!is.na(horizontal_break), !is.na(vertical_break))
      
      ggplot(plot_data, aes(x = horizontal_break, y = vertical_break, colour = pitch_type)) +
        geom_point(alpha = 0.7, size = 1.5) +
        geom_hline(yintercept = 0, colour = "black", size = 0.5) +
        geom_vline(xintercept = 0, colour = "black", size = 0.5) +
        scale_colour_manual(values = pitch_colors, na.value = "grey50") +
        scale_x_continuous(
          limits = c(-25, 25),
          breaks = seq(-20, 20, by = 10),
          labels = c("20", "10", "0", "10", "20")
        ) +
        scale_y_continuous(
          limits = c(-25, 25),
          breaks = seq(-20, 20, by = 10)
        ) +
        annotate("text", x = -15, y = -23, label = "Arm Side",
                 size = 3, hjust = 0.5, fontface = "bold") +
        annotate("text", x = 15, y = -23, label = "Glove Side",
                 size = 3, hjust = 0.5, fontface = "bold") +
        labs(title = "Pitch Movement", x = "Horizontal Break (in)", y = "Induced Vertical Break (in)") +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          panel.grid.major = element_line(color = "grey85", size = 0.5),
          panel.grid.minor = element_line(color = "grey92", size = 0.3),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          axis.text.x = element_text(size = 8),
          legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    }
    
    create_p3_location_plot <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      plot_data <- df %>%
        filter(!is.na(plate_loc_side), !is.na(plate_loc_height))
      
      if (nrow(plot_data) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No location data",
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = "Pitch Location"))
      }
      
      summary_data <- plot_data %>%
        group_by(pitch_type) %>%
        summarise(
          mean_x = mean(plate_loc_side, na.rm = TRUE),
          mean_z = mean(plate_loc_height, na.rm = TRUE),
          sd_x = sd(plate_loc_side, na.rm = TRUE),
          sd_z = sd(plate_loc_height, na.rm = TRUE),
          .groups = "drop"
        )
      
      zone_width <- 17
      zone_height <- 26
      zone_left <- -zone_width / 2
      zone_right <- zone_width / 2
      zone_bottom <- 18
      zone_top <- zone_bottom + zone_height
      
      ggplot(summary_data, aes(x = mean_x, y = mean_z, colour = pitch_type, fill = pitch_type)) +
        ggforce::geom_ellipse(aes(x0 = mean_x, y0 = mean_z, a = sd_x, b = sd_z, angle = 0),
                              alpha = 0.2, colour = NA) +
        geom_point(size = 2) +
        geom_rect(xmin = zone_left, xmax = zone_right,
                  ymin = zone_bottom, ymax = zone_top,
                  colour = "black", fill = NA, linewidth = 0.5) +
        scale_colour_manual(values = pitch_colors, na.value = "grey50") +
        scale_fill_manual(values = pitch_colors, na.value = "grey50") +
        coord_fixed(xlim = c(-24, 24), ylim = c(0, 60)) +
        labs(title = "Pitch Location", x = "Plate X (in)", y = "Plate Z (in)") +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          panel.grid.major = element_line(color = "grey85", size = 0.5),
          panel.grid.minor = element_line(color = "grey92", size = 0.3),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          axis.text.x = element_text(size = 8),
          legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    }
    
    # ---- 11e. Batter handedness pitch usage plot --------------------
    create_pitch_usage_plot <- function(player_df) {
      if (is.null(player_df) || nrow(player_df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No data available",
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = "Pitch Usage by Batter Side"))
      }
      
      pitch_order <- player_df %>%
        filter(!is.na(pitch_type)) %>%
        count(pitch_type, sort = TRUE) %>%
        pull(pitch_type)
      
      plot_data <- player_df %>%
        filter(!is.na(pitch_type), !is.na(stand)) %>%
        mutate(pitch_type = factor(pitch_type, levels = pitch_order)) %>%
        group_by(stand, pitch_type) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(stand) %>%
        mutate(percent = 100 * count / sum(count)) %>%
        ungroup() %>%
        mutate(direction = ifelse(stand == "L", -percent, percent))
      
      if (nrow(plot_data) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No usage data",
                          size = 3, hjust = 0.5) +
                 theme_void() +
                 theme(plot.title = element_text(hjust = 0.5, size = 12)) +
                 labs(title = "Pitch Usage by Batter Side"))
      }
      
      label_y <- length(unique(plot_data$pitch_type)) + 0.7
      
      ggplot(plot_data, aes(x = direction, y = pitch_type, fill = pitch_type)) +
        geom_col(color = "black", width = 0.6) +
        geom_text(aes(label = paste0(round(percent, 1), "%"),
                      hjust = ifelse(stand == "L", 1.1, -0.1)),
                  size = 3) +
        geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
        annotate("text", x = -50, y = label_y, label = "vs LHH",
                 size = 3, fontface = "bold") +
        annotate("text", x = 50, y = label_y, label = "vs RHH",
                 size = 3, fontface = "bold") +
        scale_fill_manual(values = pitch_colors, na.value = "grey50") +
        scale_x_continuous(
          labels = function(x) paste0(abs(x), "%"),
          limits = c(-100, 100)
        ) +
        labs(
          title = "Pitch Usage by Batter Side",
          x = "Usage %",
          y = NULL,
          fill = "Pitch"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          panel.grid.major = element_line(color = "grey85", size = 0.5),
          panel.grid.minor = element_line(color = "grey92", size = 0.3),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          legend.position = "none",
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        )
    }
    
    # ---- 11aa. Format API tables ------------------------------------
    prepare_game_logs_table <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      date_name <- intersect(c("date", "Date", "gameDate"), names(df))[1]
      date_col <- df[[date_name]]
      
      df <- df %>%
        transmute(
          Date = as.Date(date_col),
          IP = as.numeric(stat.inningsPitched),
          Ks = as.integer(stat.strikeOuts),
          BB = as.integer(stat.baseOnBalls),
          ER = as.integer(stat.earnedRuns)
        )
      
      datatable(
        df,
        options = list(
          dom = "t",
          ordering = FALSE,
          paging = FALSE,
          pageLength = nrow(df),
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      ) %>%
        formatStyle(columns = 1:ncol(df), fontSize = '10px')
    }
    
    prepare_season_stats_table <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      df <- df %>%
        transmute(
          Season = season,
          IP = round(as.numeric(stat.inningsPitched), 1),
          ERA = round(as.numeric(stat.era), 2),
          FIP = round(((13 * as.numeric(stat.homeRuns)) +
                         (3 * (as.numeric(stat.baseOnBalls) + as.numeric(stat.hitByPitch))) -
                         (2 * as.numeric(stat.strikeOuts))) /
                        as.numeric(stat.inningsPitched) + 3.166, 2),
          Ks = as.integer(stat.strikeOuts),
          Walks = as.integer(stat.baseOnBalls)
        )
      
      datatable(
        df,
        options = list(
          dom = "t",
          ordering = FALSE,
          pageLength = 10,
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      ) %>%
        formatStyle(columns = 1:ncol(df), fontSize = '10px')
    }
    
    # ---- 12. Render all tables and plots -----------------------------
    # Player 1 tables
    output$season_table1 <- renderDT({
      req(player1_data())
      create_compact_table(get_season_data1())
    })
    
    output$game_table1 <- renderDT({
      data <- get_game_data1()
      req(!is.null(data))
      create_compact_table(data)
    })
    
    output$game_logs_table1 <- renderDT({
      data <- game_logs_data1()
      req(!is.null(data))
      prepare_game_logs_table(data)
    })
    
    output$p3_table1 <- renderDT({
      data <- get_p3_filtered_data1()
      req(!is.null(data))
      create_compact_table(data)
    })
    
    # Player 2 tables
    output$season_table2 <- renderDT({
      req(player2_data())
      create_compact_table(get_season_data2())
    })
    
    output$game_table2 <- renderDT({
      data <- get_game_data2()
      req(!is.null(data))
      create_compact_table(data)
    })
    
    output$game_logs_table2 <- renderDT({
      data <- game_logs_data2()
      req(!is.null(data))
      prepare_game_logs_table(data)
    })
    
    output$p3_table2 <- renderDT({
      data <- get_p3_filtered_data2()
      req(!is.null(data))
      create_compact_table(data)
    })
    
    output$season_stats_table1 <- renderDT({
      data <- season_stats_data1()
      req(!is.null(data))
      prepare_season_stats_table(data)
    })
    
    output$season_stats_table2 <- renderDT({
      data <- season_stats_data2()
      req(!is.null(data))
      prepare_season_stats_table(data)
    })
    
    observe({
      req(isTRUE(input$season_split1))
      data <- get_season_data1()
      years <- sort(unique(data$year))
      lapply(years, function(y) {
        output[[paste0("season_table1_", y)]] <- renderDT({
          create_compact_table(filter(data, year == y))
        })
        output[[paste0("stuffplus_plot1_", y)]] <- renderPlot({
          create_stuffplus_plot(filter(data, year == y))
        })
        output[[paste0("pitch_breaks_plot1_", y)]] <- renderPlot({
          create_pitch_breaks_plot(filter(data, year == y))
        })
        output[[paste0("pitch_usage_plot1_", y)]] <- renderPlot({
          create_pitch_usage_plot(filter(data, year == y))
        })
      })
    })
    
    observe({
      req(isTRUE(input$season_split2))
      data <- get_season_data2()
      years <- sort(unique(data$year))
      lapply(years, function(y) {
        output[[paste0("season_table2_", y)]] <- renderDT({
          create_compact_table(filter(data, year == y))
        })
        output[[paste0("stuffplus_plot2_", y)]] <- renderPlot({
          create_stuffplus_plot(filter(data, year == y))
        })
        output[[paste0("pitch_breaks_plot2_", y)]] <- renderPlot({
          create_pitch_breaks_plot(filter(data, year == y))
        })
        output[[paste0("pitch_usage_plot2_", y)]] <- renderPlot({
          create_pitch_usage_plot(filter(data, year == y))
        })
      })
    })
    
    # Stuff+ plots
    output$stuffplus_plot1 <- renderPlot({
      create_stuffplus_plot(get_season_data1())
    })
    
    output$stuffplus_plot2 <- renderPlot({
      create_stuffplus_plot(get_season_data2())
    })
    
    # Pitch breaks plots
    output$pitch_breaks_plot1 <- renderPlot({
      create_pitch_breaks_plot(get_season_data1())
    })
    
    output$pitch_breaks_plot2 <- renderPlot({
      create_pitch_breaks_plot(get_season_data2())
    })
    
    # Pitch usage plots
    output$pitch_usage_plot1 <- renderPlot({
      create_pitch_usage_plot(get_season_data1())
    })
    
    output$pitch_usage_plot2 <- renderPlot({
      create_pitch_usage_plot(get_season_data2())
    })
    
    # Game-specific plots
    output$game_breaks_plot1 <- renderPlot({
      create_pitch_breaks_plot(get_game_data1())
    })
    
    output$game_breaks_plot2 <- renderPlot({
      create_pitch_breaks_plot(get_game_data2())
    })
    
    output$location_rhb_plot1 <- renderPlot({
      create_pitch_location_plot(get_game_data1(), "R")
    })
    
    output$location_lhb_plot1 <- renderPlot({
      create_pitch_location_plot(get_game_data1(), "L")
    })
    
    output$location_rhb_plot2 <- renderPlot({
      create_pitch_location_plot(get_game_data2(), "R")
    })
    
    output$location_lhb_plot2 <- renderPlot({
      create_pitch_location_plot(get_game_data2(), "L")
    })
    
    output$p3_movement_plot1 <- renderPlot({
      create_p3_movement_plot(get_p3_filtered_data1())
    })
    
    output$p3_location_plot1 <- renderPlot({
      create_p3_location_plot(get_p3_filtered_data1())
    })
    
    output$p3_movement_plot2 <- renderPlot({
      create_p3_movement_plot(get_p3_filtered_data2())
    })
    
    output$p3_location_plot2 <- renderPlot({
      create_p3_location_plot(get_p3_filtered_data2())
    })
    
    # ---- 13. Summary UIs ---------------------------------------------
    # Player 1
    output$season_summary_ui1 <- renderUI({
      data <- get_season_data1()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      ns <- session$ns
      years <- sort(unique(data$year))
      if (!isTRUE(input$season_split1)) {
        tagList(
          h3(paste("Season Pitch Metrics:", paste(years, collapse = ", ")), class = "section-title"),
          div(class = "plot-row",
              div(class = "stuffplus-plot-wrapper", plotOutput(ns("stuffplus_plot1"), height = "300px")),
              div(class = "breaks-plot-wrapper", plotOutput(ns("pitch_breaks_plot1"), height = "300px")),
              div(class = "usage-plot-wrapper", plotOutput(ns("pitch_usage_plot1"), height = "300px"))
          ),
          div(class = "data-table-container", DTOutput(ns("season_table1")))
        )
      } else {
        tagList(
          h3(paste("Season Pitch Metrics:", paste(years, collapse = ", ")), class = "section-title"),
          lapply(years, function(y) {
            tagList(
              h4(paste("Season", y), class = "section-title", style = "margin-top: 16px;"),
              div(class = "plot-row",
                  div(class = "stuffplus-plot-wrapper", plotOutput(ns(paste0("stuffplus_plot1_", y)), height = "300px")),
                  div(class = "breaks-plot-wrapper", plotOutput(ns(paste0("pitch_breaks_plot1_", y)), height = "300px")),
                  div(class = "usage-plot-wrapper", plotOutput(ns(paste0("pitch_usage_plot1_", y)), height = "300px"))
              ),
              div(class = "data-table-container",
                  DTOutput(ns(paste0("season_table1_", y)))
              )
            )
          })
        )
      }
    })
    
    output$game_logs_ui1 <- renderUI({
      data <- game_logs_data1()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      years_text <- paste(sort(input$logs_year_filter1), collapse = ", ")
      range_text <- if (!is.null(input$logs_range1) && input$logs_range1 > 0)
        paste("- Last", input$logs_range1, "Games") else "- All Games"
      tagList(
        h3(paste("Game Logs:", years_text, range_text), class = "section-title", style = "margin-top: 16px;"),
        div(class = "data-table-container", DTOutput(ns("game_logs_table1")))
      )
    })
    
    output$game_summary_ui1 <- renderUI({
      data <- get_game_data1()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      dates <- sort(as.Date(input$date_filter1))
      if (length(dates) == 0) return(NULL)
      date_text <- if (length(dates) == 1) {
        format(dates, "%b %d, %Y")
      } else {
        paste(format(min(dates), "%b %d, %Y"), "-", format(max(dates), "%b %d, %Y"))
      }
      tagList(
        h3(paste("Games", date_text), class = "section-title", style = "margin-top: 16px;"),
        div(class = "plot-row",
            div(class = "breaks-plot-wrapper", plotOutput(ns("game_breaks_plot1"), height = "300px")),
            div(class = "stuffplus-plot-wrapper", plotOutput(ns("location_rhb_plot1"), height = "300px")),
            div(class = "usage-plot-wrapper", plotOutput(ns("location_lhb_plot1"), height = "300px"))
        ),
        div(class = "data-table-container", DTOutput(ns("game_table1")))
      )
    })
    
    output$season_stats_ui1 <- renderUI({
      data <- season_stats_data1()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      years_text <- paste(sort(input$stats_year_filter1), collapse = ", ")
      tagList(
        h3(paste("Season Stats:", years_text), class = "section-title", style = "margin-top: 16px;"),
        div(class = "data-table-container", DTOutput(ns("season_stats_table1")))
      )
    })
    
    output$p3_summary_ui1 <- renderUI({
      data <- get_p3_filtered_data1()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      ns <- session$ns
      tagList(
        h3("P3 Pitch Metrics", class = "section-title"),
        div(class = "plot-row",
            div(class = "breaks-plot-wrapper", plotOutput(ns("p3_movement_plot1"), height = "300px")),
            div(class = "stuffplus-plot-wrapper", plotOutput(ns("p3_location_plot1"), height = "300px"))
        ),
        div(class = "data-table-container", DTOutput(ns("p3_table1")))
      )
    })
    
    # Player 2
    output$season_summary_ui2 <- renderUI({
      data <- get_season_data2()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      ns <- session$ns
      years <- sort(unique(data$year))
      if (!isTRUE(input$season_split2)) {
        tagList(
          h3(paste("Season Pitch Metrics:", paste(years, collapse = ", ")), class = "section-title"),
          div(class = "plot-row",
              div(class = "stuffplus-plot-wrapper", plotOutput(ns("stuffplus_plot2"), height = "300px")),
              div(class = "breaks-plot-wrapper", plotOutput(ns("pitch_breaks_plot2"), height = "300px")),
              div(class = "usage-plot-wrapper", plotOutput(ns("pitch_usage_plot2"), height = "300px"))
          ),
          div(class = "data-table-container", DTOutput(ns("season_table2")))
        )
      } else {
        tagList(
          h3(paste("Season Pitch Metrics:", paste(years, collapse = ", ")), class = "section-title"),
          lapply(years, function(y) {
            tagList(
              h4(paste("Season", y), class = "section-title", style = "margin-top: 16px;"),
              div(class = "plot-row",
                  div(class = "stuffplus-plot-wrapper", plotOutput(ns(paste0("stuffplus_plot2_", y)), height = "300px")),
                  div(class = "breaks-plot-wrapper", plotOutput(ns(paste0("pitch_breaks_plot2_", y)), height = "300px")),
                  div(class = "usage-plot-wrapper", plotOutput(ns(paste0("pitch_usage_plot2_", y)), height = "300px"))
              ),
              div(class = "data-table-container",
                  DTOutput(ns(paste0("season_table2_", y)))
              )
            )
          })
        )
      }
    })
    
    output$game_logs_ui2 <- renderUI({
      data <- game_logs_data2()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      years_text <- paste(sort(input$logs_year_filter2), collapse = ", ")
      range_text <- if (!is.null(input$logs_range2) && input$logs_range2 > 0)
        paste("- Last", input$logs_range2, "Games") else "- All Games"
      tagList(
        h3(paste("Game Logs:", years_text, range_text), class = "section-title", style = "margin-top: 16px;"),
        div(class = "data-table-container", DTOutput(ns("game_logs_table2")))
      )
    })
    
    output$game_summary_ui2 <- renderUI({
      data <- get_game_data2()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      dates <- sort(as.Date(input$date_filter2))
      if (length(dates) == 0) return(NULL)
      date_text <- if (length(dates) == 1) {
        format(dates, "%b %d, %Y")
      } else {
        paste(format(min(dates), "%b %d, %Y"), "-", format(max(dates), "%b %d, %Y"))
      }
      tagList(
        h3(paste("Games", date_text), class = "section-title", style = "margin-top: 16px;"),
        div(class = "plot-row",
            div(class = "breaks-plot-wrapper", plotOutput(ns("game_breaks_plot2"), height = "300px")),
            div(class = "stuffplus-plot-wrapper", plotOutput(ns("location_rhb_plot2"), height = "300px")),
            div(class = "usage-plot-wrapper", plotOutput(ns("location_lhb_plot2"), height = "300px"))
        ),
        div(class = "data-table-container", DTOutput(ns("game_table2")))
      )
    })
    
    output$season_stats_ui2 <- renderUI({
      data <- season_stats_data2()
      if (is.null(data)) return(NULL)
      ns <- session$ns
      years_text <- paste(sort(input$stats_year_filter2), collapse = ", ")
      tagList(
        h3(paste("Season Stats:", years_text), class = "section-title", style = "margin-top: 16px;"),
        div(class = "data-table-container", DTOutput(ns("season_stats_table2")))
      )
    })
    
    output$p3_summary_ui2 <- renderUI({
      data <- get_p3_filtered_data2()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      ns <- session$ns
      tagList(
        h3("P3 Pitch Metrics", class = "section-title"),
        div(class = "plot-row",
            div(class = "breaks-plot-wrapper", plotOutput(ns("p3_movement_plot2"), height = "300px")),
            div(class = "stuffplus-plot-wrapper", plotOutput(ns("p3_location_plot2"), height = "300px"))
        ),
        div(class = "data-table-container", DTOutput(ns("p3_table2")))
      )
    })
    
  })
}
