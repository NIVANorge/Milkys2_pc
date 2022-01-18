#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# Run at start-up ----
#

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(DT)


df_projects <- get_projects() %>%
  select(-ENTERED_BY) %>%
  arrange(PROJECT_NAME) %>%
  mutate(Menu_string = paste0(PROJECT_NAME, " (ID:", PROJECT_ID, ")"))

projects_available <- df_projects$Menu_string

#
# USER INTERFACE ----
#

ui <- fluidPage(

    # Application title
    titlePanel("Biota chemistry - read data directly from Nivabasen"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
          
          # Menu ----
          selectizeInput(inputId = "projects_selected", label = "Projects", 
                         choices = NULL, multiple = TRUE),
          radioButtons("stations_or_years", "Primary selection: years or stations", 
                       choices = c("Years", "Stations"), selected = "Years"),
          selectizeInput(inputId = "years_selected", label = "Years", 
                         choices = NULL, multiple = TRUE),
          selectizeInput(inputId = "stations_selected", label = "Stations", 
                         choices = NULL, multiple = TRUE),
          width = 4
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # Plots ----
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Stations x years",
                plotOutput("station_years_plot", width = "600px"),
                div(DTOutput("station_years_datatable"), style = "font-size:90%")
              ), # end tabPanel 1

              tabPanel(
                "Specimens",
                div(DTOutput("specimens_datatable"), style = "font-size:90%")
              ), # end tabPanel 3
              
              tabPanel(
                "Samples",
                div(DTOutput("samples_selected_years_datatable"), style = "font-size:90%")
              ), # end tabPanel 4
              
              tabPanel(
                "Measurements",
                div(DTOutput("measurements_selected_years_datatable"), style = "font-size:90%")
              ) # end tabPanel 5
              
            
            ), # end tabsetPanel
            width = 8
            
        )  # end mainPAnel
    )
)

#
# SERVER ----
#
server <- function(input, output, session) {
  
  #
  # fill menu projects_available ----
  #
  updateSelectizeInput(
    session,
    inputId = "projects_selected",
    choices = projects_available,
    server = TRUE)
  
  #
  # get_stations ----
  #
  # Triggered by selecting project
  # For 'stations_selected' menu
  #
  get_stations <- reactive({
    
    validate(
      need(input$projects_selected != "", "Please select a project")
    )
    
    df_projects_sel <- df_projects %>%
      filter(Menu_string %in% input$projects_selected)
    id <- df_projects_sel %>%
      pull(PROJECT_ID)
    
    result <- get_nivabase_selection(
      "PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME, STATION_IS_ACTIVE, PROJECTS_STATION_ID",
      "PROJECTS_STATIONS",
      "PROJECT_ID",
      id) %>%
      mutate(
        Menu_string = paste0(STATION_CODE, " ", STATION_NAME, " (ID:", STATION_ID, ")")
      )
    result
  })
  
  #
  # get_stations_years ----
  #
  # Triggered by get_stations (which is triggered by selecting project)
  #
  # Count of number of 
  get_stations_years <- reactive({
    df_stations <- get_stations() %>%
      distinct(STATION_ID, STATION_CODE, STATION_NAME, Menu_string)
    station_id <- df_stations %>%
      pull(STATION_ID) %>%
      unique()
    sql <-   paste(
      "select STATION_ID, extract(YEAR from DATE_CAUGHT) as YEAR, count(*) as N",
      "from NIVADATABASE.BIOTA_SINGLE_SPECIMENS",
      "WHERE STATION_ID in",
      paste("(", paste(station_id, collapse = ","), ")"),
      "group by STATION_ID, extract(YEAR from DATE_CAUGHT)",
      "order by STATION_ID"
    )
    result <- get_nivabase_data(sql) %>%
      left_join(
        df_stations %>% select(STATION_ID, STATION_CODE, STATION_NAME, Menu_string),
        by = "STATION_ID") %>%
      select(STATION_CODE, STATION_NAME, STATION_ID, YEAR, N, Menu_string)
    # browser()
    result
  })
  

  #
  # output station_years_plot ----
  #
  output$station_years_plot <- renderPlot({
    df <- get_stations_years()
    # df <- mutate(STATION_ID = factor(STATION_ID))
    resultplot <- ggplot(df, aes(YEAR, STATION_CODE, fill = N)) +
      geom_tile() +
      scale_fill_viridis_b()
    resultplot
    })
  
  #
  # output station_years_datatable ----
  #
  output$station_years_datatable <- DT::renderDT(
    get_stations_years(), 
    filter = "top"
  )
  
  #
  # fill menu stations_selected ---- 
  #
  # Using get_stations
  #
  observeEvent(get_stations(), {
    df_stations <- get_stations()
    stations_available <- df_stations %>% pull(Menu_string)
    updateSelectizeInput(
      inputId = "stations_selected",
      choices = stations_available
      # selected = input$stations_selected
      )
  })
  
  #
  # fill menu years_selected ----  
  #
  # Using get_stations_years
  #
  observeEvent(get_stations_years(), {
    df <- get_stations_years()
    years_available <- df %>%
      distinct(YEAR) %>%
      pull(YEAR) %>%
      sort()
    updateSelectizeInput(
      inputId = "years_selected",
      choices = years_available
      # selected = input$stations_selected
    )
  })
  
  

  #
  # get_specimens ----
  #
  # Note: can be used with or without selecting stations  
  # - if stations NOT selected, it returns specimens for all stations that year    
  # - if stations ARE selected, it returns specimens for specific stations that year    
  #
  
  get_specimens <- reactive({
    
    if (input$stations_or_years == "Years"){
      validate(
        need(input$years_selected != "", "Please select a year")
      )
    } else {
      validate(
        need(input$stations_selected != "", "Please select a station")
      )
    }
    
    df_stations_years <- get_stations_years()
    df_stations <- get_stations()  # this have 'Menu_string" so we need this one too
    # browser()

    sql_years <- paste(" and extract(YEAR from DATE_CAUGHT) in (", 
                       paste(input$years_selected, collapse = ","),
                       ")")  
    
    if (is.null(input$stations_selected)){
      station_ids <- df_stations_years %>%
        filter(YEAR %in% input$years_selected) %>%
        pull(STATION_ID)
      extras_sql_string <- sql_years   
    } else if (is.null(input$years_selected)){
      station_ids <- df_stations %>%
        filter(Menu_string %in% input$stations_selected) %>%
        pull(STATION_ID)
      extras_sql_string <- ""
    } else {
      station_ids <- df_stations_years %>%
        filter(Menu_string %in% input$stations_selected) %>%
        pull(STATION_ID)
      extras_sql_string <- sql_years   
    }
    result <- get_nivabase_selection(
      "STATION_ID, DATE_CAUGHT, SPECIMEN_NO, TAXONOMY_CODE_ID, SPECIMEN_ID",
      "BIOTA_SINGLE_SPECIMENS",
      "STATION_ID",
      station_ids, 
      extra_where = extras_sql_string
    )
    # browser()
    result
  })
  
  #
  # output specimens_selected_stations_datatable ----
  #
  output$specimens_datatable <- DT::renderDT(
    get_specimens(), 
    filter = "top"
  )
  
  #
  # get_samples ----
  #

  get_samples <- reactive({
    
    if (input$stations_or_years == "Years"){
      validate(
        need(input$years_selected != "", "Please select a year")
      )
    } else {
      validate(
        need(input$stations_selected != "", "Please select a station")
      )
    }
    
    df_specimens <- get_specimens()  
    
    specimen_ids <- df_specimens %>%
      pull(SPECIMEN_ID) %>%
      unique()
    
    result_samp_spec <- get_nivabase_selection(
      "SPECIMEN_ID, SAMPLE_ID, BIOTA_SAMPLES_SPECIMENS_ID",
      "BIOTA_SAMPLES_SPECIMENS",
      "SPECIMEN_ID",
      specimen_ids
    )
    
    result_specimens_summ <- df_specimens %>%
      left_join(
        result_samp_spec %>% select(SPECIMEN_ID, SAMPLE_ID),
        by = "SPECIMEN_ID") %>%
      group_by(SAMPLE_ID) %>%
      summarise(
        SPECIMEN_ID = paste(SPECIMEN_ID, collapse = ","),
        SPECIMEN_NO = paste(SPECIMEN_NO, collapse = ","))
    
    
    result <- get_nivabase_selection(
      "SAMPLE_ID, TISSUE_ID, SAMPLE_DATE, SAMPLE_NO, REPNO",
      "BIOTA_SAMPLES",
      "SAMPLE_ID",
      unique(result_samp_spec$SAMPLE_ID)
    ) %>%
      left_join(result_specimens_summ, by = "SAMPLE_ID")

    # browser()
    result
  })
  
  #
  # output specimens_selected_stations_datatable ----
  #
  output$samples_selected_years_datatable <- DT::renderDT(
    get_samples(), 
    filter = "top"
  )
  
  #
  # get_measurements ----
  #
  
  get_measurements <- reactive({
    
    if (input$stations_or_years == "Years"){
      validate(
        need(input$years_selected != "", "Please select a year")
      )
    } else {
      validate(
        need(input$stations_selected != "", "Please select a station")
      )
    }
    
    df_samples <- get_samples()  
    
    sample_ids <- df_samples %>%
      pull(SAMPLE_ID) %>%
      unique()
    
    result <- get_nivabase_selection(
      "SAMPLE_ID, METHOD_ID, VALUE, FLAG1, DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, VALUE_ID",
      "BIOTA_CHEMISTRY_VALUES",
      "SAMPLE_ID",
      sample_ids)
    
    df_methods <- get_nivabase_selection(
      "METHOD_ID, NAME, UNIT, LABORATORY, MATRIX",
      "METHOD_DEFINITIONS",
      "METHOD_ID",
      unique(result$METHOD_ID))
    
    result <- result %>%
      left_join(
        df_methods, by = "METHOD_ID") %>%
      left_join(
        df_samples %>% select(SAMPLE_ID, SAMPLE_NO, SPECIMEN_NO, SPECIMEN_ID), 
        by = "SAMPLE_ID") %>%
      select(
        SAMPLE_NO, SPECIMEN_NO, NAME, UNIT, VALUE, FLAG1, 
        DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, 
        LABORATORY, MATRIX,
        SAMPLE_ID, SPECIMEN_ID, METHOD_ID, VALUE_ID)

    # browser()
    result
  })
  
  #
  # output measurements_selected_years_datatable ----
  #
  output$measurements_selected_years_datatable <- DT::renderDT(
    get_measurements(), 
    filter = "top"
  )
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
