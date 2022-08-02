#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# ** App overview ** ----
#
# menu 'projects_menu' is filled using 'projects_available' (available from start-up)
# user selects projects -> 'get_stations' data frame
#   -> creates 'get_stations_years' data frame (one line per station x year)
#   -> creates 'get_stations_years_select' data frame (one line per station x year)
#   -> outputs plot 'station_years_plot' and table 'station_years_datatable'  
#   -> filling menus 'stations_menu', 'years_menu' and 'lastyear_data_series'  
# user selects stations and/or years and clicks button 'download_samples'  
#   -> creates 'get_specimens', 'get_samples' and 'get_parameters' data frames    
#   -> outputs tables 'specimens_datatable', 'samples_datatable' and 'parameters_datatable'  
#   -> fills menu 'parameters_menu'  
# user selects parameters in menu 'parameters_menu'  
#   -> creates 'get_measurements' data frame  
#   -> outputs table 'measurements_datatable'  


#
# Run at start-up ----
#

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(DT)
library(niRvana)

# . df_projects ----
df_projects <- get_projects() %>%
  select(-ENTERED_BY) %>%
  arrange(PROJECT_NAME) %>%
  mutate(Menu_string = paste0(PROJECT_NAME, " (ID:", PROJECT_ID, ")"))

# . projects_available ----
projects_available <- df_projects$Menu_string

# . lookup_tissues ----
lookup_tissues <- get_nivabase_data(
  "select TISSUE_ID, TISSUE_NAME from NIVADATABASE.BIOTA_TISSUE_TYPES")

# . df_taxoncode_id ----
df_taxoncode_id <- get_nivabase_data(
  "select DISTINCT TAXONOMY_CODE_ID from NIVADATABASE.BIOTA_SINGLE_SPECIMENS;")

# Taxonomy lookup table
# use NAME (from TAXONOMY_CODES) instead of LATIN_NAME (from NIVADATABASE.TAXONOMY) as some 
#  things such as 'Zooplankton epilimnion' only has NAME
# When latin names does exist, NAME and LATIN_NAME seem to always be the same

lookup_taxonomy <- get_nivabase_data(paste(
  "select TAXONOMY_CODE_ID, NAME as TAXON_NAME",
  "from NIVADATABASE.TAXONOMY_CODES",
  "where TAXONOMY_CODE_ID in (",
  paste(sQuote(df_taxoncode_id$TAXONOMY_CODE_ID), collapse = ","), 
  ");"
))



#
# USER INTERFACE ----
#

ui <- fluidPage(

    # Application title
    titlePanel("Biota chemistry - read data directly from Nivabasen"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
          
          # Sidebar menu ----
          selectizeInput(inputId = "projects_menu", label = "Projects", 
                         choices = NULL, multiple = TRUE),
          textOutput("n_years_stations"),
          br(),
          selectizeInput(inputId = "years_menu", label = "Years", 
                         choices = NULL, multiple = TRUE),
          selectizeInput(inputId = "stations_menu", label = "Stations", 
                         choices = NULL, multiple = TRUE),
          actionButton("download_samples", "Download available samples"),
          textOutput("n_specimens_samples"),
          br(),
          selectizeInput(inputId = "parameters_menu", label = "Parameters", 
                         choices = NULL, multiple = TRUE),
          width = 4
        ),

        # Show a plot of the generated distribution
        mainPanel(
          
            # Result tabs ----
            
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Project",
                selectizeInput(inputId = "lastyear_data_series", label = "Show time series lasting at least until", 
                               choices = NULL, multiple = FALSE),
                plotOutput("station_years_plot", width = "600px"),
                div(DTOutput("station_years_datatable"), style = "font-size:90%")
              ), # end tabPanel 1

              tabPanel(
                "Specimens",
                div(DTOutput("specimens_datatable"), style = "font-size:90%")
              ), # end tabPanel 2
              
              tabPanel(
                "Samples",
                div(DTOutput("samples_datatable"), style = "font-size:90%")
              ), # end tabPanel 3
              
              tabPanel(
                "Parameters",
                div(DTOutput("parameters_datatable"), style = "font-size:90%")
              ), # end tabPanel 4
              
              tabPanel(
                "Measurements",
                div(DTOutput("measurements_datatable"), style = "font-size:90%")
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
  # fill menu 'projects_menu' ----
  #
  updateSelectizeInput(
    session,
    inputId = "projects_menu",
    choices = projects_available,
    server = TRUE)
  
  #
  # get_stations ----
  #
  # Used to fill menu 'stations_menu' and make 'get_stations_years' and 'get_specimens'   
  #
  # Triggered by selecting project
  # For the menu 'stations_menu', plus 'get_stations_years', ''
  # Returns data frame from PROJECTS_STATIONS (one line per station x project)
  #
  get_stations <- reactive({
    
    validate(
      need(input$projects_menu != "", "Please select a project")
    )
    
    df_projects_sel <- df_projects %>%
      filter(Menu_string %in% input$projects_menu)
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
  # Used to fill menu 'years_menu'  make 'get_stations_years_select' and 'get_specimens'   
  #
  # Triggered by get_stations (which is triggered by selecting project)
  # Returns data frame with one line per station x year, including count of number of specimens 
  
  get_stations_years <- reactive({
    
    df_stations <- get_stations() %>%
      distinct(STATION_ID, STATION_CODE, STATION_NAME, Menu_string)
    station_ids <- df_stations %>%
      pull(STATION_ID) %>%
      unique()

    result <- get_nivabase_selection(
      "STATION_ID, extract(YEAR from DATE_CAUGHT) as YEAR, count(*) as N",
      "BIOTA_SINGLE_SPECIMENS",
      "STATION_ID",
      station_ids, 
      extra_sql = "group by STATION_ID, extract(YEAR from DATE_CAUGHT) order by STATION_ID"
    ) %>%
      left_join(
        df_stations %>% select(STATION_ID, STATION_CODE, STATION_NAME, Menu_string),
        by = "STATION_ID") %>%
      select(STATION_CODE, STATION_NAME, STATION_ID, YEAR, N, Menu_string)
    
    result
    
  })
  
  #
  # output n_years_stations ----
  #
  output$n_years_stations <- renderText(
    paste("Number of years x stations:", nrow(get_stations_years()))
  )
  
  #
  # get_stations_years_select ----
  #
  # Used to make outputs 'station_years_plot' and 'station_years_datatable'  
  #
  # Triggered by get_stations_years (which is triggered by selecting project)
  # Retrieves 'get_stations_years' and groups by station  
  #   - using menu input 'lastyear_data_series', it excludes stations not used after e.g. 2020
  #
  # Returns data frame with one line per station and columns STATION_ID and YEAR_last   
  #
  get_stations_years_select <- reactive({

    df_stations_years <- get_stations_years()
    
    if (!is.null(input$lastyear_data_series)){
      df_stations_years <- df_stations_years %>%
        group_by(STATION_ID) %>%
        mutate(YEAR_last = max(YEAR)) %>%
        ungroup() %>%
        filter(YEAR_last >= input$lastyear_data_series)
    }
    
    df_stations_years
    
  })
  
  
  #
  # output 'station_years_plot' ----
  #
  output$station_years_plot <- renderPlot({
    
    df_stations_years <- get_stations_years_select()
    
    # df <- mutate(STATION_ID = factor(STATION_ID))
    resultplot <- ggplot(df_stations_years, aes(YEAR, STATION_CODE, fill = N)) +
      geom_tile() +
      scale_fill_viridis_b()
    resultplot
    
  })
  
  #
  # output 'station_years_datatable' ----
  #
  output$station_years_datatable <- DT::renderDT(
    get_stations_years_select(), 
    filter = "top"
  )
  
  #
  # fill menu 'stations_menu' ---- 
  #
  # Triggered by 'get_stations' (which is triggered by selecting project)
  # Retrieving column 'Menu_string' from get_stations()
  #
  observeEvent(get_stations(), {
    df_stations <- get_stations()
    stations_available <- df_stations %>% pull(Menu_string)
    updateSelectizeInput(
      inputId = "stations_menu",
      choices = stations_available
      # selected = input$stations_menu
      )
  })
  
  #
  # fill menu 'years_menu' ----  
  #
  # Triggered by 'get_stations_years' (which is triggered by selecting project)
  # Retrieving unique values of column 'YEAR' from get_stations_years()
  #
  observeEvent(get_stations_years(), {
    df <- get_stations_years()
    years_available <- df %>%
      distinct(YEAR) %>%
      pull(YEAR) %>%
      sort()
    updateSelectizeInput(
      inputId = "years_menu",
      choices = years_available
      # selected = input$stations_menu
    )
  })
  
  
  #
  # fill menu 'lastyear_data_series' ----  
  #
  # Triggered by 'get_stations_years' (which is triggered by selecting project)
  # Retrieving unique values of column 'YEAR' from get_stations_years() 
  #
  observeEvent(get_stations_years(), {
    df <- get_stations_years()
    years_available <- df %>%
      distinct(YEAR) %>%
      pull(YEAR) %>%
      sort(decreasing = TRUE)
    updateSelectizeInput(
      inputId = "lastyear_data_series",
      choices = years_available
      # selected = input$stations_menu
    )
  })
  
  
  #
  # get_specimens ----
  #
  # Used to make 'get_samples', also for output 'specimens_datatable'        
  #
  # Triggered when user clicks button 'download_samples'  
  # Fetches data frames 'get_stations_years' and 'get_stations'  
  # - then retrieves STATION_ID for selected stations/years
  # From Nivabasen, gets BIOTA_SINGLE_SPECIMENS rows based on STATION_IDs and (if selected) years
  # - then adds columns STATION_CODE, STATION_NAME, TAXON_NAME by left join  
  # Returns table with one line per specimen  
  #
  # Note: works by selecting either (or both of) stations and years   
  # - if stations NOT selected, it returns specimens for all stations that year    
  # - if stations ARE selected, it returns specimens for specific stations that year  
  #
  
  get_specimens <- eventReactive(input$download_samples, {
    
    # Make sure either years or stations is selected
    if (is.null(input$years_menu) & is.null(input$stations_menu)){
      validate(need(FALSE, "Please select years and/or stations"))
    }
    
    df_stations_years <- get_stations_years()
    df_stations <- get_stations()  # this one has 'Menu_string" so we need this one too
    # browser()

    sql_years <- paste(" and extract(YEAR from DATE_CAUGHT) in (", 
                       paste(input$years_menu, collapse = ","),
                       ")")  
    
    if (is.null(input$stations_menu)){
      station_ids <- df_stations_years %>%
        filter(YEAR %in% input$years_menu) %>%
        pull(STATION_ID)
      extras_sql_string <- sql_years   
    } else if (is.null(input$years_menu)){
      station_ids <- df_stations %>%
        filter(Menu_string %in% input$stations_menu) %>%
        pull(STATION_ID)
      extras_sql_string <- ""
    } else {
      station_ids <- df_stations_years %>%
        filter(Menu_string %in% input$stations_menu) %>%
        pull(STATION_ID)
      extras_sql_string <- sql_years   
    }
    
    result_1 <- get_nivabase_selection(
      "STATION_ID, DATE_CAUGHT, SPECIMEN_NO, TAXONOMY_CODE_ID, SPECIMEN_ID",
      "BIOTA_SINGLE_SPECIMENS",
      "STATION_ID",
      station_ids, 
      extra_sql = extras_sql_string
    )
    # browser()
    
    result_2 <- result_1 %>%
      left_join(
        df_stations %>% select(STATION_ID, PROJECT_ID, STATION_CODE, STATION_NAME),
        by = "STATION_ID") %>% 
      left_join(
        lookup_taxonomy, by = "TAXONOMY_CODE_ID",
      ) %>%
      select(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, everything())  # ordering columns    
    
    # browser()
    result_2 
    
  })
  
  #
  # output 'specimens_datatable' ----
  #
  output$specimens_datatable <- DT::renderDT(
    get_specimens(), 
    filter = "top"
  )
  
  #
  # get_samples ----
  #
  # Used to make 'get_parameters' and (after selecting parameters) 'get_measurements'
  # - also for output 'samples_datatable'        
  #
  # Triggered by 'get_specimens' (which is triggerd when user clicks button 'download_samples')  
  # Fetches data frame 'get_specimens'    
  # - then retrieves SPECIMEN_ID  
  # From Nivabasen, gets BIOTA_SAMPLES_SPECIMENS rows based on SPECIMEN_ID  
  # - then summarizes this table by SAMPLE_ID (getting one line per sample) -> result_specimens_summ
  # From Nivabasen, gets BIOTA_SAMPLES rows based on SAMPLE_ID (from 'result_specimens_summ')  
  # - adds specimen info by left join (STATION_CODE, DATE_CAUGHT, SPECIMEN_NO etc. from 'result_specimens_summ')  
  # - adds TISSUE_NAME by left join  
  # Returns table with one line per sample  

  get_samples <- reactive({
    
    # Make sure either years or stations is selected
    if (is.null(input$years_menu) & is.null(input$stations_menu)){
      validate(need(FALSE, "Please select years and/or stations"))
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
      summarise(across(
          c(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, SPECIMEN_NO, SPECIMEN_ID),
          .fn = ~paste(unique(.x), collapse = ",")
        ))
    
    result <- get_nivabase_selection(
      "SAMPLE_ID, TISSUE_ID, SAMPLE_DATE, SAMPLE_NO, REPNO",
      "BIOTA_SAMPLES",
      "SAMPLE_ID",
      unique(result_samp_spec$SAMPLE_ID)
    ) %>%
      left_join(result_specimens_summ, by = "SAMPLE_ID") %>%
      left_join(lookup_tissues, by = "TISSUE_ID") %>%
      arrange(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, TISSUE_NAME, SAMPLE_NO, SPECIMEN_NO) %>%
      select(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, TISSUE_NAME, SAMPLE_NO, SPECIMEN_NO, everything())

    # browser()
    result
  })
  
  #
  # output 'samples_datatable' ----
  #
  output$samples_datatable <- DT::renderDT(
    get_samples(), 
    filter = "top"
  )
  
  #
  # output 'n_specimens_samples' ----
  #
  output$n_specimens_samples <- renderText(
    paste("Downloaded", nrow(get_samples()), 
          "samples from", nrow(get_specimens()), "specimens")
  )
  

  #
  # get_parameters ----
  #
  # Used to fill menu 'parameters_menu', result is also shown in a table in the output
  #
  # Triggered by 'get_samples' (which is triggerd when user clicks button 'download_samples')  
  # Fetches data frame 'get_samples'    
  # - then retrieves SAMPLE_ID  
  # From Nivabasen, gets 
  #   (1) a count per METHOD_ID from BIOTA_CHEMISTRY_VALUES based on SAMPLE_ID 
  #       (this is much faster than getting the actual measurements)   
  #   (2) NAME, UNIT, LABORATORY, MATRIX from METHOD_DEFINITIONS based on the METHOD_ID
  # Adds columns from 2 to 1 using left join 
  # Returns table with one line per method (parameter), including count per parameter    
  
  get_parameters <- reactive({
    
    # Make sure either years or stations is selected
    if (is.null(input$years_menu) & is.null(input$stations_menu)){
      validate(need(FALSE, "Please select years and/or stations"))
    }
    
    df_samples <- get_samples()  
    
    sample_ids <- df_samples %>%
      pull(SAMPLE_ID) %>%
      unique()
    
    result <- get_nivabase_selection(
      "METHOD_ID, count(*) as N",
      "BIOTA_CHEMISTRY_VALUES",
      "SAMPLE_ID",
      sample_ids,
      extra_sql = "group by METHOD_ID"
      )
    
    df_methods <- get_nivabase_selection(
      "METHOD_ID, NAME, UNIT, LABORATORY, MATRIX",
      "METHOD_DEFINITIONS",
      "METHOD_ID",
      result$METHOD_ID)
    
    result <- result %>%
      left_join(
        df_methods, by = "METHOD_ID") %>%
      select(
        NAME, UNIT, LABORATORY, METHOD_ID, MATRIX, N) %>%
      mutate(Menu_string = paste0(NAME, " (", UNIT, "; id:", METHOD_ID, ")"))
    
    # browser()
    result
    
  })
  
  #
  # fill menu 'parameters_menu' ----  
  #
  # Using get_parameters
  #
  observeEvent(get_parameters(), {
    df <- get_parameters()
    params_available <- df %>%
      pull(Menu_string) %>%
      sort()
    updateSelectizeInput(
      inputId = "parameters_menu",
      choices = params_available,
      selected = input$parameters_menu
    )
  })

  
  #
  # output 'parameters_datatable' ----
  #
  output$parameters_datatable <- DT::renderDT(
    get_parameters(), 
    filter = "top"
  )
  
  #
  # get_measurements ----  
  #
  # Used to get measurements for selected stations/years and selected parameters
  # - shown in table  
  #
  # Retrieves get_samples() and get_parameters() data frames  
  # - gets values of SAMPLE_ID (all of them) and the user's selected values of METHOD_ID  
  # From Nivabasen, gets BIOTA_CHEMISTRY_VALUES rows based on SAMPLE_ID and METHOD_ID     
  # - then gets corresponding rows from METHOD_DEFINITIONS and adds NAME, UNIT etc. by left join
  # - from get_samples(), adds STATION_CODE, TAXON_NAME, DATE_CAUGHT, SPECIMEN_NO etc. by left join  
  
  get_measurements <- reactive({
    
    # Make sure either years or stations is selected
    if (is.null(input$years_menu) & is.null(input$stations_menu)){
      validate(need(FALSE, "Please select years and/or stations"))
    }
    validate(need(input$parameters_menu != "", "Please select parameters"))

    df_samples <- get_samples()  
    df_parameters <- get_parameters()
    
    sample_ids <- df_samples %>%
      pull(SAMPLE_ID) %>%
      unique()
    parameter_ids <- df_parameters %>%
      filter(Menu_string %in% input$parameters_menu) %>%
      pull(METHOD_ID) %>%
      unique()

    result <- get_nivabase_selection(
      "SAMPLE_ID, METHOD_ID, VALUE, FLAG1, DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, VALUE_ID",
      "BIOTA_CHEMISTRY_VALUES",
      "SAMPLE_ID",
      sample_ids,
      extra_sql = paste("and METHOD_ID in (", paste(parameter_ids, collapse = ","), ")")
    )

    df_methods <- get_nivabase_selection(
      "METHOD_ID, NAME, UNIT, LABORATORY, MATRIX",
      "METHOD_DEFINITIONS",
      "METHOD_ID",
      unique(result$METHOD_ID))
    
    result <- result %>%
      left_join(
        df_methods, by = "METHOD_ID") %>%
      left_join(
        df_samples %>% select(PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, 
                              SPECIMEN_NO, SPECIMEN_ID, SAMPLE_ID, SAMPLE_NO), 
        by = "SAMPLE_ID") %>%
      select(
        PROJECT_ID, STATION_CODE, STATION_NAME, TAXON_NAME, DATE_CAUGHT, 
        SAMPLE_NO, SPECIMEN_NO, NAME, UNIT, VALUE, FLAG1, 
        DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, 
        LABORATORY, MATRIX,
        SAMPLE_ID, SPECIMEN_ID, METHOD_ID, VALUE_ID)
    
    # browser()
    result
    
  })
  
  
  #
  # output 'measurements_datatable' ----
  #
  output$measurements_datatable <- DT::renderDT(
    get_measurements(), 
    filter = "top"
  )
  


}

# Run the application 
shinyApp(ui = ui, server = server)
