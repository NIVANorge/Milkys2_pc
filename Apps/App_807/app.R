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
          selectizeInput(inputId = "projects_selected", label = "Projects", 
                         choices = NULL, multiple = TRUE),
          selectizeInput(inputId = "stations_selected", label = "Stations", 
                         choices = NULL, multiple = TRUE),
          selectizeInput(inputId = "years_selected", label = "Years", 
                         choices = NULL, multiple = TRUE),
          sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
          width = 4
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # Tabset panel
            tabsetPanel(
              type = "tabs",
              # .. Tab 1 (plot 1) ----
              tabPanel(
                plotOutput("station_years_plot", width = "600px"),
                div(DTOutput("station_years_datatable"), style = "font-size:90%")
              ) # end tabPanel 1
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
  # Update projects_available
  #
  updateSelectizeInput(
    session,
    inputId = "projects_selected",
    choices = projects_available,
    server = TRUE)
  
  #
  # get_stations ----
  #
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
  # Count of number of 
  get_stations_years <- reactive({
    df_stations <- get_stations() %>%
      distinct(STATION_ID, STATION_CODE, STATION_NAME)
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
        df_stations %>% select(STATION_ID, STATION_CODE, STATION_NAME),
        by = "STATION_ID") %>%
      select(STATION_CODE, STATION_NAME, STATION_ID, YEAR, N)
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
  # For 'years_available'  
  #
  get_specimens <- reactive({
    df_stations <- get_stations()
    id <- df_stations %>%
      filter(Menu_string %in% input$stations_selected) %>%
      pull(STATION_ID)
    if (length(id) > 0){
      result <- get_nivabase_selection(
        "STATION_ID, DATE_CAUGHT, SPECIMEN_NO, TAXONOMY_CODE_ID, SPECIMEN_ID",
        "BIOTA_SINGLE_SPECIMENS",
        "STATION_ID",
        id)
    } else {
      result <- data.frame(
        STATION_ID = NA, DATE_CAUGHT = NA, SPECIMEN_NO = NA, TAXONOMY_CODE_ID = NA, SPECIMEN_ID = NA)
    }
    # browser()
    result
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
