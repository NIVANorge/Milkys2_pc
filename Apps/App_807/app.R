#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Reading data directly from Nivabasen including Labware tables"),

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
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
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
  # Get 'stations_available' ----
  #
  # For 'stations_selected' menu
  #
  stations_available <- reactive({
    id <- df_projects %>%
      filter(Menu_string %in% input$projects_selected) %>%
      pull(PROJECT_ID)
    if (length(id) > 0){
      result <- get_nivabase_selection(
      "PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME, STATION_IS_ACTIVE, PROJECTS_STATION_ID",
      "PROJECTS_STATIONS",
      "PROJECT_ID",
      id) %>%
      mutate(
        Menu_string = paste0(STATION_CODE, " ", STATION_NAME, " (ID:", STATION_ID, ")")
        )
    } else {
      result <- data.frame(
        PROJECT_ID = NA, STATION_ID = NA, STATION_CODE = NA, STATION_NAME = NA, STATION_IS_ACTIVE = NA, PROJECTS_STATION_ID = NA,
        Menu_string = NA)
    }
    # browser()
    result
  })

  observeEvent(stations_available(), {
    df_stations <- stations_available()
    stations_available <- df_stations %>% pull(Menu_string)
    updateSelectizeInput(
      inputId = "stations_selected",
      choices = stations_available
      # selected = input$stations_selected
      )
  })
  
  #
  # Get 'specimens_available' ----
  #
  # For 'years_available'  
  #
  specimens_available <- reactive({
    df_stations <- stations_available()
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
  
  observeEvent(specimens_available(), {
    df_specimens <- specimens_available()
    years_available <- df_specimens %>%
      distinct(DATE_CAUGHT) %>%
      mutate(Year = year(DATE_CAUGHT)) %>%
      distinct(Year) %>% 
      pull(Year)
    updateSelectizeInput(
      inputId = "years_selected",
      choices = years_available
      # selected = input$stations_selected
    )
  })
  
  
  #
  # Get 'years_available' ----
  #
  # For 'years_selected' menu
  #
  years_available <- reactive({
    df_specimens <- specimens_available()
    df_specimens %>%
      distinct(DATE_CAUGHT) %>%
      mutate(Year = year(DATE_CAUGHT)) %>%
      distinct(Year) %>% 
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
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
