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

dat_med <- readRDS("../Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds")

# Parameters to choose from (only those found in 2020)
species_avaliable <- dat_med %>%
  filter(MYEAR == 2020) %>%
  distinct(LATIN_NAME) %>%
  pull(LATIN_NAME)
params_avaliable <- dat_med %>%
  filter(MYEAR == 2020) %>%
  distinct(PARAM) %>%
  pull(PARAM)
basis_avaliable <- dat_med %>%
  filter(MYEAR == 2020) %>%
  distinct(Basis) %>%
  pull(Basis)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Milkys data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "species_selected", label = "Species", 
                       choices = species_avaliable, multiple = TRUE, 
                       selected = "Mytilus edulis"),
        selectizeInput(inputId = "params_selected", label = "Parameters", 
                       choices = params_avaliable, multiple = TRUE,
                       selected = c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")),
        selectizeInput(inputId = "stations_selected", label = "Stations", 
                       choices = NULL, multiple = TRUE),
        selectizeInput(inputId = "basis_selected", label = "Basis", 
                       choices = basis_avaliable, multiple = FALSE,
                       selected = "WW"),
        sliderInput("min_x", "Show plot from year", min = 1980, max = 2020, value = 1990, sep = "", step = 1),
        numericInput("max_y", "Max y axis", 0),
        width = 4
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("timeseriesplot", height = "700px"), 
        width = 8
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  stations_avaliable <- reactive({
    dat_med %>%
      filter(MYEAR == 2020 & LATIN_NAME %in% input$species_selected) %>%
      distinct(STATION_CODE) %>%
      pull(STATION_CODE)
  })
  
  observeEvent(stations_avaliable(), {
    updateSelectizeInput(
      inputId = "stations_selected",
      choices = stations_avaliable(),
      options = list(render = I(
        '{
    option: function(item, escape) {
      return "<div><strong>" + escape(item.value) + "</strong>"
    }
  }'))
    )
  })
  
  output$timeseriesplot <- renderPlot({
    dat_select1 <- dat_med %>%
      filter(PARAM %in% input$params_selected,
             Basis == input$basis_selected)
    dat_plot <- dat_select1 %>%
      filter(STATION_CODE %in% input$stations_selected) %>%
      mutate(
        Over_LOQ_perc = round(Over_LOQ/N_median*100,0),
        Over_LOQ_med = ifelse(Over_LOQ_perc >= 50, ">=50% over LOQ", "<50% over LOQ")
      )
    gg <- ggplot(dat_plot, aes(MYEAR, Value)) +
      geom_line() +
      geom_point(aes(shape = Over_LOQ_med, fill = Over_LOQ_perc), size = 2) +
      scale_shape_manual(values = c(25,21)) +
      scale_fill_viridis_b() +
      facet_grid(vars(STATION_CODE), vars(PARAM))
    if (input$max_y != 0){
      gg <- gg + 
        coord_cartesian(xlim = c(input$min_x, 2020), ylim = c(0, input$max_y))
    }
    gg  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
