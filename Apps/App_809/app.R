#
# App_801 - Plot medians from Jupyterhub   
#

#
# Code to perform when app is starting
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)

# For background map
library(maps)
library(mapdata)
simple_map <- map_data("worldHires", c("Norway"))

dat_coord <- read_excel("../../Files_for_other_use/Milkys_stasjoner_for_kart.xlsx") %>%
  select(STATION_CODE, Lat, Long, Station_name)

dat_coord_moved <- tibble::tribble(
  ~STATION_CODE, ~Lat, ~Long,
  "30A", 61.5, 15,
  "30B", 61, 15,
  "I301", 60.5, 15,
  "I304", 60, 15,
  "I306", 59.5, 15,
  "I307", 59, 15,
  "31A", 58.5, 15,
) 

dat_coord <- dat_coord %>%
  rename(
    Lat_orig = Lat, 
    Long_orig = Long) %>%
  left_join(dat_coord_moved, by = "STATION_CODE") %>%
  mutate(
    Lat = ifelse(is.na(Lat), Lat_orig, Lat),
    Long = ifelse(is.na(Long), Long_orig, Long)
  ) 

dat_med <- readRDS("../../Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds") %>%
  left_join(dat_coord, by = "STATION_CODE") %>%
  filter(!is.na(Lat))

dat_stations <- dat_med %>%
  filter(MYEAR == 2020) %>%
  distinct(STATION_CODE, Lat_orig, Long_orig, Lat, Long, Station_name, LATIN_NAME) %>%
  # For now we just delete Svalbard (to make the map fit better)
  filter(Lat_orig < 72)
  # filter(Lat > 65 & Lat < 68)

# Parameters to choose from (only those found in 2020)
params_avaliable <- dat_med %>%
  filter(MYEAR >= 2020) %>%
  distinct(PARAM) %>%
  pull(PARAM)
basis_avaliable <- dat_med %>%
  filter(MYEAR >= 2020) %>%
  distinct(Basis) %>%
  pull(Basis)

#
# User interface  
#
ui <- fluidPage(

    # Application title
    titlePanel("Milkys data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "params_selected", label = "Parameters", 
                       choices = params_avaliable, multiple = TRUE,
                       selected = c("CB118", "CB138")),
        selectizeInput(inputId = "basis_selected", label = "Basis", 
                       choices = basis_avaliable, multiple = FALSE,
                       selected = "WW"),
        sliderInput("min_x", "Show plot from year", min = 1980, max = 2020, value = 1990, sep = "", step = 1),
        numericInput("max_y", "Max y axis", 0),
        width = 4
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("map", height = "400px", click = "plot_click"), 
        plotOutput("timeseriesplot", height = "300px"), 
        verbatimTextOutput("table"),
        width = 8
        )
    )
)

#
# Server code  
#
server <- function(input, output) {
  
  stations_avaliable <- reactive({
    dat_med %>%
      filter(MYEAR >= 2020, 
             PARAM %in% input$params_selected,
             Basis == input$basis_selected) %>%
      distinct(STATION_CODE) %>%
      pull(STATION_CODE)
  })
  
  output$map <- renderPlot({
    # Map from library(maps)
    dat_stations_available <- dat_stations %>% 
      filter(STATION_CODE %in% stations_avaliable())
    ggplot(dat_stations_available, aes(Long, Lat)) +
      annotation_map(simple_map, fill = "lightgreen") +
      geom_point(aes(x = Long_orig, y = Lat_orig)) +
      geom_segment(aes(x = Long_orig, y = Lat_orig, xend = Long, yend = Lat)) +
      geom_point(aes(color = LATIN_NAME)) +
      geom_text(aes(label = STATION_CODE), hjust = 0, vjust = 0.25, nudge_x = 0.35) 
      # coord_map("lambert", parameters = c(10.4, 59.3))
  }) 
  
  output$timeseriesplot <- renderPlot({
    dat_plot_param <- dat_med %>%
      filter(PARAM %in% input$params_selected,
             Basis == input$basis_selected) %>%
      mutate(
        Over_LOQ_perc = round(Over_LOQ/N_median*100,0),
        Over_LOQ_med = ifelse(Over_LOQ_perc >= 50, ">=50% over LOQ", "<50% over LOQ")
      )
    dat_plot <- nearPoints(dat_plot_param, 
                           input$plot_click, 
                           xvar = "Long", yvar = "Lat", threshold = 5)   
    if (nrow(dat_plot) > 0){
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
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
