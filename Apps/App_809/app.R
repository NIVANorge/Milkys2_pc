#
# App_801 - Plot medians from Jupyterhub   
#

#
# Code performed when app is starting ----
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)

# For background map
library(maps)
library(mapdata)
simple_map <- map_data("worldHires", c("Norway"))

# Station coordinates
# IN TEST SCRIPT: path changed (../.. removed) 
dat_coord <- read_excel("../../Files_for_other_use/Milkys_stasjoner_for_kart.xlsx") %>%
  select(STATION_CODE, Lat, Long, Station_name)

# Some of the stations are very close together and it's impossible to click a single station
# Therefore, we move some points, and connect them to the original points using a line
# (this is just an example, this should be done also for other clusters of points - see test script)
dat_coord_moved <- tibble::tribble(
  ~STATION_CODE, ~Lat, ~Long,
  "30A", 61.5, 15,
  "30B", 61, 15,
  "I301", 60.5, 15,
  "I304", 60, 15,
  "31A", 59.5, 15,
) 

# Modify the data so we end up with two columns for the original coordinates (_orig),
#   and Long and Lat are modified for staations given by 'dat_coord_moved'  
# Both sets of coordinates will be used when plotting the map 
#   (black dot for the orginal coordinaytes, coloured dot for new coordinates, and a line between)
# Clicking on the *moved* points in the map affects 
dat_coord <- dat_coord %>%
  rename(
    Lat_orig = Lat, 
    Long_orig = Long) %>%
  # New ("moved") coordinates are added as new columns  
  left_join(dat_coord_moved, by = "STATION_CODE") %>%
  # Where there are no new coordinates given by 'dat_coord_moved', the original coordinates will be used
  mutate(
    Lat = ifelse(is.na(Lat), Lat_orig, Lat),
    Long = ifelse(is.na(Long), Long_orig, Long)
  ) 

# Median concentrations (median per year per time series)
dat_med <- readRDS("../../Files_from_Jupyterhub_2020/Raw_data/110_mediandata_updated_2021-10-08.rds") %>%
  # 5 columns (old Coordinates, moved coordinates and Station_name) are added  
  left_join(dat_coord, by = "STATION_CODE") %>%
  # Keep only data for which we have coordinates 
  filter(!is.na(Lat))

# Stations (with coordinates) that are found in the data since 2020  
dat_stations <- dat_med %>%
  filter(MYEAR == 2020) %>%
  distinct(STATION_CODE, Lat_orig, Long_orig, Lat, Long, Station_name, LATIN_NAME) %>%
  # For now we just delete Svalbard (to make the map fit better)
  filter(Lat_orig < 72)

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
# User interface ----  
#
ui <- fluidPage(

    # Application title
    titlePanel("Milkys data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        # Menu for selecting parameter (compound) 
        selectizeInput(inputId = "params_selected", label = "Parameters", 
                       choices = params_avaliable, multiple = TRUE,
                       selected = c("CB118", "CB138")),
        # Menu for selecting basis   
        selectizeInput(inputId = "basis_selected", label = "Basis", 
                       choices = basis_avaliable, multiple = FALSE,
                       selected = "WW"),
        # Menus only affecting the plot     
        sliderInput("min_x", "Show plot from year", min = 1980, max = 2020, value = 1990, sep = "", step = 1),
        numericInput("max_y", "Max y axis", 0),
        width = 4
      ),
      
      # Show plots
      mainPanel(
        # Map
        plotOutput("map", height = "400px", click = "plot_click"), 
        # Timne series plot(s)  
        plotOutput("timeseriesplot", height = "300px"), 
        width = 8
        )
    )
)

#
# Server code ---- 
#
server <- function(input, output) {
  
  # stations_available() is a reactive function that returns
  #   the stations that are available with the given selection of PARAM
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
      # Background map of Norway
      annotation_map(simple_map, fill = "lightgreen") +
      # Actual station position (black dot)
      geom_point(aes(x = Long_orig, y = Lat_orig)) +
      # Line from actual station position to station label
      geom_segment(aes(x = Long_orig, y = Lat_orig, xend = Long, yend = Lat)) +
      # Coloured station dot - this is the one that should be clicked
      geom_point(aes(color = LATIN_NAME), size = 2) +
      # Station code (to the right of the coloured station dot)  
      geom_text(aes(label = STATION_CODE), hjust = 0, vjust = 0.25, nudge_x = 0.35) +
      # Keep proportions (hight/width) of the map:
      coord_fixed(ratio = 2) +
      theme_bw()
      # The map would look better iwith e.g. Lambert projection, but that messes up 
      #   click interaction:
      # coord_map("lambert", parameters = c(10.4, 59.3))
  }) 
  
  output$timeseriesplot <- renderPlot({
    dat_plot_param <- dat_med %>%
      # Select rows by parameter and basis
      filter(PARAM %in% input$params_selected,
             Basis == input$basis_selected) %>%
      # For colouring points by LOQ status 
      mutate(
        Over_LOQ_perc = round(Over_LOQ/N_median*100,0),
        Over_LOQ_med = ifelse(Over_LOQ_perc >= 50, ">=50% over LOQ", "<50% over LOQ")
      )
    # Select rows by station (by clicking on the map)
    dat_plot <- nearPoints(dat_plot_param, 
                           input$plot_click, 
                           xvar = "Long", yvar = "Lat", threshold = 5)   
    if (nrow(dat_plot) > 0){
      # Make ggplot
      gg <- ggplot(dat_plot, aes(MYEAR, Value)) +
        geom_line() +
        geom_point(aes(shape = Over_LOQ_med, fill = Over_LOQ_perc), size = 2) +
        scale_shape_manual(values = c(25,21)) +
        scale_fill_viridis_b() +
        facet_grid(vars(STATION_CODE), vars(PARAM))
      if (input$max_y != 0){
        # Set limits
        gg <- gg + 
          coord_cartesian(xlim = c(input$min_x, 2020), ylim = c(0, input$max_y))
      }
      gg  
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
