#
# App_803 - Plot raw data from CEMP Access database   
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 1. Startup ----
# Code that will run one time (when app is starting up)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)

dat <- read_csv("dat.csv")
dat_summ <- read_csv("dat_summ.csv")

species_available <- dat %>%
  count(LATIN_NAME) %>%
  arrange(desc(n)) %>%
  mutate(LATIN_NAME = factor(LATIN_NAME) %>% fct_inorder) %>%
  pull(LATIN_NAME)

params_available <- dat %>%
  distinct(PARAM) %>%
  pull(PARAM)

stations_available <- dat %>%
  distinct(Station_name) %>%
  pull(Station_name)

basis_available <- dat %>%
  distinct(Basis) %>%
  pull(Basis)  

# browser()

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. UI ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

ui <- fluidPage(

    # Application title
    titlePanel("Milkys Access data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "species_selected", label = "Species", 
                       choices = species_available, multiple = TRUE, 
                       selected = "MYTI EDU"),
        selectizeInput(inputId = "params_selected", label = "Parameters", 
                       choices = params_available, multiple = TRUE,
                       selected = c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")),
        selectizeInput(inputId = "basis_selected", label = "Basis", 
                       choices = basis_available, multiple = FALSE,
                       selected = "W"),
        width = 4
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "All stations", 
            plotOutput("overviewplot", height = "700px"),
            sliderInput("min_no_years", "Show stations with at least x years", min = 1, max = 40, value = 1, sep = "", step = 1)
          ),
          tabPanel(
            "Time series", plotOutput("timeseriesplot", height = "700px"),
            selectizeInput(inputId = "stations_selected", label = "Stations", 
                           choices = NULL, multiple = TRUE),
            sliderInput("range_x", "Show plot for years", min = 1980, max = 2020, value = c(1980,2020), sep = "", step = 1),
            numericInput("max_y", "Max y axis", 0)
          ) 
        ),
        width = 8
        )
    )
)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3. Server ----
#    Define server logic 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

server <- function(input, output) {
  
  # 3a. stations_avaliable ----
  #     used in stations menu below (3b)
  stations_avaliable <- reactive({
    dat %>%
      filter(LATIN_NAME %in% input$species_selected) %>%
      distinct(Station_name) %>%
      pull(Station_name)
  })
  
  # 3b. stations menu ----
  observeEvent(stations_avaliable(), {
    updateSelectizeInput(
      inputId = "stations_selected",
      choices = stations_avaliable(),
      selected = input$stations_selected,     # => selected stations are not 'lost' if another species is selected 
      options = list(render = I(
        '{
    option: function(item, escape) {
      return "<div><strong>" + escape(item.value) + "</strong>"
    }
  }'))
    )
  })
  
  # 3c. overviewplot ----  
  output$overviewplot <- renderPlot({
    
    dat_plot1 <- dat_summ %>%
      filter(PARAM %in% input$params_selected,
             Basis %in% input$basis_selected)
    # browser()
    
    # min_no_years <- 1
    series_lasting_until <- 1980
    
    gg <- dat_plot1 %>%
      mutate(Station_name = fct_rev(Station_name)) %>%
      filter(No_years >= input$min_no_years,
             Last_year >= series_lasting_until) %>%
      ggplot(aes(Year, Station_name)) +
      geom_raster(aes(fill = log10(Median_value))) +
      scale_fill_viridis_c() +
      geom_point(data = dat_plot1 %>% filter(Prop_over_LOQ > 0.5, 
                                             No_years >= input$min_no_years,
                                             Last_year >= series_lasting_until))
    
    gg  
    
  })
  
  # 3d. timeseriesplot ----  
  output$timeseriesplot <- renderPlot({
    
    dat_plot2 <- dat %>%
      filter(PARAM %in% input$params_selected,
             Basis %in% input$basis_selected,
             Station_name %in% input$stations_selected) %>%
      mutate(LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ"))
    # browser()
    
    gg <- ggplot(dat_plot2, aes(Year, value)) +
      geom_smooth(se = FALSE, method = 'loess', formula = 'y ~ x') +
      geom_point(aes(shape = LOQ, color = Basis), size = 2) +
      scale_shape_manual(values = c(19,25)) +
      scale_fill_viridis_b() +
      facet_grid(vars(Station_name), vars(PARAM))
    if (input$max_y != 0){
      if (input$range_x[1] == 1980 & input$range_x[2] == 2020){
      gg <- gg + 
        coord_cartesian(ylim = c(0, input$max_y))
      } else {
        gg <- gg + 
          coord_cartesian(xlim = input$range_x, ylim = c(0, input$max_y))
      }
    } else {
      if (input$range_x[1] != 1980 | input$range_x[2] != 2020){
        gg <- gg + 
          coord_cartesian(xlim = input$range_x)
      }
    }
    gg  
  })
  
}

# 4. Run the application ----
shinyApp(ui = ui, server = server)
