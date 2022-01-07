#
# App_804 - Plot raw data from NIVA (RData file from Jupyterhub)   
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)

#
# These files are too big to put on Github
# NIVA people can find them here: 'K:\Avdeling\214-Oseanografi\DHJ\Data\Milkys_ICES_files'
#  

#
#  . Raw data ----
#  App reads from file made below
#

#
# NOTE: The files used (and made below) are too big to put on Github
# NIVA people can find it on Jupyterhub (DHJ/Milkys2)
# 

dat <- readRDS("../../Files_from_Jupyterhub_2020/Raw_data/109_adjusted_data_2021-09-15.rds") %>%
  mutate(
    VALUE_ORIG = VALUE_WW,     # cheating a bit here
    Basis = "W"                # ... and here
  ) %>%
  rename(
    qflag = FLAG1)

# Station names
dat_stations <- readxl::read_excel("../../Files_for_other_use/Milkys_stasjoner_for_kart.xlsx")


# Add station names
dat <- dat %>%
  left_join(dat_stations %>% select(STATION_CODE, Station_name), by = "STATION_CODE") %>%
  mutate(Station_name = paste(STATION_CODE, Station_name))

# Summary data
dat_summ <- read_csv("109_adjusted_data_summ.csv")
  
  
#
species_available <- dat %>%
  count(LATIN_NAME) %>%
  arrange(desc(n)) %>%
  mutate(LATIN_NAME = factor(LATIN_NAME) %>% fct_inorder) %>%
  pull(LATIN_NAME)

speciesgroup_available <- dat_summ %>%
  count(Speciesgroup) %>%
  arrange(desc(n)) %>%
  mutate(Speciesgroup = factor(Speciesgroup) %>% fct_inorder) %>%
  pull(Speciesgroup)

params_available <- dat %>%
  distinct(PARAM) %>%
  pull(PARAM)

stations_available <- dat %>%
  distinct(Station_name) %>%
  pull(Station_name)

basis_available <- dat %>%
  distinct(Basis) %>%
  pull(Basis)  

#
# UI ---- 
#
ui <- fluidPage(
  
  # Application title
  titlePanel("Jupyterhub adjusted raw data (from script 109)"),
  
  # . Sidebar ---- 
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "params_selected", label = "Parameters", 
                     choices = params_available, multiple = TRUE,
                     selected = c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")),
      selectizeInput(inputId = "basis_selected", label = "Original basis of measurement", 
                     choices = basis_available, multiple = TRUE,
                     selected = basis_available),
      selectInput(inputId = "valuebasis_selected", label = "Concentration basis", 
                  choices = c("Wet weight basis", "Dry weight basis", "Lipid weight basis", "Original value"), 
                  selected = c("Wet weight basis")),
      width = 4
    ),
    
    # . Main panel ----
    mainPanel(
      
      # Main tabset panel
      tabsetPanel(
        type = "tabs",
        
        # Main panel 1 (plot 1)
        tabPanel(
          "All stations", 
          selectizeInput(inputId = "speciesgroup_selected", label = "Species group", 
                         choices = speciesgroup_available, 
                         multiple = TRUE,
                         selected = c("Cod", "Blue mussel", "Flatfish", "Snail", "Eider duck")),
          plotOutput("overviewplot", height = "700px"),
          sliderInput("min_no_years", "Show only stations with at least x years", min = 1, max = 40, value = 1, sep = "", step = 1)
        ),
        
        # Main panel 2
        tabPanel(
          "Selected station(s) and tissue(s)",
          
          # Menus that are in common for plots 2 and 3 (subpanel 2a and 2b)
          selectizeInput(inputId = "species_selected", label = "Species", 
                         choices = species_available, multiple = TRUE, 
                         selected = c("Gadus morhua", "Mytilus edulis")),
          selectizeInput(inputId = "stations_selected", label = "Stations", 
                         choices = NULL, multiple = TRUE),
          selectizeInput(inputId = "tissues_selected", label = "Tissues", 
                         choices = NULL, multiple = TRUE),
          tabsetPanel(
            
            
            # Subpanel 2a (plot 2)
            tabPanel(
              "Time series", 
              plotOutput("timeseriesplot", height = "600px"),
              checkboxInput("p2_logscale_y", "Use log10 on y axis ('Max y axis' must be set to zero)", value = FALSE),
              sliderInput("range_x", "Show plot for years (for auto, set to 1980-2020)", 
                          min = 1980, max = 2020, value = c(1980,2020), sep = "", step = 1),
              numericInput("max_y", "Max y axis (for auto, set to 0)", 0),
              checkboxInput("number_of_points", "Show number of overplotted points")
            ),
            
            # Subpanel 3a (plot 3)
            tabPanel(
              "Scatter plots by year",
              selectizeInput(inputId = "p3_params_selected_x", label = "Parameter(s) for x axis", 
                             choices = params_available, multiple = TRUE,
                             selected = c("EXLIP%", "FATWT%")),
              selectizeInput(inputId = "p3_params_selected_y", label = "Parameter(s) for y axis", 
                             choices = params_available, multiple = TRUE,
                             selected = "CB118"),
              selectInput(inputId = "p3_valuebasis_selected_x", label = "Concentration basis for x axis", 
                          choices = c("Wet weight basis", "Dry weight basis", "Lipid weight basis", "Original value"), 
                          selected = c("Wet weight basis")),
              selectInput(inputId = "p3_valuebasis_selected_y", label = "Concentration basis for y axis", 
                          choices = c("Wet weight basis", "Dry weight basis", "Lipid weight basis", "Original value"), 
                          selected = c("Wet weight basis")),
              plotOutput("correlationplot", height = "500px"),
              selectInput(inputId = "p3_selectplot", label = "Concentration basis for x axis", 
                          choices = c("No subplots", "Subplots per year, common x+y", 
                                      "Subplots per year, free y", "Subplots per year, free x", 
                                      "Subplots per year, free x+y"), 
                          selected = c("Subplots per year, free y")),
              checkboxInput("p3_logscale_x", "Use log10 on x axis", value = FALSE),
              checkboxInput("p3_logscale_y", "Use log10 on y axis", value = FALSE)
            )
            
          )  # end tabsetPanel
        ) # end main tabset 2
        
      ),  # end tabsetPanel
      width = 8
    )  # end mainPanel
  )
)

#
# SERVER ---- 
#
server <- function(input, output) {
  
  #
  # Get 'stations_avaliable' ----
  #
  # For 'stations_selected' menu
  #
  stations_avaliable <- reactive({
    dat %>%
      filter(LATIN_NAME %in% input$species_selected) %>%
      distinct(Station_name) %>%
      pull(Station_name)
  })
  
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
  
  #
  # Get 'tissues_avaliable' ----
  #
  # For 'tissues_selected' menu
  #
  tissues_avaliable <- reactive({
    dat %>%
      filter(LATIN_NAME %in% input$species_selected,
             PARAM %in% input$params_selected & Station_name %in% input$stations_selected) %>%
      distinct(TISSUE_NAME) %>%
      pull(TISSUE_NAME)
  })
  
  observeEvent(tissues_avaliable(), {
    updateSelectizeInput(
      inputId = "tissues_selected",
      choices = tissues_avaliable(),
      selected = input$tissues_selected,     # => selected tissues are not 'lost' if another species is selected 
      options = list(render = I(
        '{
    option: function(item, escape) {
      return "<div><strong>" + escape(item.value) + "</strong>"
    }
  }'))
    )
  })
  
  #
  # Plot 1 (overviewplot) ----
  #
  output$overviewplot <- renderPlot({
    
    # browser()
    
    dat_plot1 <- dat_summ %>%
      filter(PARAM %in% input$params_selected,
             Basis %in% input$basis_selected,
             Speciesgroup %in% input$speciesgroup_selected)
    
    if (input$valuebasis_selected == "Wet weight basis"){
      dat_plot1$Value_plot <- dat_plot1$Value_wet_median
    } else  if (input$valuebasis_selected == "Dry weight basis"){
      dat_plot1$Value_plot <- dat_plot1$Value_dry_median
    } else if (input$valuebasis_selected == "Lipid weight basis"){
      dat_plot1$Value_plot <- dat_plot1$Value_lip_median
    } else if (input$valuebasis_selected == "Original value"){
      dat_plot1$Value_plot <- dat_plot1$Value_orig_median
    }
    
    # browser()
    
    # min_no_years <- 1
    series_lasting_until <- 1980
    
    gg <- dat_plot1 %>%
      mutate(Station_name = fct_rev(Station_name)) %>%
      filter(No_years >= input$min_no_years,
             Last_year >= series_lasting_until) %>%
      ggplot(aes(MYEAR, Station_name)) +
      geom_raster(aes(fill = log10(Value_plot))) +
      scale_fill_viridis_c("log10(concentration)") +
      geom_point(data = dat_plot1 %>% filter(Prop_over_LOQ > 0.5, 
                                             No_years >= input$min_no_years,
                                             Last_year >= series_lasting_until)) +
      labs(x = "Year", y = "Station")
    
    gg   
    
  })
  
  #
  # Plot 2 (timeseriesplot) ----
  #
  
  # Plot below needs to be wrapped in observe() as 'height' uses an input value
  observe({         
    output$timeseriesplot <- renderPlot({
      
      validate(
        need(input$stations_selected != "", "Please select at least one station")
      )
      validate(
        need(input$tissues_selected != "", "Please select at least one tissue")
      )
      
      # Code in app, unchanged
      dat_plot2 <- dat %>%
        filter(PARAM %in% input$params_selected,
               Basis %in% input$basis_selected,
               LATIN_NAME %in% input$species_selected,
               TISSUE_NAME %in% input$tissues_selected,
               Station_name %in% input$stations_selected) %>%
        mutate(
          LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ"),
          Series = paste0(LATIN_NAME, ", ", 
                          TISSUE_NAME, ", ", 
                          Basis, "-basis")
        )
      
      if (input$valuebasis_selected == "Wet weight basis"){
        dat_plot2$Value_plot <- dat_plot2$VALUE_WW
      } else  if (input$valuebasis_selected == "Dry weight basis"){
        dat_plot2$Value_plot <- dat_plot2$VALUE_DW
      } else if (input$valuebasis_selected == "Lipid weight basis"){
        dat_plot2$Value_plot <- dat_plot2$VALUE_FB
      } else if (input$valuebasis_selected == "Original value"){
        dat_plot2$Value_plot <- dat_plot2$VALUE_ORIG
      }
      
      # browser()
      
      resultplot <- ggplot(dat_plot2, aes(MYEAR, Value_plot)) +
        geom_point(aes(shape = LOQ, color = Series), size = 2) +
        geom_smooth(se = FALSE, method = 'loess', formula = 'y ~ x') +
        scale_shape_manual(values = c(19,25)) +
        scale_fill_viridis_b() +
        facet_grid(vars(Station_name), vars(PARAM)) +
        labs(x = "Year",
             y = paste0("Concentration (", tolower(input$valuebasis_selected), ")"))
      
      if (input$number_of_points)
        resultplot <- resultplot +
        geom_text(
          data = dat_plot2 %>% count(MYEAR, Value_plot) %>% filter(n > 1),  
          aes(x = MYEAR + 0.3, label = n))
      
      if (input$max_y != 0){
        if (input$range_x[1] == 1980 & input$range_x[2] == 2020){
          resultplot <- resultplot + 
            coord_cartesian(ylim = c(0, input$max_y))
        } else {
          resultplot <- resultplot + 
            coord_cartesian(xlim = input$range_x, ylim = c(0, input$max_y))
        }
      } else {
        if (input$range_x[1] != 1980 | input$range_x[2] != 2020){
          resultplot <- resultplot + 
            coord_cartesian(xlim = input$range_x)
        }
      }
      
      if (input$p2_logscale_y){
        resultplot <- resultplot + scale_y_log10()
      }
      
      resultplot  
    }, 
    height = case_when(
      length(input$stations_selected) == 1 ~ 300,
      length(input$stations_selected) == 2 ~ 400,
      length(input$stations_selected) == 3 ~ 500,
      TRUE ~ 600)   # let height vary
    )
  })  # end of observe  
  
  
  #
  # Plot 3 (correlationplot) ----
  #
  
  
  
  # . Get plot data (dat_plot3b) ----
  dat_plot3b <- reactive({
    
    
    dat_plot3a <- dat %>%
      filter(PARAM %in% c(input$p3_params_selected_x, input$p3_params_selected_y),
             LATIN_NAME %in% input$species_selected,
             TISSUE_NAME %in% input$tissues_selected,
             Station_name %in% input$stations_selected) %>%
      mutate(
        LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ"),
        Series = paste0(LATIN_NAME, ", ", 
                        TISSUE_NAME, ", ", 
                        Basis, "-basis")
      )
    
    # xtabs(~MYEAR + PARAM, dat_plot3b)
    # dat_plot3b %>% filter(MYEAR == 2000) %>% View()
    
    if (input$p3_valuebasis_selected_x == "Wet weight basis"){
      dat_plot3a$Value_plot_x <- dat_plot3a$VALUE_WW
    } else  if (input$p3_valuebasis_selected_x == "Dry weight basis"){
      dat_plot3a$Value_plot_x <- dat_plot3a$VALUE_DW
    } else if (input$p3_valuebasis_selected_x == "Lipid weight basis"){
      dat_plot3a$Value_plot_x <- dat_plot3a$VALUE_FB
    } else if (input$p3_valuebasis_selected_x == "Original value"){
      dat_plot3a$Value_plot_x <- dat_plot3a$VALUE_ORIG
    }
    
    if (input$p3_valuebasis_selected_y == "Wet weight basis"){
      dat_plot3a$Value_plot_y <- dat_plot3a$VALUE_WW
    } else  if (input$p3_valuebasis_selected_y == "Dry weight basis"){
      dat_plot3a$Value_plot_y <- dat_plot3a$VALUE_DW
    } else if (input$p3_valuebasis_selected_y == "Lipid weight basis"){
      dat_plot3a$Value_plot_y <- dat_plot3a$VALUE_FB
    } else if (input$p3_valuebasis_selected_y == "Original value"){
      dat_plot3a$Value_plot_y <- dat_plot3a$VALUE_ORIG
    }
    
    # browser()
    
    df_x <- dat_plot3a %>%
      filter(PARAM %in% input$p3_params_selected_x) %>%
      select(PARAM, MYEAR, Basis, LATIN_NAME, TISSUE_NAME, Station_name, smpno, subno, sub.sample, Value_plot_x, LOQ) %>%
      mutate(Under_LOQ_x = ifelse(LOQ == "Under LOQ", "x", ""))
    df_y <- dat_plot3a %>%
      filter(PARAM %in% input$p3_params_selected_y) %>%
      select(PARAM, MYEAR, Basis, LATIN_NAME, TISSUE_NAME, Station_name, smpno, subno, sub.sample, Value_plot_y, LOQ) %>%
      mutate(Under_LOQ_y = ifelse(LOQ == "Under LOQ", "y", ""))
    
    # df_x %>% filter(MYEAR == 2000) %>% View()
    df_x %>% filter(MYEAR == 2000) %>% head()
    df_y %>% filter(MYEAR == 2000) %>% head()
    
    result <- inner_join(
      df_x %>% select(-LOQ), 
      df_y %>% select(-LOQ),
      by = c("MYEAR", "LATIN_NAME", "TISSUE_NAME", "Station_name", "smpno", "subno", "sub.sample")) %>%
      mutate(
        Under_LOQ = case_when(
          Under_LOQ_x == "" & Under_LOQ_y == "" ~ "None under",
          TRUE ~ paste0(Under_LOQ_x, Under_LOQ_y, " under")
        )
      )
    
    # browser()
    
    result
    
  })
  
  # . Make plot ----
  
  output$correlationplot <- renderPlot({
    
    data_for_plot <- dat_plot3b()
    
    # browser()
    
    paramtext_x <- paste(input$p3_params_selected_x, collapse = ", ")
    paramtext_y <- paste(input$p3_params_selected_y, collapse = ", ")
    
    gg1 <- ggplot(data_for_plot, aes(Value_plot_x, Value_plot_y)) +
      geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x') +
      geom_point(aes(shape = Under_LOQ, color = MYEAR), size = 2) +
      scale_shape_manual(values = c(`None under` = 19, `x under` = 120, `y under` = 121, `xy under` = 6)) +
      scale_color_viridis_c("Year") +
      labs(
        x = paste0(paramtext_x, " (", input$p3_valuebasis_selected_x, ")"),
        y = paste0(paramtext_y, " (", input$p3_valuebasis_selected_y, ")")
      )
    
    gg2 <- ggplot(data_for_plot, aes(Value_plot_x, Value_plot_y)) +
      geom_smooth(se = FALSE, method = 'lm', formula = 'y ~ x') +
      geom_point(aes(shape = Under_LOQ), size = 2) +
      scale_shape_manual(values = c(`None under` = 19, `x under` = 120, `y under` = 121, `xy under` = 6)) +
      labs(
        x = paste0(paramtext_x, " (", input$p3_valuebasis_selected_x, ")"),
        y = paste0(paramtext_y, " (", input$p3_valuebasis_selected_y, ")")
      )
    
    if (input$p3_selectplot == "No subplots"){
      resultplot <- gg1 
    } else if (input$p3_selectplot == "Subplots per year, common x+y"){
      resultplot <- gg2 + facet_wrap(vars(MYEAR))
    } else if (input$p3_selectplot == "Subplots per year, free y"){
      resultplot <- gg2 + facet_wrap(vars(MYEAR), scales = "free_y")
    } else if (input$p3_selectplot == "Subplots per year, free x"){
      resultplot <- gg2 + facet_wrap(vars(MYEAR), scales = "free_x")
    } else if (input$p3_selectplot == "Subplots per year, free x+y"){
      resultplot <- gg2 + facet_wrap(vars(MYEAR), scales = "free")
    } 
    
    if (input$p3_logscale_x){
      resultplot <- resultplot + scale_x_log10()
    }
    if (input$p3_logscale_y){
      resultplot <- resultplot + scale_y_log10()
    }
    
    resultplot
    
  })
  
}  # end of server  



# Run the application ----
shinyApp(ui = ui, server = server)

