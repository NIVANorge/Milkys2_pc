#
# App_802 - Plot raw data from ICES (csv file from Rob)   
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(DT)

#
# These files are too big to put on Github
# NIVA people can find them here: 'K:\Avdeling\214-Oseanografi\DHJ\Data\Milkys_ICES_files'
#  
dat <- read_csv("../../Files_to_ICES/Data_from_ICES/data_extraction_211008/Norway_added_columns.csv") %>%
  mutate(Speciestissue = paste0(LATIN_NAME, ", ", TISSUE_NAME))
dat_summ <- read_csv("../../Files_to_ICES/Data_from_ICES/data_extraction_211008/Norway_summ.csv")

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

selection_from_start_param <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")
selection_from_start_param <- "PFOS"

#
# UI ---- 
#
ui <- fluidPage(

    # Application title
    titlePanel("OSPAR/ICES data"),

    # . Sidebar ---- 
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "params_selected", label = "Parameters", 
                       choices = params_available, multiple = TRUE,
                       selected = selection_from_start_param),
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
          
          # .. Main panel 1 (plot 1) ----
          tabPanel(
            "All stations", 
            selectizeInput(inputId = "speciesgroup_selected", label = "Species group", 
                           choices = speciesgroup_available, 
                           multiple = TRUE,
                           selected = c("Cod", "Blue mussel", "Flatfish", "Snail", "Eider duck")),
            plotOutput("overviewplot", height = "700px"),
            checkboxInput("p1_show_medians", "Rectangles showing median value", value = TRUE),
            checkboxInput("p1_show_50perc_overLOQ", "Points showing >50% over LOQ", value = TRUE),
            checkboxInput("p1_show_min_overLOQ", "Points showing smallest value over LOQ", value = FALSE),
            checkboxInput("p1_show_number_overLOQ", "Numbers showing number over LOQ", value = FALSE),
            sliderInput("min_no_years", "Show only stations with at least x years", min = 1, max = 40, value = 1, sep = "", step = 1)
          ),
          
          # .. Main panel 2 (plot 2 and 3) ----
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
                uiOutput("timeseriesplot_ui"),
                checkboxInput("p2_logscale_y", "Use log10 on y axis ('Max y axis' must be set to zero)", value = FALSE),
                sliderInput("range_x", "Show plot for years (for auto, set to 1980-2020)", 
                            min = 1980, max = 2020, value = c(1980,2020), sep = "", step = 1),
                numericInput("max_y", "Max y axis (for auto, set to 0)", 0),
                checkboxInput("number_of_points", "Show number of overplotted points"),
                div(DTOutput("timeseriesdata"), style = "font-size:90%")
              ),  # end subpanel 2a
              
              # Subpanel 2b (plot 3)
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
                checkboxInput("p3_logscale_y", "Use log10 on y axis", value = FALSE),
                sliderInput("p3_range_years", "Show plot for years (for auto, set to 1980-2020)", 
                            min = 1980, max = 2020, value = c(1980,2020), sep = "", step = 1)
              ) # end subpanel 2b (no comma since it's the last)
              
            )  # end tabsetPanel for plots 2a + 2b 
            
          ), # end main tab 2
          
          # .. Main panel 3 (plot 4) ----
          tabPanel(
            "Selected parameter at several stations",
            selectizeInput(inputId = "p4_years_selected", label = "Year(s)", 
                           choices = 1981:2020, multiple = TRUE,
                           selected = 2020),
            selectizeInput(inputId = "p4_speciestissue_selected", label = "Species and tissue", 
                           choices = NULL, multiple = TRUE),
            plotOutput("stationplot", height = "500px"),
            checkboxInput("p4_logscale_y", "Use log10 on y axis", value = FALSE),
            div(DTOutput("stationdata"), style = "font-size:90%")
          )
          
        ),  # end entire tabsetPanel
        width = 8
      )  # end mainPanel
    )
)

#
# SERVER ---- 
#
server <- function(input, output) {
  
  #
  # Get 'stations_available' ----
  #
  # For 'stations_selected' menu
  #
  stations_available <- reactive({
    dat %>%
      filter(LATIN_NAME %in% input$species_selected,
             PARAM %in% input$params_selected) %>%
      distinct(Station_name) %>%
      pull(Station_name)
  })
  
  observeEvent(stations_available(), {
    updateSelectizeInput(
      inputId = "stations_selected",
      choices = stations_available(),
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
  # Get 'tissues_available' ----
  #
  # For 'tissues_selected' menu
  #
  tissues_available <- reactive({
    dat %>%
      filter(LATIN_NAME %in% input$species_selected,
             PARAM %in% input$params_selected & Station_name %in% input$stations_selected) %>%
      distinct(TISSUE_NAME) %>%
      pull(TISSUE_NAME)
  })
  
  observeEvent(tissues_available(), {
    updateSelectizeInput(
      inputId = "tissues_selected",
      choices = tissues_available(),
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
  # Get dat_param ----
  #
  dat_param_function <- reactive({
    
    # Code in app, unchanged
    result <- dat %>%
      filter(PARAM %in% input$params_selected,
             Basis %in% input$basis_selected) %>%
      mutate(
        LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ"),
        Series = paste0(LATIN_NAME, ", ", 
                        TISSUE_NAME)
      )
    
    result
    
  })
  
  #
  # Get dat_plot2 ----
  #
  dat_plot2_function <- reactive({
    
    validate(
      need(input$stations_selected != "", "Please select at least one station")
    )
    validate(
      need(input$tissues_selected != "", "Please select at least one tissue")
    )
    
    dat_param <- dat_param_function()
    
    # Code in app, unchanged
    result <- dat_param %>%
      filter(LATIN_NAME %in% input$species_selected,
             TISSUE_NAME %in% input$tissues_selected,
             Station_name %in% input$stations_selected) %>%
      mutate(
        LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ"),
        Series = paste0(LATIN_NAME, ", ", 
                        TISSUE_NAME, ", ", 
                        Basis, "-basis")
      )
    
    result
    
  })
  
  #
  # Get dat_plot1 ----
  # FOR DYNAMIC CALCULATION OF MEDIANS (not used now)
  #
  dat_plot1_function <- reactive({
    
    dat_param <- dat_param_function()
    
    result <- dat_param %>%
      group_by(PARAM, Basis, Station_name, TISSUE_NAME, MYEAR) %>%
      summarize(
        N = n(),
        Over_LOQ = sum(is.na(qflag)),
        Prop_over_LOQ = Over_LOQ/N,
        Value_orig_median = median(VALUE_ORIG),
        Value_wet_median = median(VALUE_WW),
        Value_dry_median = median(VALUE_DW),
        Value_lip_median = median(VALUE_FB)
      ) %>%
      mutate(
        Station_name = factor(Station_name),
        Speciesgroup = case_when(
          grepl("[0-9]+B", Station_name) ~ "Cod",
          grepl("[0-9]+F", Station_name) ~ "Flatfish",
          grepl("[0-9]+A", Station_name) ~ "Blue mussel",
          grepl("[0-9]+X", Station_name) ~ "Blue mussel",
          grepl("I[0-9]+", Station_name) ~ "Blue mussel",
          grepl("305", Station_name) ~ "Blue mussel",
          grepl("[0-9]+G", Station_name) ~ "Snail",
          grepl("[0-9]+N", Station_name) ~ "Eider duck",
          grepl("[0-9]+C", Station_name) ~ "Shrimp",
          TRUE ~ "Others")
      ) %>%
      group_by(PARAM, Basis, Station_name) %>%
      mutate(
        No_years = n(),
        Last_year = max(MYEAR)
      )
    
    result
    
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
    
    # FOR DYNAMIC CALCULATION OF MEDIANS (not used now)
    # dat_plot1 <- dat_plot1_function() %>%
    #   filter(Speciesgroup %in% input$speciesgroup_selected)
    
    if (input$valuebasis_selected == "Wet weight basis"){
      dat_plot1$Value_plot <- dat_plot1$Value_wet_median
      dat_plot1$Value_min_overloq <- dat_plot1$Value_wet_min_overloq
    } else  if (input$valuebasis_selected == "Dry weight basis"){
      dat_plot1$Value_plot <- dat_plot1$Value_dry_median
      dat_plot1$Value_min_overloq <- dat_plot1$Value_dry_min_overloq
    } else if (input$valuebasis_selected == "Lipid weight basis"){
      dat_plot1$Value_plot <- dat_plot1$Value_lip_median
      dat_plot1$Value_min_overloq <- dat_plot1$Value_lip_min_overloq
    } else if (input$valuebasis_selected == "Original value"){
      dat_plot1$Value_plot <- dat_plot1$Value_orig_median
      dat_plot1$Value_min_overloq <- dat_plot1$Value_orig_min_overloq
    }
    
    # browser()
    
    # min_no_years <- 1
    series_lasting_until <- 1980
    
    gg <- dat_plot1 %>%
      mutate(Station_name = fct_rev(Station_name)) %>%
      filter(No_years >= input$min_no_years,
             Last_year >= series_lasting_until) %>%
      ggplot(aes(MYEAR, Station_name)) +
      facet_grid(rows = vars(PARAM)) +
      labs(x = "Year", y = "Station")

    
    if (input$p1_show_medians){
      gg <- gg +   
        geom_raster(aes(fill = log10(Value_plot))) +
        scale_fill_viridis_c("log10(concentration)")
    }
    
    if (input$p1_show_50perc_overLOQ){
      gg <- gg +   
      geom_point(data = dat_plot1 %>% filter(Prop_over_LOQ > 0.5, 
                                             No_years >= input$min_no_years,
                                             Last_year >= series_lasting_until),
                 size = 5)
    }
    
    if (input$p1_show_number_overLOQ){
      gg <- gg +   
        geom_label(data = dat_plot1 %>% filter(No_years >= input$min_no_years,
                                               Last_year >= series_lasting_until),
                   aes(label = Over_LOQ))
    }

    if (input$p1_show_min_overLOQ){
      gg <- gg +   
        geom_point(data = dat_plot1 %>% filter(No_years >= input$min_no_years,
                                               Last_year >= series_lasting_until,
                                               Over_LOQ > 0),
                   aes(color = Value_min_overloq),
                   size = 5) +
        scale_color_viridis_c("Min value over LOQ")
    }
    
    gg   
    
  })
  
  #
  # Plot 2 (timeseriesplot) ----
  #
  
  
  
  # Plot below needs to be wrapped in observe() as 'height' uses an input value
  
    output$timeseriesplot <- renderPlot({
      
      dat_plot2 <- dat_plot2_function()

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
        scale_shape_manual(values = c(`Over LOQ` = 19, `Under LOQ` = 25)) +
        scale_fill_viridis_b() +
        facet_grid(vars(Station_name), vars(PARAM)) +
        labs(x = "Year",
             y = paste0("Concentration (", tolower(input$valuebasis_selected), ")"))
      
      if (input$number_of_points)
        resultplot <- resultplot +
        geom_text(
          data = dat_plot2 %>% count(MYEAR, Value_plot) %>% filter(n > 1),  
          aes(x = MYEAR + 0.3, label = n))
      
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
    })
  
  
  #
  # . make ui ----
  #
  
  p2_plotheight <- reactive({
    length(input$stations_selected)/length(input$params_selected)*500
  })
  
  output$timeseriesplot_ui <- renderUI({
    plotOutput("timeseriesplot", height = p2_plotheight())
  })

  #
  #  . table of data ----
  #
  
  output$timeseriesdata <- DT::renderDT(
    dat_plot2_function(), 
    filter = "top"
  )

  
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
    
    if (input$p3_range_years[1] > 1980 | input$p3_range_years[2] < 2020){
      result <- result %>%
        filter(MYEAR >= input$p3_range_years[1] & MYEAR <= input$p3_range_years[2])
    }

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
      geom_smooth(aes(color = Station_name), se = FALSE, method = 'lm', formula = 'y ~ x') +
      geom_point(aes(shape = Under_LOQ, color = Station_name), size = 2) +
      scale_color_brewer(palette = "Dark2") +
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
  
  #
  # Plot 4 ----
  #
  
  #
  # Get 'stations_available_year' ----
  # NOT USED for now
  #
  # For 'stations_selected' menu in plot 4
  #
  stations_available_year <- reactive({
    dat %>%
      filter(LATIN_NAME %in% input$species_selected,
             PARAM %in% input$params_selected) %>%
      distinct(Station_name) %>%
      pull(Station_name)
  })
  
  observeEvent(stations_available_year(), {
    updateSelectizeInput(
      inputId = "p4_stations_selected",
      choices = stations_available_year(),
      selected = input$p4_stations_selected,
      options = list(render = I(
        '{
    option: function(item, escape) {
      return "<div><strong>" + escape(item.value) + "</strong>"
    }
  }'))
    )
  })
  
  #
  # Get 'speciestissue_available_year' ----
  # NOT USED for now
  #
  # For 'stations_selected' menu in plot 4
  #
  speciestissue_available_year <- reactive({
    dat %>%
      filter(PARAM %in% input$params_selected,
             MYEAR %in% input$p4_years_selected) %>%
      distinct(Speciestissue) %>%
      pull(Speciestissue)
  })
  
  observeEvent(speciestissue_available_year(), {
    updateSelectizeInput(
      inputId = "p4_speciestissue_selected",
      choices = speciestissue_available_year(),
      selected = input$p4_speciestissue_selected,
      options = list(render = I(
        '{
    option: function(item, escape) {
      return "<div><strong>" + escape(item.value) + "</strong>"
    }
  }'))
    )
  })
  
  

  # . Get plot data (dat_plot3b) ----

  dat_plot4_function <- reactive({
    
    validate(
      need(input$p4_years_selected != "", "Please select at least one year")
    )
    validate(
      need(input$p4_speciestissue_selected != "", "Please select at least one species / tissue")
    )
    
    # Code in app
    result <- dat %>%
      filter(PARAM %in% input$params_selected,
             Basis %in% input$basis_selected,
             MYEAR %in% input$p4_years_selected,
             Speciestissue %in% input$p4_speciestissue_selected
             # Station_name %in% input$p4_stations_selected    NOT INCLUDED, for now
             ) %>%
      mutate(
        LOQ = ifelse(is.na(qflag), "Over LOQ", "Under LOQ"),
        Series = paste0(LATIN_NAME, ", ", 
                        TISSUE_NAME, ", ", 
                        Basis, "-basis")
      )
    
    result
    
  })
  
  # . Make plot ----

  output$stationplot <- renderPlot({
    
    dat_plot4 <- dat_plot4_function()  
    
    resultplot <- ggplot(dat_plot4, aes(Station_name, VALUE_WW)) +
      geom_jitter(aes(shape = LOQ, color = LOQ), size = 2, height = 0, width = 0.2) +
      scale_shape_manual(values = c(`Over LOQ` = 19, `Under LOQ` = 25)) +
      scale_color_manual(values = c(`Over LOQ` = "black", `Under LOQ` = "red2")) +
      facet_grid(vars(MYEAR), vars(PARAM)) +
      labs(x = "Station",
           y = paste0("Concentration (", tolower(input$valuebasis_selected), ")")) +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
    
    if (input$p4_logscale_y){
      resultplot <- resultplot + scale_y_log10()
    }

      resultplot
    
    
  })
  
  #
  #  . Table of data ----
  #
  
  output$stationdata <- DT::renderDT(
    dat_plot4_function(), 
    filter = "top"
  )
  
  
  
}  # end of server  



# Run the application ----
shinyApp(ui = ui, server = server)
