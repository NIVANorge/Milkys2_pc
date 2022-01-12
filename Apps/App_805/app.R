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
library(purrr)
library(forcats)
library(ggplot2)
library(DT)

source("../../844_ICES_submission_check_functions.R")
source("../../845_Check_ICES_submission_functions.R")   # set_numeric


save_folder_rds <- "../../Files_to_ICES/2020/Rdata"
files_available <- dir(save_folder_rds) %>% sort(decreasing = TRUE)

#
# UI ---- 
#
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Check ICES submission file"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      selectInput("selected_folder", "Select folder of data",
                  c("Submission files 1981-2014", "Submission files 2015-2019", "2020/Delivered"), 
                  selected = "2020/Delivered"
                  ),
      selectInput("selected_dataset", "Select dataset",
                  files_available),
      selectizeInput(inputId = "selected_dataset2", label = "Select dataset", 
                     choices = NULL, multiple = FALSE, 
                     selected = "NIVA2020CF_11.NO"),
      selectizeInput(inputId = "params_selected", label = "Parameter", 
                     choices = NULL, multiple = FALSE),
      selectizeInput(inputId = "matrix_selected", label = "Matrix", 
                     choices = NULL, multiple = FALSE),
      selectizeInput(inputId = "uncert_selected", label = "Uncertainty method", 
                     choices = NULL, multiple = FALSE),
      checkboxInput("logscale_y", "Use log10 on y axis", value = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("timeseriesplot"),
      #div(DT::dataTableOutput("timeseriesdata", filter = "top"), style = "font-size:80%")
      div(DTOutput("timeseriesdata"), style = "font-size:80%")
    )
  )
)

#
# SERVER ---- 
#
server <- function(input, output) {
  
  files_available <- reactive({
    folder_data <- paste0("../../Files_to_ICES/", input$selected_folder)
    fns <- dir(folder_data, full.names = FALSE)
    # browser()
    fns
  })
  
  observeEvent(files_available(), {
    updateSelectizeInput(
      inputId = "selected_dataset2",
      choices = files_available(),
      selected = input$selected_dataset2     # => selection isn't 'lost' when other input changes 
    )
  })
  
  filename <- reactive({
    validate(
      need(input$selected_dataset2 != "", "Please select dataset")
    )
    
    fn <- paste0("../../Files_to_ICES/", input$selected_folder, "/", input$selected_dataset2)
    browser()
    fn
    
  })
  
  
  data_list_fn <- reactive({
    
    data_list <- read_ices_file(filename()) %>%
      add_field_codes() %>%
      set_numeric()
    
    # data_list <-readRDS(paste0(save_folder_rds, "/", input$selected_dataset))
    # names(data_list) <- c("00", "03", "04", "10", "20", "21", "90", "91", "92")
    # # 04 = sample table
    # # 10 = concentration data
    # # 91 = station data
    
    data_list
  })
  
  params_avaliable <- reactive({
    data_list <- data_list_fn()
    data_list[["10"]] %>% 
      pull(PARAM) %>% unique() %>% sort()
  })
  
  observeEvent(params_avaliable(), {
    updateSelectizeInput(
      inputId = "params_selected",
      choices = params_avaliable(),
      selected = input$params_selected     # => selection isn't 'lost' when other input changes 
    )
  })
  
  matrix_avaliable <- reactive({
    data_list <- data_list_fn()
    data_list[["10"]] %>% 
      pull(MATRX) %>% unique() %>% sort()
  })
  
  observeEvent(matrix_avaliable(), {
    updateSelectizeInput(
      inputId = "matrix_selected",
      choices = matrix_avaliable(),
      selected = input$matrix_selected     # => selection isn't 'lost' when other input changes 
    )
  })
  
  uncert_avaliable <- reactive({
    data_list <- data_list_fn()
    data_list[["10"]] %>%
      filter(PARAM %in% input$params_selected,
             MATRX %in% input$matrix_selected) %>%
      pull(METCU) %>% unique() %>% sort()
  })
  
  observeEvent(uncert_avaliable(), {
    updateSelectizeInput(
      inputId = "uncert_selected",
      choices = uncert_avaliable(),
      selected = input$uncert_selected     # => selection isn't 'lost' when other input changes 
    )
  })
  
  dat_plot_fn <- reactive({
    
    validate(
      need(input$params_selected != "", "Please select at least one parameter")
    )
    validate(
      need(input$matrix_selected != "", "Please select matrix")
    )
    validate(
      need(input$uncert_selected != "", "Please select type of uncertainty")
    )
    
    data_list <- data_list_fn()
    dat <- data_list[["10"]]
    
    # Code in app, unchanged
    dat_plot <- dat %>%
      filter(PARAM %in% input$params_selected,
             METCU %in% input$uncert_selected | is.na(METCU),
             MATRX %in% input$matrix_selected) %>%
      mutate(
        LOQ = ifelse(is.na(QFLAG), "Over LOQ", "Under LOQ")
      )
    
    if (input$uncert_selected == "%"){
      dat_plot$VALUE_min <- dat_plot$VALUE*(100 - dat_plot$UNCRT)/100
      dat_plot$VALUE_max <- dat_plot$VALUE*(100 + dat_plot$UNCRT)/100
    } else if (input$uncert_selected == "SD"){
      dat_plot$VALUE_min <- dat_plot$VALUE - 2*dat_plot$UNCRT
      dat_plot$VALUE_max <- dat_plot$VALUE + 2*dat_plot$UNCRT
    }
    
    dat_plot
  })
  
  output$timeseriesplot <- renderPlot({
    
    dat_plot <- dat_plot_fn()

    # browser()
    
    resultplot <- ggplot(dat_plot, aes(STNNO, VALUE)) +
      geom_pointrange(aes(ymin = VALUE_min, ymax = VALUE_max, color = LOQ), position = position_jitter(width = 0.2, height = 0)) +
      geom_point(aes(y = DETLI), shape = 4, position = position_jitter(width = 0.2, height = 0)) +
      facet_wrap(vars(MATRX))
    
    if (input$logscale_y)
      resultplot <- resultplot + scale_y_log10()

    resultplot  
  })
  
  output$timeseriesdata <- DT::renderDT(
    dat_plot_fn(), 
    filter = "top"
  )
  
}


#
# Run the application ----
#
shinyApp(ui = ui, server = server)
