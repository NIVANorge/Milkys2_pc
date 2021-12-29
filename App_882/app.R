#
# Modified from
# https://community.rstudio.com/t/updateselectizeinput-causes-double-run/13345
#
# Used as model for App_801
#

library(shiny)
library(ggplot2) # use mpg dataset in ggplot2
library(data.table)

dt <- as.data.table(mpg)
manufacturer_list <- dt[, unique(manufacturer)]

ui <- fluidPage(
  selectInput(inputId = "manufacturer",
              label = "Manufacturer",
              choices = manufacturer_list,
              selected = manufacturer_list[1]
  ),
  selectizeInput(inputId = "model",                   # changed to 'selectizeInput'
              label = "Model",
              choices = NULL, multiple = TRUE         # added multiple = TRUE 
  ),
  tableOutput("table")
)

server <- function(input, output, session) {
  
  filtered_models <- reactive({dt[manufacturer == input$manufacturer, unique(model)]})
  
  observeEvent(filtered_models(), {
    updateSelectizeInput(session, 'model', choices = c(filtered_models()), selected = filtered_models()[1])
  })
  
  filtered_mpg <- reactive({
    # str(input$model)
    dt[manufacturer %in% input$manufacturer & model %in% input$model]
  })
  
  output$table <- renderTable(filtered_mpg())
  
}

shinyApp(ui, server)
