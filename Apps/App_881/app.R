#
# From
# https://github.com/rstudio/shiny/issues/3437
#
# Similar issue as
# https://community.rstudio.com/t/updateselectizeinput-causes-double-run/13345 
#

library(shiny)
set.seed(123)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("updateSelectizeInput triggers twice"),
  
  
  # main UI
  mainPanel(
    checkboxInput(inputId = "use_server",
                  label = "Use server side Processing in updateSelectizeInput()",
                  value = TRUE),
    selectizeInput(inputId = "in1",
                   label = "Input 1",
                   choices = 1:3,
                   selected = 1),
    selectizeInput(inputId = "in2",
                   label = "Input 2",
                   choices = letters,
                   selected = NULL)
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # show server checkbox value
  observeEvent(input$use_server, ignoreInit = FALSE, ignoreNULL = FALSE, {
    print(paste("server:", input$use_server))
  })
  
  # show input 1 value
  observeEvent(input$in1, ignoreInit = FALSE, ignoreNULL = FALSE, {
    print(paste("Input 1:", input$in1))
  })
  
  # show input 2 value
  observeEvent(input$in2, ignoreInit = FALSE, ignoreNULL = FALSE, {
    print(paste("Input 2:", input$in2))
  })
  
  # trigger update if input 2
  observeEvent(input$in1, ignoreInit = TRUE, ignoreNULL = FALSE, {
    choices <- sample(LETTERS, size = 5)
    updateSelectizeInput(session = session,
                         inputId = "in2",
                         choices = choices,
                         selected = choices[1],
                         server = input$use_server)
  })
}

# Run the application
shinyApp(ui = ui, server = server)