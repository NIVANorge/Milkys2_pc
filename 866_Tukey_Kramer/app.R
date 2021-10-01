#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(agricolae)

# Data and station metadata
dat <- readRDS("../Files_from_Jupyterhub_2020/Raw_data/109_adjusted_data_2021-09-15.rds")
dat_stations <- readxl::read_excel("../Files_to_Jupyterhub_2019/Kartbase_edit.xlsx")

dat2 <- dat %>%
    filter(VALUE_WW > 0.1) %>%
    left_join(dat_stations, by = "STATION_CODE") %>%
    mutate(
        Matrix = paste0(LATIN_NAME, ", ", TISSUE_NAME),
        log_Conc = log10(VALUE_WW + 0.1),
        Station = paste(STATION_CODE, substr(STATION_NAME, 1, 15)),
        LOQ = ifelse(is.na(FLAG1), "Over LOQ", "Under LOQ")
    ) %>%
    filter(!is.na(log_Conc))

matrices <- dat2 %>%
    filter(MYEAR == 2020) %>%
    count(Matrix) %>%
    arrange(desc(n)) %>%
    pull(Matrix)

years <- sort(unique(dat2$MYEAR))

group_seq <- c("Metals and metalloids", "Chlorobiphenyls", 
               "Polycyclic aromatic hydrocarbons (PAHs)", 
               "Organobromines", 
               "Dichloro-diphenyl-trichloroethane (DDTs)", "Organochlorines (general)",
               "Chlorinated paraffins", "Organofluorines", "Phenols/chlorophenols", 
               "Organo-metallic compounds", 
               "Chlorinated flame retardants",
               "Biological effects: molecular/biochemical/cellular/assays", 
               "Organic esters", "Isotopes",  
               "Cyclodienes", "Dioxins", "Biomarkers", "Phthalates", 
               "Phosphorus flame retardant (PFR)", "Major inorganic constituents", 
               "Support parameters", "Hexachlorocyclohexanes", "Triazines", 
               "Siloxanes", 
               ""
)

param_meta <- read.csv2("../Files_to_Jupyterhub_2019/Lookup for big excel - param.csv") %>%
    select(Parameter.Code, Substance.Group) %>%
    mutate(Substance.Group = factor(Substance.Group, levels = group_seq))

parameters <- dat2 %>%
    filter(MYEAR == 2020) %>%
    distinct(PARAM) %>%
    left_join(param_meta, by = c("PARAM" = "Parameter.Code")) %>%
    arrange(Substance.Group, PARAM) %>%
    pull(PARAM)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Milkys - sammenligning mellom stasjoner samme år"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("matrix", "Matrix", choices = matrices),
            selectInput("param", "Parameter", choices = parameters),
            selectInput("year", "Year", choices = years, selected = 2020)
        ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("ggplot"),
           plotOutput("multcomp_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    dat_select <- reactive({
        dat2 %>%
            filter(
                PARAM %in% input$param & 
                    Matrix %in% input$matrix & 
                    MYEAR %in% input$year) 
    }) 
    
    multcomp <- reactive({
        model <- aov(log_Conc ~ Station, data = dat_select())
        outHSD <-  HSD.test(model, "Station", alpha = 0.05)
    })
    
    df_summ <- reactive({
        multcomp_val <- multcomp()
        multcomp_val$means %>%
            mutate(Station = rownames(multcomp_val$means)
            ) 
    })
    
    # For plotting Min, Max, Q25, Q75, Station_group labels
    df_groups <- reactive({
        multcomp_val <- multcomp()
        data.frame(
            Station = rownames(multcomp_val$groups),
            Station_group = multcomp_val$groups$groups) %>%
            left_join(df_summ(), by = "Station") %>%
            rename(
                Mean = log_Conc,
                Median = Q50)
    })
    
    # For plotting mean and median
    df_groups2 <- reactive({
        df_groups_val <- df_groups()
        df_groups_val %>%
            select(Station, Mean, Median) %>%
            tidyr::pivot_longer(-Station, names_to = "Type", values_to = "Value")
    })
    
    output$ggplot <- renderPlot({
        ggplot(dat_select(), aes(Station, VALUE_WW, color = LOQ)) +
            geom_jitter(width = 0.1, height = 0, size = rel(1.6)) +
            scale_color_manual(values = c("Under LOQ" = "red2", "Over LOQ" = "black")) +
            scale_y_log10() +
            theme(axis.text.x = element_text(angle = -45, hjust = 0, size = rel(1.5))) +
            labs(x = "")
    })
    
    # output$multcomp_plot <- renderPlot({
    #     par(mfrow=c(1,1), mar=c(3,10,2,3))
    #     plot(multcomp(), horiz=TRUE, las = 1, cex.names = 1, 
    #          main = "Tukey-Kramer HSD test")
    # })
    
    output$multcomp_plot <- renderPlot({
        dat_select_val <- dat_select()
        ggplot(df_groups2(), aes(Station)) +
            geom_linerange(data = df_groups(), aes(ymin = Min, ymax = Max)) +
            geom_linerange(data = df_groups(), aes(ymin = Q25, ymax = Q75), size = 1.25) +
            geom_point(aes(y = Value, shape = Type), color = "white", fill = "red2", size = 3.5) +
            scale_shape_manual(values = c(22, 23)) +
            geom_text(data = df_groups(),
                      aes(
                          y = Max + diff(range(dat_select_val$log_Conc))*0.07,
                          label = Station_group,
                          color = Station_group)) +
            labs(title = "Tukey-Kramer HSD post-hoc test", 
                 y = "log10(Concentration + 0.1)", x = "") +
            theme(axis.text.x = element_text(angle = -45, hjust = 0, size = rel(1.5)))
            
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
