# import packages
library(shiny)
library(ggplot2)
library(usmap)

# read the data
data <- readRDS(file="cdi_data.rds")
minYear <- min(as.numeric(levels(data$YearStart))[data$YearStart])
maxYear <- max(as.numeric(levels(data$YearStart))[data$YearStart])

# Define UI for Chronic Disease Indicators
ui <- fluidPage(

    # Application title
    titlePanel("Chronic Disease Indicators in the United States"),

    # sidebar to select the chronic disease category
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "cdi",
                        label = "Chronic Disease Category",
                        choices = data$Topic,
                        selected = "Alcohol")
        ),

        mainPanel(
            # slider to select the year
            sliderInput(inputId = "year",
                        label = "Year",
                        min = minYear,
                        max = maxYear,
                        value = maxYear,
                        sep = ""),
            plotOutput("us.map")
        )
    )
)

# Define server logic
server <- function(input, output) {
    data.subset <- reactive({
        req(input$cdi)
        data %>% 
            filter(Topic == input$cdi,
                   YearStart == paste0(input$year)) %>%
            group_by(Topic, state, Stratification1) %>%
            summarize(total = sum(DataValue, na.rm=TRUE))
    })
    
    # draw map of the US with heatmap by CDI (with overall stratification)
    output$us.map <- renderPlot({
        plot_usmap(data = filter(data.subset(), Stratification1 == "Overall"), 
                   values = "total") + 
                   scale_fill_continuous(name = "Total Mortality per million",
                                         low = "white",
                                         high = "red",
                                         label=scales::comma) +
            theme(legend.position = "right")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
