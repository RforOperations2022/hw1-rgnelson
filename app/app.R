# import packages
library(tidyverse)
library(forcats)
library(shiny)
library(ggplot2)
library(usmap)

# read the data
data <- readRDS(file="cdi_data.rds")
#data <- complete(data, state, YearStart)
min.year <- min(as.numeric(levels(data$YearStart))[data$YearStart])
max.year <- max(as.numeric(levels(data$YearStart))[data$YearStart])
topic.list <- unique(data$Topic)
state.list <- levels(data$state)

# Define UI for Chronic Disease Indicators
ui <- fluidPage(

    # Application title
    titlePanel("Chronic Disease Indicators in the United States"),

    # sidebar to select the chronic disease category
    sidebarLayout(
        sidebarPanel(
            # select chronic disease
            selectInput(inputId = "cdi",
                        label = "Chronic Disease Category",
                        choices = topic.list,
                        selected = topic.list[1]),
            
            # select state
            selectInput(inputId = "state",
                        label = "State",
                        choices = c("All", state.list),
                        selected = "All")
        ),

        mainPanel(
            # slider to select the year
            sliderInput(inputId = "year",
                        label = "Year",
                        min = min.year,
                        max = max.year,
                        value = max.year,
                        sep = ""),
            
            plotOutput("us.map"),
            
            plotOutput("us.barplot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    # reactive to subset data to right topic
    data.topic.subset <- reactive({
        req(input$cdi)
        req(input$state)
        data %>% 
            filter(Topic == input$cdi,
                   YearStart == paste0(input$year)) %>%
            group_by(Topic, state, Stratification1) %>%
            summarize(total = sum(DataValue, na.rm=TRUE)) %>%
            mutate(selected.state = ifelse(state == input$state,
                                           TRUE, FALSE))
    })
    
    # reactive to subset data to right topic
    data.state.subset <- reactive({
        req(input$state, input$cdi)
        data %>% 
            filter(if (input$state != "All") (state == input$state)) %>%
            filter(Topic == input$cdi,
                   YearStart == paste0(input$year)) %>%
            group_by(Topic, state, Stratification1) %>%
            summarize(total = sum(DataValue, na.rm=TRUE))
    })
    
    
    # draw map of the US with heatmap by CDI (with overall stratification)
    output$us.map <- renderPlot({
        plot_usmap(data = filter(data.topic.subset(), Stratification1 == "Overall"), 
                   values = "total") + 
                   scale_fill_continuous(name = "Total Mortality per million",
                                         low = "white",
                                         high = "darkred",
                                         na.value = "blue",
                                         label=scales::comma) +
            theme(legend.position = "right")
    })
    
    # draw bar plot of mortality in the topic by state
    output$us.barplot <- renderPlot({
        ggplot(data = data.topic.subset(),
               aes(x = reorder(state, -total), y = total, fill = selected.state)) +
            geom_bar(stat = "identity") +
            xlab("State") +
            ylab("Total Mortality per Million") +
            scale_fill_manual(values = c("darkgray", "red")) +
            guides(fill = "none")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
