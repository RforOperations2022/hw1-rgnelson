# import packages
library(tidyverse)
library(forcats)
library(shiny)
library(ggplot2)
library(usmap)
library(DT)

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
    
    fluidRow(
             # Main user input panel
             column(4, 
                     # select chronic disease
                     selectInput(inputId = "cdi",
                                 label = "Chronic Disease Category",
                                 choices = topic.list,
                                 selected = topic.list[1]),
                     # select state
                     selectInput(inputId = "state",
                                 label = "State",
                                 choices = c("All", state.list),
                                 selected = "All"),
                     # chronic disease breakdown for the overall topic
                     tableOutput("cdi.levels")),
             # US map output
             column(6, 
                    # slider to select the year
                    # centered it following these instructions:
                    # https://stackoverflow.com/a/61818613
                    div(style = "margin: auto; width: 50%",
                        sliderInput(inputId = "year",
                                    label = "Year",
                                    min = min.year,
                                    max = max.year,
                                    value = max.year,
                                    sep = "",
                                    width = "100%")),
                    
                    plotOutput("us.map"),
                    
                    tabsetPanel(
                        tabPanel("Plot", plotOutput("us.barplot")),
                        tabPanel("Table", DT::dataTableOutput("us.table"),
                                 downloadButton("download.button"))
                    ))
             
             
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
                   YearStart == paste0(input$year),
                   Stratification1 == "Overall") %>%
            group_by(Topic, state, Stratification1) %>%
            summarize(total = sum(DataValue, na.rm=TRUE)) %>%
            mutate(selected.state = ifelse(state == input$state,
                                           TRUE, FALSE))
    })
    
    # pretty data table output
    data.topic.subset.pretty <- reactive({
        data.topic.subset() %>% 
            select(!selected.state) %>% 
            rename(State = state, Stratification = Stratification1,
                   `Total Mortality per Million` = total)
            
    })
    
    # reactive to get the right topic levels
    data.cdi.levels <- reactive({
        req(input$cdi)
        data %>%
            filter(Topic == input$cdi) %>%
            select(Question) %>%
            distinct() %>%
            rename(`Chronic Disease` = Question)
    })
    
    # reactive to subset data to right topic and state
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
            scale_fill_manual(values = c("darkgray", "darkred")) +
            guides(fill = "none") + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    })
    
    # data table output
    output$us.table <- DT::renderDataTable(
        DT::datatable(data = data.topic.subset.pretty(),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    )
    
    # data table output for chronic disease breakdown per topic
    output$cdi.levels <- renderTable(data.cdi.levels())
    
    # download button
    output$download.button <- downloadHandler(
        # from documentation
        filename = function() {
            paste(input$cdi, "_mortality_", str_replace_all(Sys.time(), ":|\ ", "_"), 
                  ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data.topic.subset.pretty(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
