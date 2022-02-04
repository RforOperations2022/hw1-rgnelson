# import packages
library(tidyverse)
library(forcats)
library(shiny)
library(ggplot2)
library(usmap)
library(DT)
library(stringr)

# read the data
data <- readRDS(file="cdi_data.rds")
min.year <- min(as.numeric(levels(data$YearStart))[data$YearStart])
max.year <- max(as.numeric(levels(data$YearStart))[data$YearStart])
topic.list <- unique(data$Topic)
state.list <- levels(data$state)

# Define UI for Chronic Disease Indicators
ui <- fluidPage(

    # Application title
    titlePanel("Chronic Disease Mortality in the United States"),
    
    # User input and US Map section
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
                    
                    plotOutput("us.map"))
    ),
    
    # Bar plots and tabular output
    fluidRow(
        column(6,
               h2("State Data"),
               tabsetPanel(
                   tabPanel("Bar Plot", plotOutput("state.barplot")),
                   tabPanel("Time Trend", plotOutput("state.time.trend")),
                   tabPanel("Table", DT::dataTableOutput("state.table"),
                            downloadButton("state.download.button"))
               )),
        # US bar plot and table
        column(6,
               h2("United States Data"),
               tabsetPanel(
                   tabPanel("Plot", plotOutput("us.barplot")),
                   tabPanel("Table", DT::dataTableOutput("us.table"),
                            downloadButton("us.download.button"))
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
            summarize(total = sum(DataValue, na.rm=FALSE)) %>%
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
            filter(state == input$state) %>%
            filter(Topic == input$cdi,
                   Stratification1 == "Overall") %>%
            group_by(Question, state, YearStart) %>%
            summarize(total = sum(DataValue, na.rm=FALSE))
    })
    
    
    # draw map of the US with heatmap by topic (with overall stratification)
    output$us.map <- renderPlot({
        plot_usmap(data = filter(data.topic.subset(), Stratification1 == "Overall"), 
                   values = "total") + 
                   scale_fill_continuous(name = "Total Mortality per million",
                                         low = "white",
                                         high = "darkred",
                                         na.value = "lightgray",
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
    
    # draw bar plot at the state level for each question
    output$state.barplot <- renderPlot({
        req(input$year)
        ggplot(data = data.state.subset() %>% filter(YearStart == input$year),
               aes(x = Question, y = total)) +
            geom_bar(stat = "identity", width = 0.2) +
            xlab("Chronic Disease") +
            ylab("Mortality per Million") +
            guides(fill = "none") + 
            ggtitle(paste0("Year: ", input$year)) + 
            if (input$state != "All") {
                scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
            }
    })
    
    # draw a lineplot over time at the state level
    output$state.time.trend <- renderPlot({
        ggplot(data = data.state.subset() %>%
                   group_by(state, YearStart) %>%
                   summarize(total = sum(total)),
               aes(x = YearStart, y = total)) +
            geom_line(group = 1, color = "darkred", size = 1, linetype = "dashed") + 
            geom_point(group = 1, color = "darkred", size = 3) +
            xlab("Year") +
            ylab("Total Mortality per Million")
    })
    
    output$state.table <- DT::renderDataTable(
        DT::datatable(data = data.state.subset(),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    )
    
    # data table output for US/Topic level mortality data
    output$us.table <- DT::renderDataTable(
        DT::datatable(data = data.topic.subset.pretty(),
                      options = list(pageLength = 10),
                      rownames = FALSE)
    )
    
    # data table output for chronic disease breakdown per topic
    output$cdi.levels <- renderTable(data.cdi.levels())
    
    # download button for US/Topic level mortality data
    output$us.download.button <- downloadHandler(
        # from documentation
        filename = function() {
            paste(input$cdi, "_mortality_", str_replace_all(Sys.time(), ":|\ ", "_"), 
                  ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data.topic.subset.pretty(), file)
        }
    )
    
    # download button for state level mortality data
    output$state.download.button <- downloadHandler(
        # from documentation
        filename = function() {
            paste(input$state, "_", input$cdi, "_mortality_", str_replace_all(Sys.time(), ":|\ ", "_"), 
                  ".csv", sep = "")
        },
        content = function(file) {
            write.csv(data.state.subset(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
