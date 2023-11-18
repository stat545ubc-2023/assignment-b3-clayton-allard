#
# Created by: Clayton Allard
# 
# Credit to Dean Attali's BC Liquor Store prices app. Many of the things
# implemented here were inspired by looking at the source code for this app.
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Orange Data App"),
    h4("Use this app to explore content on oranges"),
    sidebarLayout(
      sidebarPanel(
        selectInput("dropdown", "Select variable:", 
                    choices=c('age', 'circumference'), selected = 'age'),
        
        sliderInput("id_slider", "Select a age range:", 
                    min = min(Orange$age), max = max(Orange$age), 
                    value = range(Orange$age)),
        
        sliderInput("bins", "Number of bins:", min = 1,max = 30,value = 10),
      
        checkboxInput("by_tree", "Check box to group by tree", FALSE),
                      ),
      mainPanel(
        plotOutput("id_histogram"),
        DT::dataTableOutput("id_table")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # observe(print(input$id_slider))
  
  orange_data <- reactive({
    Orange$Tree <- factor(Orange$Tree, levels=as.character(1:5))
    Orange %>%
    dplyr::filter(!!sym(input$dropdown) >= input$id_slider[1],
           !!sym(input$dropdown) <= input$id_slider[2])
  })
  
  observe({
    
    # Update the slider input after selecting dropdown
    updateSliderInput(session, "id_slider", 
                      value = range(Orange[input$dropdown]),
                      label = paste("Select a ", input$dropdown, " range:"),
                      min = min(Orange[input$dropdown]),
                      max = max(Orange[input$dropdown]))
  })
  
  # plot histogram
  output$id_histogram <- renderPlot({
    # plot by specific tree
    if(input$by_tree){
      orange_data() %>%  
        ggplot(aes_string(input$dropdown, fill='Tree')) +
          geom_histogram(bins=input$bins, color='black', alpha=0.7)+
          theme_classic(20)
    } else {
      # plot all as the same
      orange_data() %>%  
        ggplot(aes_string(input$dropdown)) +
          geom_histogram(bins=input$bins, fill='orange', color='black', 
                         alpha=0.7)+
          theme_classic(20)
    }
  })
  
  output$id_table <- DT::renderDataTable({
    orange_data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
