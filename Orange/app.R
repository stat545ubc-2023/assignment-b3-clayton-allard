#
# Created by: Clayton Allard
# 
# Credit to Dean Attali's BC Liquor Store prices app. Many of the things
# implemented here were inspired by looking at the source code for this app.
#

library(conflicted)
conflict_prefer('filter', winner = 'dplyr')
conflict_prefer('lag', winner = 'dplyr')
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Orange Data App"),
    h4("Use this app to explore content on oranges"),
    sidebarLayout(
      sidebarPanel(
        # FEATURE 1: allow the user to choose variable to plot by
        # select variable to plot
        selectInput("dropdown", "Select variable:", 
                    choices=c('age', 'circumference'), selected = 'age'),
        
        # FEATURE 2: allow user to select a range for the variable they chose.
        # select range of variable to visualize
        sliderInput("id_slider", "Select a age range:", 
                    min = min(Orange$age), max = max(Orange$age), 
                    value = range(Orange$age)),
        
        # FEATURE 3: allow user to select the number of bins.
        # select number of bins for histogram
        sliderInput("bins", "Number of bins:", min = 1,max = 30,value = 10),
      
        # FEATURE 4: allow user to select whether to group by tree.
        # choose whether to group by tree type
        checkboxInput("by_tree", "Check box to group by tree", FALSE),
                      ),
      mainPanel(
        # output a histogram and a table
        plotOutput("id_histogram"),
        # FEATURE 5: add an interactive table sorting by any variable.
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
        ggplot(aes(x = !!sym(input$dropdown), fill = Tree)) +
          geom_histogram(bins=input$bins, color='black', alpha=0.7)+
          theme_classic(20)
    } else {
      # plot all as the same
      orange_data() %>%  
        ggplot(aes(x = !!sym(input$dropdown))) +
          geom_histogram(bins=input$bins, fill='orange', color='black', 
                         alpha=0.7)+
          theme_classic(20)
    }
  })
  
  # create interactive table
  output$id_table <- DT::renderDataTable({
    orange_data()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
