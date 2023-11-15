#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
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
        sliderInput("id_slider", "Select an age range:", 
                    min = min(Orange$age), max = max(Orange$age), 
                    value = c(quantile(Orange$age, probs = c(0.4, 0.6)))),
        
      sliderInput("bins", "Number of bins:", min = 1,max = 50,value = 30)
                      ),
      mainPanel(
        plotOutput("id_histogram"),
        tableOutput("id_table")
      )
    )
    

    # # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #        plotOutput("distPlot")
    #     )
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # observe(print(input$id_slider))
  
  orange_data <- reactive({
    Orange %>%
    filter(age >= input$id_slider[1],
           age <= input$id_slider[2])
  })
  
  output$id_histogram <- renderPlot({
    orange_data() %>%  
      ggplot(aes(circumference)) +
        geom_histogram(bins=input$bins, fill='orange', color='black', alpha=0.7)
  })
  
  output$id_table <- renderTable({
    orange_data()
  })

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
