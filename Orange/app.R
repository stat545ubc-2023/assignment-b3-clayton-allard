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
library(datasets)
library(purrr)

# used to subset datasets that have at least one numeric column
has_numeric_variable <- function(dataset){
  # only include if it is loadable
  tryCatch({
      data = get(dataset)
      # True if at least one column is numeric
      any(sapply(data.frame(data), is.numeric))
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# get all numeric/character variables that have less than 10 unique values
get_typeof_vars <- function(dataset, type='numeric'){
  vars <- names(dataset)
  if (type == 'numeric'){
    return(vars[sapply(dataset, function(col) {is.numeric(col)})])
  }
  # to not overload the plot
  else if (type == 'character'){
    chars <- vars[sapply(dataset, function(col) {
      is.character(col) || is.factor(col)})]
    if (length(chars) == 0)
      return(chars)
    return(chars[sapply(chars, function(col) {
      # print(length(unique(dataset[[col]])))
      length(unique(col)) <= 10})]) 
  }
  return(character(0))
}

# get a list of all base R data sets that have at least one numeric variable
data_names <- data(package = 'datasets')$results[, "Item"]
data_names <- keep(data_names, has_numeric_variable)

# so that we can initialize the dataframe
random_name <- sample(data_names, 1)
random_dataset <- data.frame(get(random_name))
num_vars <-  get_typeof_vars(random_dataset)
sample_num <-  sample(num_vars, 1)
char_vars <- get_typeof_vars(random_dataset, 'character')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Orange Data App"),
    h4("Use this app to explore content on oranges"),
    sidebarLayout(
      sidebarPanel(
        # NEW FEATURE 1: allow the user to choose dataset (from base R)
        # select variable to plot
        selectInput("dataset", "Select dataset:", 
                    choices=data_names, selected = random_dataset),
        
        # FEATURE 1: allow the user to choose variable to plot by
        # select variable to plot
        selectInput("dropdown", "Select variable:", 
                    choices=num_vars,
                    selected = sample_num),
        
        # FEATURE 2: allow user to select a range for the variable they chose.
        # select range of variable to visualize
        sliderInput("id_slider", "Select ", sample_num," range:", 
                    min = min(random_dataset[sample_num]), 
                    max = max(random_dataset[sample_num]), 
                    value = range(random_dataset[sample_num])),
        
        # FEATURE 3: allow user to select the number of bins.
        # select number of bins for histogram
        sliderInput("bins", "Number of bins:", min = 1,max = 30,value = 10),
        
        # group by
        selectInput("group_by", "Select variable to group by:", 
                    choices=c('', char_vars),
                    selected = ''),
      mainPanel(
        # output a histogram and a table
        plotOutput("id_histogram"),
        # FEATURE 5: add an interactive table sorting by any variable.
        DT::dataTableOutput("id_table")
      )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  dataset <- reactive({
    get(input$dataset)
  })
  
  filtered_data <- reactive({
    dataset() %>%
    dplyr::filter(!!sym(input$dropdown) >= input$id_slider[1],
           !!sym(input$dropdown) <= input$id_slider[2])
  })
  
  observe({
    data = dataset()
    
    # Update variable options once a dataset is chosen
    updateSelectInput(session, "dropdown", 
                      label = paste("Select variable:"),
                      choices = get_typeof_vars(data)
                      )
    
    # Update the slider input after selecting dropdown
    updateSliderInput(session, "id_slider", 
                      value = range(data[input$dropdown]),
                      label = paste("Select a ", input$dropdown, " range:"),
                      min = min(data[input$dropdown]),
                      max = max(data[input$dropdown]))
  })
  
  # plot histogram
  output$id_histogram <- renderPlot({
    if(input$group_by == ''){
      # plot all as the same
      orange_data() %>%  
        ggplot(aes(x = !!sym(input$dropdown))) +
          geom_histogram(bins=input$bins, fill='orange', color='black', 
                         alpha=0.7)+
          theme_classic(20)
    } else {
      # plot by group
      orange_data() %>%  
        ggplot(aes(x = !!sym(input$dropdown), fill = input$group_by)) +
          geom_histogram(bins=input$bins, color='black', alpha=0.7)+
          theme_classic(20)
    }
  })
  
  # create interactive table
  output$id_table <- DT::renderDataTable({
    dataset()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
