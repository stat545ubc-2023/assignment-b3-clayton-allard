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
      data <- get(dataset)
      if (!('data.frame' %in% class(data))){
        return(FALSE)
      }
      return(any(sapply(data, is.numeric)))
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
      length(unique(dataset[[col]])) <= 10})]) 
  }
  return(character(0))
}

# get a list of all base R data sets that have at least one numeric variable
data_names <- data(package = 'datasets')$results[, "Item"]
data_names <- keep(data_names, has_numeric_variable)

# so that we can initialize the dataframe
random_name <- sample(data_names, 1)
random_dataset <- get(random_name)
num_vars <-  get_typeof_vars(random_dataset)
sample_num <-  sample(num_vars, 1)
char_vars <- get_typeof_vars(random_dataset, 'character')
numeric_col <- as.numeric(random_dataset[[sample_num]])

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("R Dataset App"),
    h4("Use this app to explore content on all base R dataframes"),
    sidebarLayout(
      sidebarPanel(
        # NEW FEATURE 1: allow the user to choose dataset (from base R)
        # select variable to plot
        selectInput("dataset", "Select dataset:",
                    choices=data_names, selected = random_name),
        
        # FEATURE 1: allow the user to choose variable to plot by
        # select variable to plot
        selectInput("dropdown", "Select variable:", 
                    choices=num_vars,
                    selected = sample_num),
        
        # FEATURE 2: allow user to select a range for the variable they chose.
        # select range of variable to visualize
        sliderInput("id_slider", paste("Select ", sample_num, "range:"),
                    min = min(numeric_col),
                    max = max(numeric_col),
                    value = range(numeric_col)),
        
        # FEATURE 3: allow user to select the number of bins.
        # select number of bins for histogram
        sliderInput("bins", "Number of bins:", min = 1,max = 30,value = 10),
        
        # group by
        selectInput("group_by", "Select variable to group by:", 
                    choices=c('', char_vars),
                    selected = '')),
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

  dataset <- reactive({
      get(input$dataset)
  })
  
  filtered_data <- reactive({
    dataset() %>%
    dplyr::filter(!!sym(input$dropdown) >= input$id_slider[1],
           !!sym(input$dropdown) <= input$id_slider[2])
  })
  
  can_plot <- reactive({    
    num_vars <- get_typeof_vars(dataset())
    char_vars <- get_typeof_vars(dataset(), 'character')
    input$dropdown %in% num_vars && 
      input$group_by %in% c('', char_vars)
  })
  
  observe({
    data <-  dataset()
    
    numeric_col <- as.numeric(data[[input$dropdown]])
    num_vars <- get_typeof_vars(data)
    char_vars <- get_typeof_vars(dataset(), 'character')
    
    selected_val <- ifelse(!(input$dropdown %in% num_vars), 
                           sample(num_vars, 1),
                           input$dropdown)
    
    selected_group <- ifelse(!(input$group_by %in% char_vars), 
                             '',
                             input$group_by)
    
    # Update variable options once a dataset is chosen
    updateSelectInput(session, "dropdown",
                      label = paste("Select variable:"),
                      choices = num_vars,
                      selected = selected_val
                      )
    
    # Update group by options
    updateSelectInput(session, "group_by",
                      label = paste("Select variable:"),
                      choices = c('', get_typeof_vars(data, 'character')),
                      selected = selected_group)
  
  # print(input$dataset)
  # print(input$group_by)
  # 
    if (!can_plot()){
      return()
    }

    # Update the slider input after selecting dropdown
    updateSliderInput(session, "id_slider",
                      value = range(numeric_col),
                      label = paste("Select a ", input$dropdown, " range:"),
                      min = min(numeric_col),
                      max = max(numeric_col))
  })
    
  # plot histogram
  output$id_histogram <- renderPlot({
    print(input$group_by)
    if (!can_plot()){
      return()
    }
    if(input$group_by == ''){
      # plot all as the same
      filtered_data() %>%
        ggplot(aes(x = !!sym(input$dropdown))) +
          geom_histogram(bins=input$bins, fill='orange', color='black',
                         alpha=0.7)+
          theme_classic(20)
    } else {
      # plot by group
      filtered_data() %>%
        ggplot(aes(x = !!sym(input$dropdown), fill = !!sym(input$group_by))) +
          geom_histogram(bins=input$bins, color='black', alpha=0.7)+
          theme_classic(20) +
          labs(fill = input$group_by)
    }
  })
  
  # create interactive table
  output$id_table <- DT::renderDataTable({
    if (!can_plot()){
      return()
    }
    filtered_data()
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
