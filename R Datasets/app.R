#' @title Base R dataframe app
#' 
#' @author Clayton Allard
#' 
#' @description
#' This is a shiny app to show a histogram on a given feature of any base R 
#' dataframe. Additionally, this app allows the user the ability to group by 
#' another variable and to download a dataframe as a csv file.
#' 
#' Credit to Dean Attali's BC Liquor Store prices app. Many of the things
#' implemented here were inspired by looking at the source code for this app.
#'

# to ensure a clean app run
library(conflicted)
conflict_prefer('filter', winner = 'dplyr')
conflict_prefer('lag', winner = 'dplyr')

library(shiny)
library(tidyverse)
library(datasets)
library(purrr)

#' @title Determines whether a data set belongs on the shiny app.
#'
#' @description Must be a dataframe and it must have at least one numeric 
#' variable. Otherwise this will return FALSE and not be an option on the shiny app.
#'
#' @param dataset dataset name from base R.
#' @return TRUE if the dataset is a dataframe with at least one numeric 
#' variable. FALSE otherwise.
has_numeric_variable <- function(dataset){
  # only include if it is loadable
  tryCatch({
      data <- get(dataset)
      # want to filter out non-dataframes since they give many issues
      if (!('data.frame' %in% class(data))){
        return(FALSE)
      }
      # the sapply enacts the is.numeric function for each element of data
      # want at least one numeric variable
      return(any(sapply(data, is.numeric)))
  },
  error = function(e) {
    return(FALSE)
  }
  )
}

# get all numeric variables
# or character variables that have less than 10 unique values
get_typeof_vars <- function(dataset, type='numeric'){
  vars <- names(dataset)
  if (type == 'numeric'){
    return(vars[sapply(dataset, function(col) {is.numeric(col)})])
  }
  # if we want character type, it can be in the form of a character and a factor
  else if (type == 'character'){
    chars <- vars[sapply(dataset, function(col) {
      is.character(col) || is.factor(col)})]
    if (length(chars) == 0)
      return(chars)
    # Want no more than 10 values to not overload the histogram with groups
    return(chars[sapply(chars, function(col) {
      length(unique(dataset[[col]])) <= 10})]) 
  }
  return(character(0))
}

# get a list of all base R data sets that have at least one numeric variable
data_names <- data(package = 'datasets')$results[, "Item"]
data_names <- keep(data_names, has_numeric_variable)

# so that we can initialize the app with random dataset and variable
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
                    min = min(numeric_col, na.rm=TRUE),
                    max = max(numeric_col, na.rm=TRUE),
                    value = range(numeric_col, na.rm=TRUE)),
        
        # FEATURE 3: allow user to select the number of bins.
        # select number of bins for histogram
        sliderInput("bins", "Number of bins:", min = 1,max = 30,value = 10),
        
        # NEW FEATURE 2: choose any character feature (<=10 values) to group by
        # group by
        selectInput("group_by", "Select variable to group by:", 
                    choices=c('<no group>', char_vars),
                    selected = '<no group>')),
      mainPanel(
        # output a histogram and a table
        plotOutput("id_histogram"),
        # FEATURE 5: add an interactive table sorting by any variable.
        DT::dataTableOutput("id_table")
      )
    ),
    # NEW FEATURE 3: allow user to download filtered data
    downloadButton('download', 'Download Data')
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # the data set
  dataset <- reactive({
      get(input$dataset)
  })
  
  # filter the dataset by variable values
  filtered_data <- reactive({
    dataset() %>%
    dplyr::filter(!!sym(input$dropdown) >= input$id_slider[1],
           !!sym(input$dropdown) <= input$id_slider[2])
  })
  
  # this is to prevent errors. When switching data sets, the selected variable
  # doesn't change right away which causes errors. Using this as a check before
  # plotting or making the table fixes this issue.
  can_plot <- reactive({    
    num_vars <- get_typeof_vars(dataset())
    char_vars <- get_typeof_vars(dataset(), 'character')
    # Want to check if the currently selected variables belong in the dataset.
    input$dropdown %in% num_vars && 
      input$group_by %in% c('<no group>', char_vars)
  })
  
  observe({
    data <- dataset()
    
    # Variables to use for the updaters
    
    numeric_col <- as.numeric(data[[input$dropdown]])
    num_vars <- get_typeof_vars(data)
    char_vars <- get_typeof_vars(dataset(), 'character')
    
    # chooses random variable when switching datasets
    selected_val <- ifelse(!(input$dropdown %in% num_vars), 
                           sample(num_vars, 1),
                           input$dropdown)
    
    # makes sure no grouop is selected when changing datasets
    selected_group <- ifelse(!(input$group_by %in% char_vars), 
                             '<no group>',
                             input$group_by)
    
    # Update variable options once a dataset is chosen
    updateSelectInput(session, "dropdown",
                      label = paste("Select variable:"),
                      choices = num_vars,
                      selected = selected_val
                      )
    
    # Update group by options
    updateSelectInput(session, "group_by",
                      label = paste("Select variable to group by:"),
                      choices = c('<no group>', char_vars),
                      selected = selected_group)
  
    # Executing the code below without this check will cause errors
    if (!can_plot()){
      return()
    }

    # Update the slider input after selecting dropdown
    updateSliderInput(session, "id_slider",
                      value = range(numeric_col, na.rm=TRUE),
                      label = paste("Select a ", input$dropdown, " range:"),
                      min = min(numeric_col, na.rm=TRUE),
                      max = max(numeric_col, na.rm=TRUE))
  })
    
  # plot histogram
  output$id_histogram <- renderPlot({
    # Executing the code below without this check will cause errors
    if (!can_plot()){
      return()
    }
    
    # plot all as the same
    if(input$group_by == '<no group>'){
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
    # Executing the code below without this check will cause errors
    if (!can_plot()){
      return()
    }
    
    filtered_data()
  })
  
  # download data
  output$download <- downloadHandler(
    filename = function(){
      paste(input$dataset,'_',input$dropdown,'.csv', sep='')
    },
    content = function(file){
      write.csv(filtered_data(), file)
    }
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
