library(shiny)
library(tidyverse)
library(DT)  # Load the DT package
source(here::here("xtab.R"))
source(here::here("num_analysis.R"))  # Load the num_analysis function

options(shiny.maxRequestSize = 50*1024^2)

ui <- fluidPage(
  titlePanel("Basic Shiny App: Data Upload, Variable Selection, and Grouping"),
  tabsetPanel(
    tabPanel("Data Upload",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose CSV or RDS File",
                           multiple = FALSE,
                           accept = c(".csv", ".rds"),
                           placeholder = "No file selected"),  # Added placeholder for better UI
                 actionButton("load_data", "Load Data")  # Button to load data
               ),
               mainPanel(
                 DTOutput("data_preview")  # Use DTOutput for the datatable
               )
             )
    ),
    tabPanel("Variable Selection",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("fct_DV"),       # Selector for the variable to describe
                 uiOutput("fct_IV"),  # Selector for the grouping variable
                 uiOutput("fct_weight"),  # Selector for the weight variable
                 actionButton("update", "Update Table")  # Button to refresh the table output
               ),
               mainPanel(
                 tableOutput("freq_table"),
                 plotOutput("barplot")
               )
             )
    ),
    tabPanel("Numeric Variables",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("num_DV"),       # Selector for the dependent numeric variable
                 uiOutput("num_IV"),  # Selector for the independent numeric variable
                 uiOutput("num_weight"),  # Selector for the weight variable
                 actionButton("update_num", "Update Table")  # Button to refresh the table output
               ),
               mainPanel(
                 tableOutput("num_table"),
                 plotOutput("scatterplot")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)  # Use reactiveVal to store the dataset

  observeEvent(input$load_data, {  # Load data when the button is clicked
    req(input$file)
    file <- input$file
    ext <- tools::file_ext(file$datapath)

    data <- tryCatch({  # Added tryCatch for error handling
      switch(ext,
             csv = read.csv(file$datapath),
             rds = readRDS(file$datapath),
             stop("Invalid file; Please upload a .csv or .rds file")
      )
    }, error = function(e) {
      showNotification("Error reading file: Please upload a valid .csv or .rds file", type = "error")  # Added notification for errors
      return(NULL)
    })

    dataset(data)  # Store the dataset in the reactiveVal
  })

  output$data_preview <- renderTable({
    req(dataset())
    head(dataset())
  })

  # this crashes
  # output$data_preview <- renderDT({  # Use renderDT for the datatable
  #   req(dataset())
  #   DT::datatable(head(dataset()),
  #                 options = list(ordering = TRUE,
  #                                width = "100%"),
  #                 filter = "top")  # Show a preview of the uploaded data
  # })

  output$fct_DV <- renderUI({
    req(dataset())
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]  # Filter for factor variables
    selectInput("variable", "Select Variable", choices = factor_vars)  # Updated to show only factor variables
  })

  output$fct_weight <- renderUI({
    req(dataset())
    selectInput("weight_variable", "Select Weighting Variable",
                choices = c("", names(dataset())))  # Add an empty option
  })

  output$fct_IV <- renderUI({
    req(dataset())

    # Identify factor variables in the dataset
    factor_vars <- names(dataset())[sapply(dataset(), is.factor)]

    if (length(factor_vars) > 0) {
      selectInput("group_variable", "Select Grouping Variable", choices = c("", factor_vars))
    } else {
      return(NULL)  # If there are no factor variables, do not display the input
    }
  })

  # Reactive expression to create the table for factors
  mytab <- reactive({
    input$update  # Dependency on the update button

    isolate({
      req(input$variable)

      data <- dataset()
      var_name <- input$variable
      group_var_name <- input$group_variable
      weight_var_name <- if (input$weight_variable == "") NULL else input$weight_variable  # Handle NULL case

      if (group_var_name == "" || is.null(group_var_name)) {
        # No grouping variable selected
        xtab(data, VD = var_name, PES = weight_var_name)
      } else {
        # Grouping variable selected
        xtab(data, VD = var_name, VI = group_var_name, PES = weight_var_name)
      }
    })
  })

  output$freq_table <- renderTable({
    mytab()  # Access the reactive mytab
  })

  output$barplot <- renderPlot({
    table_data <- mytab()
    if (is.null(table_data)) return(NULL)

    ggplot(table_data, aes(x = VI, y = PP, fill = VD)) +
      geom_bar(stat = "identity", position = "fill") +
      labs(x = input$variable, y = "Frequency", fill = input$group_variable) +
      theme_minimal()
  })

  # UI for numeric variables
  output$num_DV <- renderUI({
    req(dataset())
    numeric_vars <- names(dataset())[sapply(dataset(), is.numeric)]  # Filter for numeric variables
    selectInput("num_variable", "Select Dependent Variable", choices = numeric_vars)  # Updated to show only numeric variables
  })

  output$num_weight <- renderUI({
    req(dataset())
    selectInput("num_weight_variable", "Select Weighting Variable",
                choices = c("", names(dataset())))  # Add an empty option
  })

  output$num_IV <- renderUI({
    req(dataset())
    numeric_vars <- names(dataset())[sapply(dataset(), is.numeric)]  # Filter for numeric variables
    selectInput("num_group_variable", "Select Independent Variable", choices = numeric_vars)  # Updated to show only numeric variables
  })

  # Reactive expression to create the table for numeric variables
  numtab <- reactive({
    input$update_num  # Dependency on the update button

    isolate({
      req(input$num_variable, input$num_group_variable)

      data <- dataset()
      num_var_name <- input$num_variable
      num_group_var_name <- input$num_group_variable
      num_weight_var_name <- if (input$num_weight_variable == "") NULL else input$num_weight_variable  # Handle NULL case

      num_analysis(data, DV = num_var_name, IV = num_group_var_name, weight = num_weight_var_name)
    })
  })

  output$num_table <- renderTable({
    numtab()  # Access the reactive numtab
  })

  output$scatterplot <- renderPlot({
    table_data <- numtab()
    if (is.null(table_data)) return(NULL)

    ggplot(table_data, aes(x = IV, y = DV, weight = weight)) +
      geom_point() +
      labs(x = input$num_group_variable, y = input$num_variable) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
