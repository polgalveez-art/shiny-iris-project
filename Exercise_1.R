library(shiny)
library(ggplot2)

data(iris)

ui <- fluidPage(
  titlePanel("Iris"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Filter data and visualize - Iris dataset."),
      
      checkboxGroupInput(
        inputId = "species",
        label = "Species (filter):",
        choices = levels(iris$Species),
        selected = levels(iris$Species)
      ),
      
      selectInput(
        inputId = "var_hist",
        label = "Variable for histogram:",
        choices = names(iris)[1:4],
        selected = "Sepal.Length"
      ),
      
      sliderInput(
        inputId = "bins",
        label = "Num bins (histogram):",
        min = 5, max = 50, value = 20
      ),
      
      selectInput(
        inputId = "xvar",
        label = "X (scatter):",
        choices = names(iris)[1:4],
        selected = "Sepal.Length"
      ),
      
      selectInput(
        inputId = "yvar",
        label = "Y (scatter):",
        choices = names(iris)[1:4],
        selected = "Petal.Length"
      ),
      
      checkboxInput(
        inputId = "show_table",
        label = "Show filtered dat",
        value = TRUE
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histograma", plotOutput("histPlot"), verbatimTextOutput("histSummary")),
        tabPanel("Scatter", plotOutput("scatterPlot"), verbatimTextOutput("corrText")),
        tabPanel("Datos", uiOutput("tableUI"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filtered by species
  filtered_data <- reactive({
    req(input$species)
    iris[iris$Species %in% input$species, , drop = FALSE]
  })
  
  # Dinamic histogram
  output$histPlot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = .data[[input$var_hist]], fill = Species)) +
      geom_histogram(bins = input$bins, alpha = 0.7, position = "identity") +
      labs(
        title = paste("Histogram: ", input$var_hist),
        x = input$var_hist,
        y = "Freq"
      ) +
      theme_minimal()
  })
  
  # Summary
  output$histSummary <- renderPrint({
    df <- filtered_data()
    req(nrow(df) > 0)
    summary(df[[input$var_hist]])
  })
  
  # Dinamic scatter
  output$scatterPlot <- renderPlot({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]], color = Species)) +
      geom_point(size = 2, alpha = 0.8) +
      labs(
        title = paste("Scatter:", input$xvar, "vs", input$yvar),
        x = input$xvar,
        y = input$yvar
      ) +
      theme_minimal()
  })
  
  # Text with correlation
  output$corrText <- renderPrint({
    df <- filtered_data()
    req(nrow(df) > 1)
    
    x <- df[[input$xvar]]
    y <- df[[input$yvar]]
    
    cat("Pearson correlation with filtered data:\n")
    cat("r =", round(cor(x, y), 4), "\n")
  })
  
  # Dinamic 
  output$tableUI <- renderUI({
    if (!isTRUE(input$show_table)) return(NULL)
    tableOutput("dataTable")
  })
  
  output$dataTable <- renderTable({
    filtered_data()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
}

shinyApp(ui, server)