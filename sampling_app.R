# Load packages ----------------------------------------------------------------
library(shiny)
library(openintro)
library(shinyjs)
ui <- fluidPage(
  
  # Title ----
  headerPanel("Sampling distribution of a proportion and mean"),
  
  # Sidebar ----
  sidebarPanel(
      radioButtons("select_params",
                 label = "Target Parameters",
                 choices = c("Proportion", "Mean"),
                 selected = character(0),
                 inline = TRUE),
      conditionalPanel(
        condition = "input.select_params == 'Proportion'",
        numericInput("pop_proportion", "Population Proportion, p (Between 0 and 1)", value = 0, min = 0, max = 1, step = 0.01),
        textOutput("pop_error"),
      ),
      conditionalPanel(
        condition = "input.select_params == 'Mean'",
        numericInput("mean", "Population Mean", value = 0),
      ),
      
      numericInput("sample_size", "Sample Size (n) ", value = 1, min = 1, step = 1),
      textOutput("samplesize_error"),
      numericInput("number_samples", "Number of Samples (N) ", value = 1, min = 1, step = 1),
      textOutput("number_samples_error"),
      tags$head(tags$style(
        "#pop_proportion { width: 35%; }",
        "#mean { width: 35%; }",
        "#pop_error{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }",
        "#sample_size { width: 35%; }",
        "#samplesize_error{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }",
        "#number_samples { width: 35%; }",
        "#number_samples_error { color: red; font-size: 15px; font-style: italic; }",
      )
      ),
      checkboxInput("display_curve",
                    label = "Display Normal Curve",
                    value = FALSE),
    
  ),
  
  mainPanel(
    textOutput("sample_proportion"),
    textOutput("sd"),
    plotOutput("histogramPlot")
  )
)

# mean dropdown

#binomial, exponential, chi square, normal, t distribution

# add 

server <- function(input, output, session) {
  
  output$sample_proportion <- renderText({
    text <- paste0('Average Sample Porportion =')
  }) 
  
  output$sd <- renderText({
    text <- paste0('Standard deviation of sample proportions =')
  })
  data <- rnorm(100)  # Example data (replace with your data)
  output$histogramPlot <- renderPlot({
    
    hist(data, main = "", xlab = "Values", col = "lightblue", border = "black")
    mean_data <- mean(data)
    sd_data <- sd(data)
  
    
    
    curve(dnorm(x, mean = mean_data, sd = sd_data), add = TRUE, col = "red", lwd = 2)
  })
  
  
  is_pop_proportion_valid <- reactive({
    input$pop_proportion >= 0 && input$pop_proportion <= 1
  })
  
  output$pop_error <- renderText({
    if (is_pop_proportion_valid()) {
      ""
    } else {
      "Invalid input. Please enter a number between 0 and 1"
    }
  })
  
  is_sample_size_valid <- reactive({
    is.integer(input$sample_size) && input$sample_size %% 1 == 0
  })
  

  output$samplesize_error <- renderText({
    if (is_sample_size_valid()) {
      ""
    } else {
      "Invalid input. Please enter a whole number"
    }
  })
  number_samples_valid <- reactive({
    is.integer(input$number_samples) && input$number_samples %% 1 == 0
  })
  
  output$number_samples_error <- renderText({
    if (number_samples_valid()) {
      ""
    } else {
      "Invalid input. Please enter a whole number"
    }
  })
}
shinyApp(ui, server)
