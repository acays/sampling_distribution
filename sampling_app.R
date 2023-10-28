# Load packages ----------------------------------------------------------------
library(shiny)
library(openintro)
library(shinyjs)
ui <- fluidPage(
  
  # Title ----
  headerPanel("Sampling distribution of a proportion and mean"),
  
  # Sidebar ----
  sidebarPanel(
      numericInput("pop_proportion", "Population Proportion, p (Between 0 and 1)", value = 0, min = 0, max = 1, step = 0.01),
      textOutput("pop_error"),
      tags$head(tags$style("#pop_error{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"
      )
      ),
      numericInput("sample_size", "Sample Size (n): ", value = 1, min = 1, step = 1),
      textOutput("samplesize_error"),
      tags$head(tags$style("#samplesize_error{color: red;
                                 font-size: 15px;
                                 font-style: italic;
                                 }"
      )
      )
    
  ),
  
  mainPanel(
    
  )
)

server <- function(input, output, session) {
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
}
shinyApp(ui, server)
