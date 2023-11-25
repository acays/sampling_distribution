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
                 selected = "Proportion",
                 inline = TRUE),
      conditionalPanel(
        condition = "input.select_params == 'Proportion'",
        numericInput("pop_proportion", "Population Proportion, p (Between 0 and 1)", value = 0.5, min = 0, max = 1, step = 0.01),
        textOutput("pop_error"),
      ),
      conditionalPanel(
        condition = "input.select_params == 'Mean'",
        numericInput("mean", "Population Mean", value = 0),
      ),
      
      numericInput("sample_size", "Sample Size (n) ", value = 1, min = 1, step = 1),
      textOutput("samplesize_error"),
      numericInput("number_samples", "Number of Samples (N) ", value = 2, min = 2, step = 1),
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
    text <- paste0('Average Sample Porportion = ', input$pop_proportion)
  }) 
  
  output$sd <- renderText({
    text <- paste0('Standard deviation of sample proportions = ', 
                   sqrt(input$pop_proportion*(1-input$pop_proportion)/input$sample_size))
  })
  
  

  output$histogramPlot <- renderPlot({
    data <- rbinom(input$number_samples, input$sample_size, input$pop_proportion)/input$sample_size
    hist_data <- hist(data, freq=FALSE, main = "", xlab = "Values", col = "lightblue", border = "black")
    mean_data <- mean(data)
    sd_data <- sd(data)

    if (input$display_curve == TRUE) {
      p0=input$pop_proportion
      N=input$number_samples
      n=input$sample_size

      x = seq( p0-4*sqrt(p0*(1-p0)/n), p0+4*sqrt(p0*(1-p0)/n),length=2000)
      y = dnorm(x,mean=p0,sd= sqrt(p0*(1-p0)/n))

      
      # if (max(y) > max(hist_data$density)) {
      #   print("activated")
      #   # Calculate new x-axis limits to accommodate the density curve
      #   new_max <- max(c(max(x), max(hist_data$breaks)))
      #   
      #   # Update the xlim outside of the plot rendering
      #   hist_data$xlim <- c(min(hist_data$breaks), new_max)
      # }
      # plot(hist_data, freq=FALSE, main = "", xlab = "Values", col = "lightblue", border = "black")
      lines(x,y, lty=2,lwd=2,col="red")
    }
  })
  # output$histogramPlot <- renderPlot({
  #   data <- rbinom(input$number_samples, input$sample_size, input$pop_proportion)/input$sample_size
  #   
  #   # Create the histogram without plotting it
  #   hist_data <- hist(data, plot = FALSE)
  #   
  #   # Calculate the density curve
  #   density_curve <- density(data)
  #   
  #   # Plot the histogram
  #   hist(data, freq=FALSE, main = "", xlab = "Values", col = "lightblue", border = "black")
  #   
  #   # Plot the density curve, adjusting its data if needed
  #   if (max(density_curve$y) > max(hist_data$density) & input$display_curve == TRUE) {
  #     # Scale the density curve data to fit within the histogram bounds
  #     scaled_density <- density_curve
  #     scaled_density$y <- scaled_density$y * (max(hist_data$density) / max(density_curve$y))
  #     lines(scaled_density, col = "red", lty = 2, lwd = 2)
  #   } else if (input$display_curve == TRUE) {
  #     lines(density_curve, col = "red", lty = 2, lwd = 2)
  #   }
  # })
  
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
