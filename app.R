library(shiny)
library(httr2)

api_url <- "http://127.0.0.1:8081/predict"

ui <- fluidPage(
  titlePanel("Penguin Mass Predictor"),
  
  # Model input values
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "bill_length",
        "Bill Length (mm)",
        min = 30,
        max = 60,
        value = 45,
        step = 0.1
      ),
      selectInput(
        "sex",
        "Sex",
        c("Male", "Female")
      ),
      selectInput(
        "species",
        "Species",
        c("Adelie", "Chinstrap", "Gentoo")
      ),
      # Get model predictions
      actionButton(
        "predict",
        "Predict"
      )
    ),
    
    mainPanel(
      h2("Penguin Parameters"),
      verbatimTextOutput("vals"),
      h2("Predicted Penguin Mass (g)"),
      textOutput("pred")
    )
  )
)

server <- function(input, output, session) {
  # Initialize prediction on app start
  initial_prediction <- reactive({
    # Construct the request body within the reactive context
    req_body <- list(
      bill_length_mm = as.numeric(input$bill_length),
      species_Chinstrap = input$species == "Chinstrap",
      species_Gentoo = input$species == "Gentoo",
      sex_male = input$sex == "Male"
    )
    
    # Make the initial prediction request
    response <- tryCatch({
      httr2::request(api_url) |>
        httr2::req_body_json(req_body) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    }, error = function(e) {
      message("Error during API request: ", e$message)
      return(NULL)
    })
    
    # Return the response or NULL if there was an error
    return(response)
  })
  
  # Display the initial prediction
  output$pred <- renderText({
    if (!is.null(initial_prediction())) {
      paste("Predicted mass:", initial_prediction()$prediction, "grams")
    } else {
      "Failed to get prediction: API request error."
    }
  })
  
  # Always update this output, regardless of whether there was an error
  output$vals <- renderPrint({
    list(
      bill_length_mm = as.numeric(input$bill_length),
      species_Chinstrap = input$species == "Chinstrap",
      species_Gentoo = input$species == "Gentoo",
      sex_male = input$sex == "Male"
    )
  })
  
  # Event handler for the "Predict" button
  observeEvent(input$predict, {
    req_body <- list(
      bill_length_mm = as.numeric(input$bill_length),
      species_Chinstrap = input$species == "Chinstrap",
      species_Gentoo = input$species == "Gentoo",
      sex_male = input$sex == "Male"
    )
    
    response <- tryCatch({
      httr2::request(api_url) |>
        httr2::req_body_json(req_body) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    }, error = function(e) {
      message("Error during API request: ", e$message)
      return(NULL)
    })
    
    if (!is.null(response)) {
      output$pred <- renderText({
        paste("Predicted mass:", response$prediction, "grams")
      })
    } else {
      output$pred <- renderText("Failed to get prediction: API request error.")
    }
    
    output$vals <- renderPrint({
      req_body
    })
  }, ignoreInit = TRUE)
}


# Run the application
shinyApp(ui = ui, server = server)
