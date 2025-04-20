# Install required packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")

library(shiny)
library(ggplot2)

# Precompute wheel positions (0-36)
numbers <- 0:36
n <- length(numbers)
angles <- seq(0, 2*pi, length.out = n + 1)[-(n+1)]
wheel_df <- data.frame(
  number = numbers,
  x = cos(angles),
  y = sin(angles)
)

# Define red/black sets
redNumbers <- c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
blackNumbers <- setdiff(numbers[numbers != 0], redNumbers)

# UI definition
ui <- fluidPage(
  titlePanel("Educational Roulette Simulator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("bet", "Bet Amount:", value = 10, min = 1),
      selectInput(
        "betType", "Bet Type:",
        choices = list(
          "Number" = "number",
          "First dozen (1-12)" = "first",
          "Second dozen (13-24)" = "second",
          "Third dozen (25-36)" = "third",
          "Red" = "red",
          "Black" = "black",
          "Odd" = "odd",
          "Even" = "even",
          "Column" = "column"
        ),
        selected = "first"
      ),
      conditionalPanel(
        condition = "input.betType == 'number'",
        numericInput("number", "Choose Number (0-36):", value = 0, min = 0, max = 36)
      ),
      conditionalPanel(
        condition = "input.betType == 'column'",
        selectInput(
          "columnChoice", "Choose Column:",
          choices = list(
            "1st Column" = "1",
            "2nd Column" = "2",
            "3rd Column" = "3"
          ),
          selected = "1"
        )
      ),
      actionButton("spin", "Spin the Wheel!"),
      actionButton("reset", "Reset Game"),
      hr(),
      h4("Bankroll:"),
      verbatimTextOutput("bankroll")
    ),
    mainPanel(
      plotOutput("wheelPlot", height = "400px"),
      h4("Last Spin Result:"),
      verbatimTextOutput("result"),
      plotOutput("fundPlot", height = "200px")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values to track game state
  rv <- reactiveValues(
    bankroll = 1000,
    spins = numeric(),
    funds = numeric(),
    lastResult = NULL,
    lastSpin = NULL
  )
  
  # Reset game
  observeEvent(input$reset, {
    rv$bankroll <- 1000
    rv$spins    <- numeric()
    rv$funds    <- numeric()
    rv$lastResult <- NULL
    rv$lastSpin   <- NULL
  })
  
  # Spin event
  observeEvent(input$spin, {
    betAmt <- input$bet
    betType <- input$betType
    
    # Validate bet amount
    if (betAmt > rv$bankroll || betAmt <= 0) {
      showModal(modalDialog(
        title = "Invalid Bet",
        "Your bet must be positive and not exceed your bankroll."))
      return()
    }
    
    # Get specific inputs
    chosenNumber <- if (betType == 'number') input$number else NA
    chosenColumn <- if (betType == 'column') as.integer(input$columnChoice) else NA
    
    # Validate straight bet
    if (betType == 'number' && (chosenNumber < 0 || chosenNumber > 36)) {
      showModal(modalDialog(
        title = "Invalid Number",
        "Please choose a number between 0 and 36."))
      return()
    }
    
    # Simulate spin
    spinNum <- sample(numbers, 1)
    
    # Determine win and payout
    win <- FALSE
    payoutMultiplier <- 0
    switch(betType,
           number = {
             if (spinNum == chosenNumber) { win <- TRUE; payoutMultiplier <- 35 }
           },
           first = { if (spinNum >= 1 && spinNum <= 12) win <- TRUE; payoutMultiplier <- 2 },
           second = { if (spinNum >= 13 && spinNum <= 24) win <- TRUE; payoutMultiplier <- 2 },
           third = { if (spinNum >= 25 && spinNum <= 36) win <- TRUE; payoutMultiplier <- 2 },
           red = { if (spinNum %in% redNumbers) win <- TRUE; payoutMultiplier <- 1 },
           black = { if (spinNum %in% blackNumbers) win <- TRUE; payoutMultiplier <- 1 },
           odd = { if (spinNum != 0 && spinNum %% 2 == 1) win <- TRUE; payoutMultiplier <- 1 },
           even = { if (spinNum != 0 && spinNum %% 2 == 0) win <- TRUE; payoutMultiplier <- 1 },
           column = {
             rem <- ifelse(chosenColumn == 3, 0, chosenColumn)
             if (spinNum != 0 && spinNum %% 3 == rem) win <- TRUE
             payoutMultiplier <- 2
           }
    )
    
    # Apply result
    if (win) {
      payout <- betAmt * payoutMultiplier
      rv$bankroll <- rv$bankroll + payout
      desc <- switch(betType,
                     number = paste0("Straight on ", chosenNumber),
                     first = "First dozen",
                     second = "Second dozen",
                     third = "Third dozen",
                     red = "Red",
                     black = "Black",
                     odd = "Odd",
                     even = "Even",
                     column = paste0(chosenColumn, ifelse(chosenColumn==1,'st',ifelse(chosenColumn==2,'nd','rd'))," Column")
      )
      rv$lastResult <- paste0("You WON! ", desc,
                              ". Number: ", spinNum,
                              ". You gain ", payout, " units.")
    } else {
      rv$bankroll <- rv$bankroll - betAmt
      rv$lastResult <- paste0("You LOST. Number: ", spinNum,
                              ". You lose ", betAmt, " units.")
    }
    
    # Record history
    rv$spins <- c(rv$spins, spinNum)
    rv$funds <- c(rv$funds, rv$bankroll)
    rv$lastSpin <- spinNum
    
    # Auto-reset if bankrupt
    if (rv$bankroll <= 0) {
      showModal(modalDialog(
        title = "Bankrupt",
        "You have reached zero. Click Reset Game to start over.",
        easyClose = TRUE
      ))
    }
  })
  
  # Outputs
  output$bankroll <- renderText({ paste0(rv$bankroll, " units") })
  output$result   <- renderText({ req(rv$lastResult); rv$lastResult })
  
  output$wheelPlot <- renderPlot({
    req(rv$lastSpin)
    ggplot(wheel_df, aes(x=x, y=y)) +
      geom_point(size=5, color="gray70") +
      geom_text(aes(label=number), size=3) +
      geom_point(data=subset(wheel_df, number==rv$lastSpin), aes(x=x,y=y), color="red", size=8) +
      coord_fixed() + theme_void() +
      ggtitle("Roulette Wheel (highlighted in red)")
  })
  
  output$fundPlot <- renderPlot({
    req(rv$funds)
    df <- data.frame(spinIndex=seq_along(rv$funds), bankroll=rv$funds)
    ggplot(df, aes(x=spinIndex, y=bankroll)) +
      geom_line() + geom_point() +
      labs(x="Spin Number", y="Bankroll (units)") + theme_minimal()
  })
}

# Run the application
shinyApp(ui=ui, server=server)
