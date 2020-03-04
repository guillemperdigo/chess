library(shiny)
library(dplyr)
library(here)
library(ggplot2)
library(ggthemes)
library(reticulate)

js <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'

ui <- fluidPage(
  tags$script(js),
  titlePanel("Evolution of chess rating"),
  sliderInput(
    inputId = "numGames",
    label = "Number of games",
    value = 100,
    min = 1,
    max = 1000
  ),
  sidebarLayout(
    sidebarPanel("whatever just testing"),
    #sidebarPanel(textOutput("current_rating")),
    mainPanel(
      textInput(inputId = "user",
                label = "Choose a chess.com user"),
      plotOutput("plot")
    ),
    position = "right"
  )
)

server <- function(input, output) {
  # reactive value that changes when enter is pressed
  User <- reactiveVal()
  
  observeEvent(input[["keyPressed"]], {
    User(input[["user"]])
  })
  
  output$current_rating <- renderText({
    myBlitzGames <-
      myBlitzGames <- readRDS(here(
        paste0(
          "gamesCleanData/",
          User(),
          "/myBlitzGames_",
          User(),
          ".RDS"
        )
      ))
    current_rating <- tail(myBlitzGames$myRating, 1)
    paste("Current rating:", current_rating)
  })
  
}

shinyApp(ui, server)