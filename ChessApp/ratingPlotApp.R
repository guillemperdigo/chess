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
  sliderInput(inputId = "numGames",
              label = "Number of games",
              value = 100, min = 1, max = 1000),
  sidebarLayout(
    sidebarPanel("whatever just testing"),
    #sidebarPanel(textOutput("current_rating")),
    mainPanel(
      textInput(
        inputId = "user",
        label = "Choose a chess.com user",
        value = "gperdigo8"
      ),
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
  
  output$plot <- renderPlot({
    # write file with username from input
    write.csv(input$user, here("gamesRawData/username.csv"),
              row.names = FALSE)
    # define python installation to run
    use_python("/Users/Guillem/anaconda3/bin/python", required = TRUE) 
    # run python script to download data
    source_python(here("scripts/download_chess_com_monthly_archives.py"))
    # run R script to clean/transform data
    source(here("scripts/chess_column_creation_shinyApp.R"))
    # read data 
    myBlitzGames <- readRDS(here(paste0("gamesCleanData/", 
                                        input$user, 
                                        "/myBlitzGames_", 
                                        input$user,".RDS")))
    
    # ploting rating evolution ----
    myBlitzGames %>%
      tail(input$numGames) %>% 
      ggplot(aes(x = rowname)) +
      geom_point(aes(y = opponentRating), alpha = 1/7, color = "Red") +
      #geom_point(aes(y = myRating), alpha = 1 / 15) +
      geom_line(data = , aes(y = myRating)) +
      labs(
        title = NULL,
        y = NULL,
        x = "Games played"
      ) +
      ylim(c(600, 1300)) +
      theme_tufte() +
      theme(plot.title = element_text(face = "bold"))

  })
 # 
 # output$current_rating <- renderText({
 #   myBlitzGames <- readRDS(here(paste0("gamesData/", input$user, "/myBlitzGames.RDS")))
 #   current_rating <- tail(myBlitzGames$myRating, 1)
 #   paste("Current rating:", current_rating)
 # })
 # 
 # output$last_game <- renderText({
 #   myBlitzGames <- readRDS(here(paste0("gamesData/", input$user, "/myBlitzGames.RDS")))
 #   last_game <- max(myBlitzGames$Datetime)
 #   paste("Last game registered:", last_game)
 # })
}

shinyApp(ui, server)