library(shiny)
library(dplyr)
library(here)
library(ggplot2)
library(ggthemes)

ui <- fluidPage(
  titlePanel("Evolution of chess rating"),
  selectInput(
    inputId = "user",
    label = "Choose a chess.com user",
    choices = c("gperdigo8", "newslide")
  ),
  plotOutput("plot")
)


server <- function(input, output) {
  output$plot <- renderPlot({
    
    # read data (previously downloaded and created)
    myBlitzGames <- readRDS(here(paste0("gamesData/", input$user, "/myBlitzGames.RDS")))
    
    # ploting rating evolution ----
    myBlitzGames %>%
      ggplot(aes(x = rowname)) +
      geom_point(aes(y = opponentRating), alpha = 1 / 7, color = "Red") +
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
  
}

shinyApp(ui, server)