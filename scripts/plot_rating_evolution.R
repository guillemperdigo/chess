# -------------------------------------------------------------- #
# Plot rating evolution
# Guillem Perdigó
# 12 - 01 - 2020
# -------------------------------------------------------------- #

# libraries
library(bigchess)
library(tidyverse)
library(ggthemes)
library(extrafont)
library(ggrepel)
library(here)

user <- "gperdigo8"
#user <- "newslide"

myBlitzGames <- read_rds(here(paste0("gamesData/", user, "/myBlitzGames.RDS")))

# ploting rating evolution ----
myBlitzGames %>%
  ggplot(aes(x = rowname)) +
  geom_point(aes(y = opponentRating), alpha = 1 / 7, color = "Red") +
  #geom_point(aes(y = myRating), alpha = 1 / 15) +
  geom_line(data = , aes(y = myRating)) +
  labs(
    title = paste(user, "chess.com rating"),
    y = NULL,
    x = "Games played"
  ) +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold"))


# plotting rating evolution 2 (trying to add legend)
myBlitzGames_long <- myBlitzGames %>%
  select(rowname, myRating, opponentRating, Date) %>% 
  pivot_longer(cols = c(myRating, opponentRating))

chessRatingPlot <- myBlitzGames_long %>% 
  ggplot(aes(x = rowname, color = name)) +
  geom_point(data = myBlitzGames_long[myBlitzGames_long$name=="opponentRating",],
             aes(y = value), alpha = 1 / 7) +
  #geom_point(aes(y = myRating), alpha = 1 / 15) +
  geom_line(data = myBlitzGames_long[myBlitzGames_long$name=="myRating",], 
            aes(y = value)) +
  ylim(c(700, 1300)) +
  labs(title = paste(user, "chess.com rating"),
       y = NULL,
       x = "Games played") +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = c(0.9, 0.2),
        legend.title = element_blank()) +
  scale_color_manual(values = c("black", "red"),
                     labels = c(paste(user, "rating"), "opponent rating"))

# adding dashed line at rating = 1200
chessRatingPlot  + 
  geom_hline(yintercept = 1200, 
             linetype = "longdash",
             size = 0.2)+ 
  geom_hline(yintercept = 1100, 
             linetype = "longdash",
             size = 0.2)  + 
  geom_hline(yintercept = 1000, 
             linetype = "longdash",
             size = 0.2)   + 
  geom_hline(yintercept = 900, 
             linetype = "longdash",
             size = 0.2)  + 
  geom_hline(yintercept = 800, 
             linetype = "longdash",
             size = 0.2) 

# adding labels for local maxima
chessRatingPlot + annotate(
  geom = "text",
  label = paste(myBlitzGames$Date[c(75, 196, 340, 675)], 
                myBlitzGames$myRating[c(75, 196, 340, 675)]),
  x = c(75, 196, 340, 675),
  y = myBlitzGames$myRating[c(75, 196, 340, 675)],
  vjust = -4,
  size = 2.5,
  family = "serif"
)
