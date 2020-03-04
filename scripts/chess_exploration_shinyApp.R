# -------------------------------------------------------------- #
# Exploration of my chess games
# Guillem Perdigó
# 12 - 01 - 2020
# -------------------------------------------------------------- #

# libraries
library(bigchess)
library(tidyverse)
library(ggthemes)
library(extrafont)
library(ggrepel)

userFile <- read.csv("gamesData/username.csv",
                     stringsAsFactors = FALSE)
user <- userFile[1,1]
#user <- "newslide"

myGames <- read_rds(paste0("gamesData/", user, "/myGames.RDS"))
myBlitzGames <- read_rds(paste0("gamesData/", user, "/myBlitzGames.RDS"))


# Frequent opponents ----
myGames %>% 
  group_by(opponent) %>% 
  summarise(gamesPlayed = n(),
            winPerc = mean(myResult)*100) %>% 
  arrange(desc(gamesPlayed))

# Exploring variables by win/loss ----

# Color & castling
myGames %>% 
  group_by(myColor, doICastle) %>% 
  summarise(counts = n(), result = mean(myResult, na.rm = TRUE)*100)

# Color & first move
myGames %>% 
  group_by(myColor, W1, B1) %>% 
  summarise(counts = n(), result = mean(myResult)*100) %>% 
  filter(counts > 5) %>% 
  arrange(desc(counts))

# Win rate by color & Opening
myGames %>% 
  group_by(myColor, ECO) %>% # something goes wrong when including ECOUrl. maybe urls not unique?
  summarise(counts = n(), result = mean(myResult)) %>% 
  filter(counts > 5) %>% 
  arrange(result)

# Win rate by ECO codes
myGames %>% 
  group_by(ECO) %>% 
  summarise(counts = n(), result = mean(myResult))


#  geom_text_repel(data = myBlitzGames[c(75, 196, 340, 675),],
#                                  aes(x = rowname, y = myRating, label = Date))

# Creating a Summary table ----
#### 10 minutes games 
### I am white 
myGames %>% 
  filter(TimeControl == "600", White == "gperdigo8") %>% 
  select(Result) %>%
  table() -> white10min
# 1-0 1/2-1/2     0-1 
# 50       6      20 

myBlitzGames %>% 
  select(Event, Date, TimeControl, myRating) %>% 
  arrange(Date)

### I am black ----
myGames %>% 
  filter(TimeControl == "600", Black == "gperdigo8") %>% 
  select(Result) %>%
  table()  -> black10min
# 1-0 1/2-1/2     0-1 
# 23       3      39 


#### 5 minutes games
### I am white
myGames %>% 
  filter(TimeControl == "300", White == "gperdigo8") %>% 
  select(Result) %>%
  table()  -> white5min
# 1-0 1/2-1/2     0-1 
# 110       3      88 

### I am black
myGames %>% 
  filter(TimeControl == "300", Black == "gperdigo8") %>% 
  select(Result) %>%
  table()  -> black5min
# 1-0 1/2-1/2     0-1 
# 93       4     104 

# summary win-rate by color & time-control
gamesSummary <- as.data.frame(rbind(white10min, black10min, white5min, black5min))
gamesSummary$winPerc <- NA
gamesSummary$winPerc[c(1, 3)] <-
  round(
    gamesSummary$`1-0`[c(1, 3)] / (
      gamesSummary$`1-0`[c(1, 3)] + gamesSummary$`1/2-1/2`[c(1, 3)] + gamesSummary$`0-1`[c(1, 3)]
    ),
    2
  )
gamesSummary$winPerc[c(2, 4)] <-
  round(
    gamesSummary$`0-1`[c(2, 4)] / (
      gamesSummary$`1-0`[c(2, 4)] + gamesSummary$`1/2-1/2`[c(2, 4)] + gamesSummary$`0-1`[c(2, 4)]
    ),
    2
  )

gamesSummary$color <- c("white", "black", "white", "black")
gamesSummary$timeControl <- c("10 min", "10 min", "5 min", "5 min")
colnames(gamesSummary) <- c("win", "draw", "loss", "winPerc", "color", "timeControl")
gamesSummary

# plot gamesSummary
ggplot(gamesSummary, aes(timeControl, winPerc)) +
  geom_col() +
  facet_grid(vars(color))

# win-rate by Opening ----
myBlitzGames %>% 
  filter()
  group_by(Opening, myColor) %>% 
  summarise(winPerc = mean(myResult),
            count = n()) %>% 
  arrange(desc(count))



  
  
  
  
  
