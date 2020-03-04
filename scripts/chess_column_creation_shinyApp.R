# -------------------------------------------------------------- #
# Exploration of my chess games
# Column creation for myGames from chess.com
# Guillem Perdigó
# 12 - 01 - 2020
# -------------------------------------------------------------- #

# libraries
library(bigchess)
library(tidyverse)
library(stringr)
library(magrittr)
library(here)

# here we first read all this files and merge them into a single dataframe
userFile <- read.csv(here("gamesRawData/username.csv"),
                     stringsAsFactors = FALSE)
user <- userFile[1,1]#user <- "newslide"

# read data files downloaded from chess.com/games/archive ----
file_list <- list.files(path= here(paste0("gamesRawData/", user)))
myGames <- data.frame()
for (i in 1:length(file_list)){
  gamesRawData <- read.pgn(here(paste0("gamesRawData/", user, "/", file_list[i])), 
                        add.tags = c("TimeControl", "WhiteElo", "BlackElo", "ECO", "ECOUrl", "UTCTime"))
  myGames <- rbind(myGames, gamesRawData) #for each iteration, bind the new data to myGames
}
rm(gamesRawData, file_list, i)

# Datetime ----
myGames %<>% 
  mutate(Datetime = as.POSIXct(paste(Date, UTCTime), format = "%Y.%m.%d %H:%M:%S")) %>% 
  arrange(Datetime)

#  My color ----
myGames$myColor <- NA # create empty column
for (i in 1:nrow(myGames)) {
  if (myGames$White[i] == user) {
    myGames$myColor[i] <- "White"
  } else {
    myGames$myColor[i] <- "Black"
  }
}

# Opponent username
myGames$opponent <- NA
for (i in 1:nrow(myGames)) {
  if (myGames$White[i] == user) {
    myGames$opponent[i] <- myGames$Black[i]
  } else {
    myGames$opponent[i] <-  myGames$White[i]
  }
}

# My result ----
myGames$myResult <- NA # create empty column
for (i in 1:nrow(myGames)) {
  if (myGames$Result[i] == "1-0") {
    if (myGames$myColor[i] == "White") {
      myGames$myResult[i] <- 1
    }
    if (myGames$myColor[i] == "Black") {
      myGames$myResult[i] <- 0
    } 
  }
  else if (myGames$Result[i] == "0-1") {
    if (myGames$myColor[i] == "White") {
      myGames$myResult[i] <- 0
    }
    if (myGames$myColor[i] == "Black") {
      myGames$myResult[i] <- 1
    } 
  }
  else {
    myGames$myResult[i] <- 0.5
  }
}

# Do I castle ----
myGames$doICastle <- NA # create empty column
for (i in 1:nrow(myGames)) {
  if (myGames$myColor[i] == "White") {
    if (myGames$W_O_moves[i] == 1) {
      myGames$doICastle[i] <- TRUE
    } else {
      myGames$doICastle[i] <- FALSE
    }
  } else {
    if (myGames$B_O_moves[i] == 1) {
      myGames$doICastle[i] <- TRUE
    } else {
      myGames$doICastle[i] <- FALSE
    }
  }
}

# Opening text ----
myGames$Opening <- NA
for (i in 1:nrow(myGames)) {
  myGames$Opening[i] <-
    str_replace_all(str_extract(myGames$ECOUrl[i], "(?<=https://www.chess.com/openings/).*"),
                    "-",
                    " ")
}


# My rating ----
myGames$myRating <- NA # create empty column
for (i in 1:nrow(myGames)) {
  if (myGames$myColor[i] == "White") {
    myGames$myRating[i] <- myGames$WhiteElo[i]
  } else {
    myGames$myRating[i] <- myGames$BlackElo[i]
  }
}

# My opponent rating ----
myGames$opponentRating <- NA # create empty column
for (i in 1:nrow(myGames)) {
  if (myGames$myColor[i] == "White") {
    myGames$opponentRating[i] <- myGames$BlackElo[i]
  } else {
    myGames$opponentRating[i] <- myGames$WhiteElo[i]
  }
}

# Rating change ----
myGames$rating_change <- NA
for (i in 2:nrow(myGames)) {
  myGames$rating_change[i] <- abs(myGames$myRating[i] -  myGames$myRating[i-1])
}

# Cleaning time control ----
# Time control gets splitted into "Time base" and "Time Increment"
myGames <- myGames %>% 
  cbind(str_split_fixed(myGames$TimeControl, "\\+|\\/", n = 2)) %>% 
  rename(timeBase = `1`, timeIncrement = `2`)

myGames$timeBase <- as.numeric(as.character(myGames$timeBase))

# dataframe with only "Blitz" games ----
myBlitzGames <- 
  myGames %>% 
  filter(Event == "Live Chess",
         between(timeBase, 121, 899))

# adding rownames as a column so it can be plotted as the x axis
myBlitzGames %<>% 
  rownames_to_column()

myBlitzGames$rowname <- as.integer(myBlitzGames$rowname)

# create directory if it doesn't exist
dir.create(file.path(here(paste0("gamesCleanData/", user))))


# save dataframes
write_rds(myGames, here(paste0("gamesCleanData/", user, "/myGames_", user, ".RDS")))
write_rds(myBlitzGames, here(paste0("gamesCleanData/", user, "/myBlitzGames_", user, ".RDS")))

