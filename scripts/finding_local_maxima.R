
tt <- c(1, 2, 3, 2, 1, 1, 2, 1)
which(diff(sign(diff(tt)))==-2)+1

x <- c(1, 2, 3, 2, 1, 1, 2, 1)
library(zoo)
xz <- as.zoo(myBlitzGames$myRating)
rxz <- rollapply(xz, 250, function(x) which.min(x)==2)
max_indeces <- index(rxz)[coredata(rxz)]
myBlitzGames$myRating[max_indeces]


#    2     3     4     5     6     7 
#FALSE FALSE FALSE  TRUE FALSE FALSE 
rollapply(xz, 10, function(x) which.max(x)==2)
rxz <- rollapply(xz, 90, function(x) which.max(x)==2)
index(rxz)[coredata(rxz)]


annotate(
  geom = "text",
  label = myBlitzGames$Date[c(75, 196, 340, 675)],
  x = c(75, 196, 340, 675),
  y = myBlitzGames$myRating[c(75, 196, 340, 675)],
  vjust = -4
)

# Create dataframe with max rating every 100 interval
cuts <- c(2, 100, 200, 300, 400, 500, 600, 700, 800)
max_df <- data.frame(
  index = c(1:8),
  value = c(1:8)
)

for (i in 1:(length(cuts)-1)) {
  max_df$index[i] <- which.max(myBlitzGames$myRating[cuts[i]:cuts[i+1]])+ cuts[i]-1
  max_df$value[i] <- myBlitzGames$myRating[which.max(myBlitzGames$myRating[cuts[i]:cuts[i+1]])+ cuts[i]-1]
}

# Pick local maxima of said dataframe

maxvalues <- as.numeric(max_df$value)
tt
mean(maxvalues)
which(diff(sign(diff(maxvalues)))==-2)+1




myBlitzGames$myRating[which.max(myBlitzGames$myRating[cuts[1]:cuts[2]])+ cuts[1]-1] #1117




# Second max
which.max(myBlitzGames$myRating[cuts[1]:cuts[2]])+ cuts[1]-1 #26
myBlitzGames$myRating[which.max(myBlitzGames$myRating[cuts[1]:cuts[2]])+ cuts[1]-1] #1117

# Second max
which.max(myBlitzGames$myRating[183:364])+182 #297
myBlitzGames$myRating[which.max(myBlitzGames$myRating[364:546])+365] #976

# Third max
which.max(myBlitzGames$myRating[365:546])+364 #546
myBlitzGames$myRating[which.max(myBlitzGames$myRating[401:600])+400] #976

# Fourth max
which.max(myBlitzGames$myRating[547:nrow(myBlitzGames)])+546 #727
myBlitzGames$myRating[which.max(myBlitzGames$myRating[601:800])+600] #1194

nrow(myBlitzGames)/4
