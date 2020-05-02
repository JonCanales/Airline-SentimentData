library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("wordcloud2")

# loading in data
airplaneData <- read.csv("/Users/jonathancanales/Desktop/FinalProjectR/Airline.csv")

# Variables to single out specific columns from the data
tweets <- airplaneData$text
sentiment <- airplaneData$airline_sentiment
pos_confidenceNumber <- airplaneData$airline_sentiment.confidence
negativeWords <- airplaneData$negativereason
neg_confidenceNumber <-  airplaneData$negativereason.confidence


# Scatter Plot Creation






# Word Plot
generalTweets <- Corpus(VectorSource(tweets))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
generalTweets <- tm_map(generalTweets, toSpace, "/")
generalTweets <- tm_map(generalTweets, toSpace, "@")
generalTweets <- tm_map(generalTweets, toSpace, "\\|")


# Convert the text to lower case
generalTweets <- tm_map(generalTweets, content_transformer(tolower))
# Remove numbers
generalTweets <- tm_map(generalTweets, removeNumbers)
# Remove english common stopwords
generalTweets <- tm_map(generalTweets, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
generalTweets <- tm_map(generalTweets, removeWords, c("blabla1", "blabla2"))
# Remove punctuations
generalTweets <- tm_map(generalTweets, removePunctuation)
# Eliminate extra white spaces
generalTweets <- tm_map(generalTweets, stripWhitespace)

dtm <- TermDocumentMatrix(generalTweets)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(words = d$word, freq = d$freq,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


# making axis
par(mai = c(0,.6,.6,.1))

plot(x = NULL,
     y = NULL,
     xlim = c(-1,5),
     # Each histograph covers half of each other except for the top one. You add .5 to get enough space for
     # The top one to get enough space.
     ylim = c(-1,5),
     ann = FALSE,
     axes = FALSE)
axis(1, at = xticks, tck = .01, lab = F, lwd = 2)
axis(2, at = yticks,tck = .01, lab = F, lwd = 2)

title(main="main title", sub="sub-title",
   xlab="x-axis label", ylab="y-axis label")

mtext("Common Negative Words",1,line = 3)


text(x, y = NULL, labels = seq_along(x$x), adj = NULL,
         pos = NULL, offset = 0.5, vfont = NULL,
         cex = 1, col = NULL, font = NULL, ...)


text(3,1,labels="Negative Words")

confidenceNumbers <- airplaneData[ , c("airline_sentiment.confidence", "negativereason.confidence")]
tweetandReason <- airplaneData[ , c("text", "airline_sentiment")]  
