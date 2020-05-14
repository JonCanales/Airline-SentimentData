# 21:198:220 Fundamentals of Data Visualization, Spring 2020
# Final Project
# Jonathan Canales, jac835

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("wordcloud2")
library("plyr")

# loading in data
airplaneData <- read.csv("/Users/jonathancanales/Desktop/FinalProjectR/Airline.csv")

# Variables to single out specific columns from the data
tweets <- airplaneData$text
sentiment <- airplaneData$airline_sentiment
pos_confidenceNumber <- airplaneData$airline_sentiment.confidence
negativeWords <- airplaneData$negativereason
neg_confidenceNumber <-  airplaneData$negativereason.confidence



###### Getting Specific Airline Data
united <- airplaneData[airplaneData$airline == "United",]
virginAmerica <- airplaneData[airplaneData$airline == "Virgin America",]
southWest <- airplaneData[airplaneData$airline == "Southwest",]
delta <- airplaneData[airplaneData$airline == "Delta",]
usAirways <- airplaneData[airplaneData$airline == "US Airways",]
american <- airplaneData[airplaneData$airline == "American",]

#Getting Individual columns for negativereason
united$negativereason
virginAmerica$negativereason
southWest$negativereason
delta$negativereason
usAirways$negativereason
american$negativereason

# Need to break this into 3 things
united$airline_sentiment


# Variable Creation
# Using library(plyr) for the count() function to get frequencies
countofUnited <- count(united$airline_sentiment)
countofVirginAmerica <- count(virginAmerica$airline_sentiment)
countofSouthWest <- count(southWest$airline_sentiment)
countofUSAir <- count(usAirways$airline_sentiment)
countofAmerican <- count(american$airline_sentiment)

 unitedFreq <- countofUnited$freq
 virginAmericaFreq <- countofVirginAmerica$freq
 SouthWestFreq <- countofSouthWest$freq
 USAirFreq <- countofUSAir$freq
 AmericanFreq <- countofAmerican$freq


 dataToPlot <-

##### BAR GRAPH CODE BELOW
 # Gives me a canvas thats side by side for my graphs
 par(mfcol = c(1,2))
# Getting Colors I need to use
coul <- brewer.pal(5, "Set3")
out <- barplot(dataToPlot,beside=TRUE,col=c("#FB8072","#FFFFB3","#8DD3C7"),axes = TRUE,axisnames=FALSE,horiz=TRUE)
axis(2,at=2.5,labels="United",horiz=TRUE,tck=FALSE)
axis(2,at=6.5,labels="Virgin America",horiz=TRUE,tck=FALSE)
axis(2,at=10.5,labels="US Airline",horiz=TRUE,tck=FALSE)
axis(2,at=14.5,labels="American Airlines",horiz=TRUE,tck=FALSE)
axis(1,at=1200,labels=)
mtext('Frequency Of Sentiment Rating', side=1, line=3.5, at=1200)
# Legend Creation
legend("topright", inset=.02, title="Airline Sentiment Ratings",
      c("Negative","Neutral","Postive"), fill=c("#FB8072","#FFFFB3","#8DD3C7"), horiz=TRUE, cex=0.8)




#### WORLD CLOUD CODE BELOW

# Cleaning Data for WordCloud
# Word Plot
generalTweets <- Corpus(VectorSource(tweets))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
generalTweets <- tm_map(generalTweets, toSpace, "/")
generalTweets <- tm_map(generalTweets, toSpace, "@")
generalTweets <- tm_map(generalTweets, toSpace, "\\|")
generalTweets <- tm_map(generalTweets, toSpace, "the")
generalTweets <- tm_map(generalTweets, toSpace, "and")
generalTweets <- tm_map(generalTweets, toSpace, "you")
generalTweets <- tm_map(generalTweets, toSpace, "your")


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
generalFreq <- data.frame(word = names(v),freq=v)
head(generalFreq, 10)

# plotting WordCloud
wordcloud(words = generalFreq$word, freq = generalFreq$freq,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(1, "Set3"))

############################################################
  #Negative Word COUNT if needed
  # Getting word count for negativeWords
  negativeWordCount<- Corpus(VectorSource(negativeWords))
  # Convert the text to lower case
  generalTweets <- tm_map(generalTweets, content_transformer(tolower))
  # Remove numbers
  generalTweets <- tm_map(negativeWordCount, removeNumbers)
  # Remove english common stopwords
  generalTweets <- tm_map(negativeWordCount, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  generalTweets <- tm_map(negativeWordCount, removeWords, c("blabla1", "blabla2"))
  # Remove punctuations
  generalTweets <- tm_map(negativeWordCount, removePunctuation)
  # Eliminate extra white spaces
  generalTweets <- tm_map(negativeWordCount, stripWhitespace)

  dtm <- TermDocumentMatrix(negativeWordCount)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  negativeFreq <- data.frame(word = names(v),freq=v)
  head(negativeFreq, 10)
