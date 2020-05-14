# loading in libraries
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("wordcloud2")
library("plyr")
library("pracma")

# Frequency Function Recleaning Code here too Just in case.
getWordFreq <- function(text){
  require("tm")
  require("SnowballC")
  require("wordcloud")
  require("RColorBrewer")

  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  excludeWords <- c("the","for", "was", "that", "this","get","but")
  docs <- tm_map(docs, removeWords, excludeWords)
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, toSpace, "the")
  docs <- tm_map(docs, toSpace, "and")
  docs <- tm_map(docs, toSpace, "you")
  docs <- tm_map(docs, toSpace, "your")
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  excludeWords <- c("the","for", "was", "that", "this","get","but")
  docs <- tm_map(docs, removeWords, excludeWords)
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}
# loading in data
# airplaneData <- read.csv("/Users/jonathancanales/Desktop/FinalProjectR/Airline.csv")
airplaneData <- read.csv("~/Downloads/FinalProjectR/Airline.csv")
# remove incomplete cases
airplaneData <- airplaneData[complete.cases(airplaneData),]


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

# Variable Creation
# Using library(plyr) for the count() function to get frequencies
countofUnited <- count(united$airline_sentiment)
countofVirginAmerica <- count(virginAmerica$airline_sentiment)
countofSouthWest <- count(southWest$airline_sentiment)
countofUSAir <- count(usAirways$airline_sentiment)
countofAmerican <- count(american$airline_sentiment)
countofDelta <- count(delta$airline_sentiment)

unitedFreq <- countofUnited$freq
virginAmericaFreq <- countofVirginAmerica$freq
SouthWestFreq <- countofSouthWest$freq
USAirFreq <- countofUSAir$freq
AmericanFreq <- countofAmerican$freq

##### BAR GRAPH CODE BELOW
# Gives me a canvas thats side by side for my graphs
par(mfcol = c(1,2))
# Getting Colors I need to use
dataToPlot <- rbind(countofUnited$freq,countofVirginAmerica$freq,countofSouthWest$freq,countofUSAir$freq,countofAmerican$freq,countofDelta$freq)
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
# Remove punctuations
generalTweets <- tm_map(generalTweets, removePunctuation)
# Eliminate extra white spaces
generalTweets <- tm_map(generalTweets, stripWhitespace)
# Remove your own stopwords
excludeWords <- c("the","for", "was", "that", "this","get","but")
generalTweets <- tm_map(generalTweets, removeWords, excludeWords)
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
# Text stemming
# Create term-document matrix
tdm <- TermDocumentMatrix(generalTweets)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
text <- data.frame(word = names(v),freq=v)
head(text, 10)
# The variable for this data is text


# plotting WordCloud
wordcloud(words = text$word, freq = text$freq,min.freq = 1,
  max.words=200, random.order=FALSE, rot.per=0.35,
  colors=brewer.pal(1, "Set3"))






  # Remove neutral ratings
  airplaneData <- airplaneData[airplaneData$airline_sentiment != "neutral",]
  tweets <- airplaneData[airplaneData$airline == "United", "text"]
  tweetsVA <- airplaneData[airplaneData$airline == "Virgin America", "text"]
  tweetsUS <- airplaneData[airplaneData$airline == "US Airways", "text"]
  tweetsAmerican <- airplaneData[airplaneData$airline == "American", "text"]
  tweetsDelta <- airplaneData[airplaneData$airline == "Delta", "text"]
  tweetsSW <- airplaneData[airplaneData$airline == "Southwest", "text"]

  # Recode for regression
  # If postive = 1 , negative = 0
  recodeSentiment <- ifelse(sentiment == "positive",1, 0)
  # dimensions of United Data
  nTweets <- dim(airplaneData[airplaneData$airline == "United",])[1]
  nTweetsVA <- dim(airplaneData[airplaneData$airline == "Virgin America",])[1]
  nTweetsUS <- dim(airplaneData[airplaneData$airline == "US Airways",])[1]
  nTweetsAmerican <- dim(airplaneData[airplaneData$airline == "American",])[1]
  nTweetsDelta <- dim(airplaneData[airplaneData$airline == "Delta",])[1]
  nTweetsSW <- dim(airplaneData[airplaneData$airline == "Southwest",])[1]
  # begin creating the tf-idf data frame
  # So From reading up on tf-idf what this does is gives more meaning and weight to our text data.
  # Instead of jsut getting a frequency we get how meaningful that word is compared by how many times it appears in the text and how many
  # times it appears in other documents and uses that to give us a weight on how important that word is.
  # Common words will not have any weight to them since they are common words

  # Gives us the first tweet from the united Data
  text <- as.character(tweets[1])
  textVA <- as.character(tweetsVA[1])
  textUS <- as.character(tweetsUS[1])
  textAmerican <- as.character(tweetsAmerican[1])
  textDelta <- as.character(tweetsDelta[1])
  textSW <- as.character(tweetsSW[1])

  text <- gsub("[^[:alnum:]=\\.]"," ",text)
  textVA <- gsub("[^[:alnum:]=\\.]"," ",textVA)
  textUS <- gsub("[^[:alnum:]=\\.]"," ",textUS)
  textAmerican <- gsub("[^[:alnum:]=\\.]"," ",textAmerican)
  textDelta <- gsub("[^[:alnum:]=\\.]"," ",textDelta)
  textSW <- gsub("[^[:alnum:]=\\.]"," ",textSW)
  #Gives us word Frequency
  D <- getWordFreq(text)
  vaFreq <- getWordFreq(textVA)
  USFreq <- getWordFreq(textUS)
  AFreq <- getWordFreq(textAmerican)
  DeltaFreq <- getWordFreq(textDelta)
  SWFreq <- getWordFreq(textSW)
  listOfWords <- data.frame(tweetNum = rep(1, length(D$word)), words = as.character(D$word), freqInDocument = D$freq, totalWords = rep(sum(D$freq), length(D$word)))
  listOfWordsVA <- data.frame(tweetNum = rep(1, length(vaFreq$word)), words = as.character(vaFreq$word), freqInDocument = vaFreq$freq, totalWords = rep(sum(vaFreq$freq), length(vaFreq$word)))
  listOfWordsUS <- data.frame(tweetNum = rep(1, length(USFreq$word)), words = as.character(USFreq$word), freqInDocument = USFreq$freq, totalWords = rep(sum(USFreq$freq), length(USFreq$word)))
  listOfWordsAmerican <- data.frame(tweetNum = rep(1, length(AFreq$word)), words = as.character(AFreq$word), freqInDocument = AFreq$freq, totalWords = rep(sum(AFreq$freq), length(AFreq$word)))
  listOfWordsDelta <- data.frame(tweetNum = rep(1, length(DeltaFreq$word)), words = as.character(DeltaFreq$word), freqInDocument = DeltaFreq$freq, totalWords = rep(sum(DeltaFreq$freq), length(DeltaFreq$word)))
  listOfWordsSW <- data.frame(tweetNum = rep(1, length(SWFreq$word)), words = as.character(SWFreq$word), freqInDocument = SWFreq$freq, totalWords = rep(sum(SWFreq$freq), length(SWFreq$word)))


# Model for United : Loops, tfidf and Model
  for (i in 2:nTweets){
    text <- as.character(tweets[i])
    text <- gsub("[^[:alnum:]=\\.]"," ",text)
    D <- getWordFreq(text)
    newWords <- data.frame(tweetNum = rep(i, length(D$word)), words = as.character(D$word),freqInDocument= D$freq,totalWords = rep(sum(D$freq), length(D$word)))
    listOfWords <- rbind(listOfWords, newWords)
  }
  listOfWords[, "termFreq"] <- listOfWords[,"freqInDocument"]/listOfWords[,"totalWords"]

  # Very Long Loop
  for (i in 1:dim(listOfWords)[1]){
    nDocumentsCont <- length(unique(listOfWords[listOfWords[, "words"] == listOfWords[i, "words"],1]))
    listOfWords[i, "IDF"] <- 1+log(1+nTweets/1+nDocumentsCont)
  }
    # Making teh tfidf column
    listOfWords[,"tfidf"] <- listOfWords[,"termFreq"]*listOfWords[,"IDF"]

    #Look for the best words
    SORTIDX<-order(listOfWords$tfidf, decreasing = TRUE)
    commonWords <- unique(listOfWords[SORTIDX,][2])
    commonWords[1:10,"words"]
    wordsToUse <- commonWords[1:10,]

    #The most common/special word is United, skip that word and go to the second
    # Get the tweets that contain the 10 words to get the sentiment score from the airplaneData data frame
    tweetIDX <- NULL
    for (i in 2:10){
      tweetIDX <- c(tweetIDX,listOfWords[listOfWords$words == wordsToUse[i],1])
    }
##################################################################

# # Model for Virgin America : Loops, tfidf and Model
  for (i in 2:nTweetsVA){
    textVA <- as.character(tweetsVA[i])
    textVA <- gsub("[^[:alnum:]=\\.]"," ",textVA)
    vaFreq <- getWordFreq(textVA)
    newWordsVA <- data.frame(tweetNum = rep(i, length(vaFreq$word)), words = as.character(vaFreq$word),freqInDocument= vaFreq$freq,totalWords = rep(sum(vaFreq$freq), length(vaFreq$word)))
    listOfWordsVA <- rbind(listOfWordsVA, newWordsVA)
  }
  listOfWordsVA[, "termFreq"] <- listOfWordsVA[,"freqInDocument"]/listOfWordsVA[,"totalWords"]
  # Very Long Loop
  for (i in 1:dim(listOfWordsVA)[1]){
    nDocumentsContVA <- length(unique(listOfWordsVA[listOfWordsVA[, "words"] == listOfWordsVA[i, "words"],1]))
    listOfWordsVA[i, "IDF"] <- 1+log(1+nTweetsVA/1+nDocumentsContVA)
  }
    # TFIDF MODEL
    listOfWordsVA[,"tfidf"] <- listOfWordsVA[,"termFreq"]*listOfWordsVA[,"IDF"]

    #Look for the best words
    SORTIDXVA<-order(listOfWordsVA$tfidf, decreasing = TRUE)
    commonWordsVA <- unique(listOfWordsVA[SORTIDXVA,][2])
    commonWordsVA[1:10,"words"]
    wordsToUseVA <- commonWordsVA[1:10,]

    #The most common/special word is United, skip that word and go to the second
    # Get the tweets that contain the 10 words to get the sentiment score from the airplaneData data frame
    tweetIDXVA <- NULL
    for (i in 2:10){
      tweetIDXVA <- c(tweetIDXVA,listOfWordsVA[listOfWordsVA$words == wordsToUseVA[i],1])
    }

    # Virgin America Model
    # create a new data frame that contains the recoded sentiment score and the tfidf value of the 10 words selected
    VAregression.data <- data.frame(sentiment = recodeSentiment[tweetIDXVA],
    tfidf = listOfWordsVA[tweetIDXVA, "tfidf"])
    #Model 1 words: United, done
    #Logestic Regression done here from sentinment and tfidf score
    modelVA <- glm(sentiment~tfidf, data = VAregression.data, family = "binomial")
    # Making predection column
    VAregression.data[, "pred"] = predict(modelVA, newdata = VAregression.data, type = "response") > .5
    #How accurate our model is
    VAmodelAccuracy <- mean(VAregression.data$sentiment == VAregression.data$pred)



##################################################
# # Model for US Airways : Loops, tfidf and Model
for (i in 2:nTweetsUS){
  textUS <- as.character(tweetsUS[i])
  textUS <- gsub("[^[:alnum:]=\\.]"," ",textUS)
  USFreq <- getWordFreq(textUS)
  newWordsUS <- data.frame(tweetNum = rep(i, length(USFreq$word)), words = as.character(USFreq$word),freqInDocument= USFreq$freq,totalWords = rep(sum(USFreq$freq), length(USFreq$word)))
  listOfWordsUS <- rbind(listOfWordsUS, newWordsUS)
}
listOfWordsUS[, "termFreq"] <- listOfWordsUS[,"freqInDocument"]/listOfWordsUS[,"totalWords"]
# Very Long Loop
for (i in 1:dim(listOfWordsUS)[1]){
  nDocumentsContUS <- length(unique(listOfWordsUS[listOfWordsUS[, "words"] == listOfWordsUS[i, "words"],1]))
  listOfWordsUS[i, "IDF"] <- 1+log(1+nTweetsUS/1+nDocumentsContUS)
}

# TFIDF MODEL
listOfWordsUS[,"tfidf"] <- listOfWordsUS[,"termFreq"]*listOfWordsUS[,"IDF"]

#Look for the best words
SORTIDXUS<-order(listOfWordsUS$tfidf, decreasing = TRUE)
commonWordsUS <- unique(listOfWordsUS[SORTIDXUS,][2])
commonWordsUS[1:10,"words"]
wordsToUseUS <- commonWordsUS[1:10,]

#The most common/special word is United, skip that word and go to the second
# Get the tweets that contain the 10 words to get the sentiment score from the airplaneData data frame
tweetIDXUS <- NULL
for (i in 2:10){
  tweetIDXUS <- c(tweetIDXUS,listOfWordsUS[listOfWordsUS$words == wordsToUseUS[i],1])
}

# US Airways Model
# create a new data frame that contains the recoded sentiment score and the tfidf value of the 10 words selected
USregression.data <- data.frame(sentiment = recodeSentiment[tweetIDXUS],
tfidf = listOfWordsUS[tweetIDXUS, "tfidf"])
#Model 1 words: United, done
#Logestic Regression done here from sentinment and tfidf score
modelUS <- glm(sentiment~tfidf, data = USregression.data, family = "binomial")
# Making predection column
USregression.data[, "pred"] = predict(modelUS, newdata = USregression.data, type = "response") > .5
#How accurate our model is
USmodelAccuracy <- mean(USregression.data$sentiment == USregression.data$pred)


#########################################################

# Model for American Airlines : Loops, tfidf and Model
for (i in 2:nTweetsAmerican){
  textAmerican <- as.character(tweetsAmerican[i])
  textAmerican <- gsub("[^[:alnum:]=\\.]"," ",textAmerican)
  AmericanFreq <- getWordFreq(textAmerican)
  newWordsAmerican <- data.frame(tweetNum = rep(i, length(AFreq$word)), words = as.character(AFreq$word),freqInDocument= AFreq$freq,totalWords = rep(sum(AFreq$freq), length(AFreq$word)))
  listOfWordsAmerican <- rbind(listOfWordsAmerican, newWordsAmerican)
}
listOfWordsAmerican[, "termFreq"] <- listOfWordsAmerican[,"freqInDocument"]/listOfWordsAmerican[,"totalWords"]
# Very Long Loop
for (i in 1:dim(listOfWordsAmerican)[1]){
  nDocumentsContAmerican <- length(unique(listOfWordsAmerican[listOfWordsAmerican[, "words"] == listOfWordsAmerican[i, "words"],1]))
  listOfWordsAmerican[i, "IDF"] <- 1+log(1+nTweetsAmerican/1+nDocumentsContAmerican)
}
# TFIDF MODEL
listOfWordsAmerican[,"tfidf"] <- listOfWordsAmerican[,"termFreq"]*listOfWordsAmerican[,"IDF"]

#Look for the best words
SORTIDXAmerican<-order(listOfWordsAmerican$tfidf, decreasing = TRUE)
commonWordsAmerican <- unique(listOfWordsAmerican[SORTIDXAmerican,][2])
commonWordsAmerican[1:10,"words"]
wordsToUseAmerican <- commonWordsAmerican[1:10,]

#The most common/special word is United, skip that word and go to the second
# Get the tweets that contain the 10 words to get the sentiment score from the airplaneData data frame
tweetIDXAmerican <- NULL
for (i in 2:10){
  tweetIDXAmerican <- c(tweetIDXAmerican,listOfWordsAmerican[listOfWordsAmerican$words == wordsToUseAmerican[i],1])
}
# American Airlines Model
# create a new data frame that contains the recoded sentiment score and the tfidf value of the 10 words selected
Americanregression.data <- data.frame(sentiment = recodeSentiment[tweetIDXAmerican],
tfidf = listOfWordsAmerican[tweetIDXAmerican, "tfidf"])
#Model 1 words: United, done
#Logestic Regression done here from sentinment and tfidf score
modelAmerican <- glm(sentiment~tfidf, data = Americanregression.data, family = "binomial")
# Making predection column
Americanregression.data[, "pred"] = predict(modelAmerican, newdata = Americanregression.data, type = "response") > .5
#How accurate our model is
AmericanmodelAccuracy <- mean(Americanregression.data$sentiment == Americanregression.data$pred)




#############################################

# # Model for Delta : Loops, tfidf and Model
for (i in 2:nTweetsDelta){
  textDelta <- as.character(tweetsDelta[i])
  textDelta <- gsub("[^[:alnum:]=\\.]"," ",textDelta)
  DeltaFreq <- getWordFreq(textDelta)
  newWordsDelta <- data.frame(tweetNum = rep(i, length(DeltaFreq$word)), words = as.character(DeltaFreq$word),freqInDocument= DeltaFreq$freq,totalWords = rep(sum(DeltaFreq$freq), length(DeltaFreq$word)))
  listOfWordsDelta <- rbind(listOfWordsDelta, newWordsDelta)
}
listOfWordsDelta[, "termFreq"] <- listOfWordsDelta[,"freqInDocument"]/listOfWordsDelta[,"totalWords"]
# Very Long Loop
for (i in 1:dim(listOfWordsDelta)[1]){
  nDocumentsContDelta <- length(unique(listOfWordsDelta[listOfWordsDelta[, "words"] == listOfWordsDelta[i, "words"],1]))
  listOfWordsDelta[i, "IDF"] <- 1+log(1+nTweetsDelta/1+nDocumentsContDelta)
}

# TFIDF MODEL
listOfWordsDelta[,"tfidf"] <- listOfWordsDelta[,"termFreq"]*listOfWordsDelta[,"IDF"]

#Look for the best words
SORTIDXDelta<-order(listOfWordsDelta$tfidf, decreasing = TRUE)
commonWordsDelta <- unique(listOfWordsDelta[SORTIDXDelta,][2])
commonWordsDelta[1:10,"words"]
wordsToUseDelta <- commonWordsDelta[1:10,]

#The most common/special word is United, skip that word and go to the second
# Get the tweets that contain the 10 words to get the sentiment score from the airplaneData data frame
tweetIDXDelta <- NULL
for (i in 2:10){
  tweetIDXDelta <- c(tweetIDXDelta,listOfWordsDelta[listOfWordsDelta$words == wordsToUseDelta[i],1])
}
# Delta Model
# create a new data frame that contains the recoded sentiment score and the tfidf value of the 10 words selected
Deltaregression.data <- data.frame(sentiment = recodeSentiment[tweetIDXDelta],
tfidf = listOfWordsDelta[tweetIDXDelta, "tfidf"])
#Model 1 words: United, done
#Logestic Regression done here from sentinment and tfidf score
modelDelta <- glm(sentiment~tfidf, data = Deltaregression.data, family = "binomial")
# Making predection column
Deltaregression.data[, "pred"] = predict(modelDelta, newdata = Deltaregression.data, type = "response") > .5
#How accurate our model is
DeltamodelAccuracy <- mean(Deltaregression.data$sentiment == Deltaregression.data$pred)



#################################################
 # Model for Southwest : Loops, tfidf, and Model
for (i in 2:nTweetsSW){
  textSW <- as.character(tweetsSW[i])
  textSW <- gsub("[^[:alnum:]=\\.]"," ",textSW)
  DeltaSW <- getWordFreq(textSW)
  newWordsSW <- data.frame(tweetNum = rep(i, length(SWFreq$word)), words = as.character(SWFreq$word),freqInDocument= SWFreq$freq,totalWords = rep(sum(SWFreq$freq), length(SWFreq$word)))
  listOfWordsSW <- rbind(listOfWordsSW, newWordsSW)
}
listOfWordsSW[, "termFreq"] <- listOfWordsSW[,"freqInDocument"]/listOfWordsSW[,"totalWords"]
# Very Long Loop
for (i in 1:dim(listOfWordsSW)[1]){
  nDocumentsContSW <- length(unique(listOfWordsSW[listOfWordsSW[, "words"] == listOfWordsSW[i, "words"],1]))
  listOfWordsSW[i, "IDF"] <- 1+log(1+nTweetsSW/1+nDocumentsContSW)
}

# TFIDF MODEL
listOfWordsSW[,"tfidf"] <- listOfWordsSW[,"termFreq"]*listOfWordsSW[,"IDF"]

#Look for the best words
SORTIDXSW<-order(listOfWordsSW$tfidf, decreasing = TRUE)
commonWordsSW <- unique(listOfWordsSW[SORTIDXSW,][2])
commonWordsSW[1:10,"words"]
wordsToUseSW <- commonWordsSW[1:10,]

#The most common/special word is United, skip that word and go to the second
# Get the tweets that contain the 10 words to get the sentiment score from the airplaneData data frame
tweetIDXSW <- NULL
for (i in 2:10){
  tweetIDXSW <- c(tweetIDXSW,listOfWordsSW[listOfWordsSW$words == wordsToUseSW[i],1])
}

# SW Model
# create a new data frame that contains the recoded sentiment score and the tfidf value of the 10 words selected
SWregression.data <- data.frame(sentiment = recodeSentiment[tweetIDXSW],
tfidf = listOfWordsSW[tweetIDXSW, "tfidf"])
#Model 1 words: United, done
#Logestic Regression done here from sentinment and tfidf score
modelSW <- glm(sentiment~tfidf, data = SWregression.data, family = "binomial")
# Making predection column
SWregression.data[, "pred"] = predict(modelSW, newdata = SWregression.data, type = "response") > .5
#How accurate our model is
SWmodelAccuracy <- mean(SWregression.data$sentiment == SWregression.data$pred)




####################################### # # # # # # #
# test and train datasets here 70/30 split
set.seed(123)
indices <- sample(nrow(airplaneData),0.70 * nrow(airplaneData))
train <- airplaneData[indices, ]
#########


# Bar Plot to show different model accuracy
totalModelAcc <- c(DeltamodelAccuracy,AmericanmodelAccuracy,USmodelAccuracy,VAmodelAccuracy,modelAccuracy)
out <- barplot(totalModelAcc,beside=TRUE,col=c("#FB8072","#D9D9D9","#8DD3C7","#B3DE69", "#80B1D3"),axes = TRUE,axisnames=FALSE,ylim=c(0,0.99),main= "Model Accuracy")
axis(1,at=0.7,labels="Delta",tck=FALSE)
axis(1,at=1.9,labels="American Airlines",tck=FALSE)
axis(1,at=3.1,labels="US Airline",tck=FALSE)
axis(1,at=4.3,labels="Virgin America ",tck=FALSE)
axis(1,at=5.5,labels="United",tck=FALSE)
