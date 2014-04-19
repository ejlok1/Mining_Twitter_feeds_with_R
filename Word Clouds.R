#-------------------------------------------------------------------------------
# Name:        Word Cloud.R 
# Purpose:     R meetup presentation - Mining Tweets with R
#
# Author:      Eu Jin Lok 
#
# Created:     19/04/2014
# Copyright:   (c) Eu Jin Lok 2014
# Licence:     GPL
#-------------------------------------------------------------------------------

# Run the Demo twitteR.R script to obtain the 'raw' object

####################################
# Create corpus 
####################################

text <- Corpus(VectorSource(raw))  
text <- tm_map(text, removePunctuation)
text <- tm_map(text, tolower)
text <- tm_map(text, function(x) removeWords(x, stopwords("english")))
text <- tm_map(text, stripWhitespace)
tdm <- TermDocumentMatrix(text)
dim(tdm)
removeSparseTerms(tdm,0.2)

################################################
# vizualisation - word cloud
###############################################

ap.m <- as.matrix(tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
table(ap.d$freq)

#set background colour
par(bg="black")

windows()

#Plot word cloud
wordcloud(cloud$word,cloud$freq, scale=c(3,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=brewer.pal(8,"Dark2"))


