#-------------------------------------------------------------------------------
# Name:        Utilities.R 
# Purpose:     R meetup presentation - Mining Tweets with R
#
# Author:      Eu Jin Lok 
#
# Created:     19/04/2014
# Copyright:   (c) Eu Jin Lok 2014
# Licence:     GPL
#-------------------------------------------------------------------------------

#############################
# Load all the key libraries 
#############################

library(twitteR)
library(sentiment)  # need to install manually from: C:\Users\Eu\Dropbox\R GIT project\doc
library(ggplot2)
library(wordcloud)
library(qdap) #http://cran.us.r-project.org/web/packages/qdap/qdap.pdf 
library(stringr)
library(cluster)
library(FactoMineR)
library(ca)
library(tm)
library(igraph)
library(maps)
library(geosphere)
library(RColorBrewer)
library(rjson)
library(bitops)
library(plyr)
library(sna)
library(rgexf)

#####################################
# Load twitter credential object
#####################################

# Reload the authorisation
load("\\Twitter Auth Credentials.Rdata")

# get authorisation
registerTwitterOAuth(cred)

# Set up the certificate - required if run on Windows
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


#################################################################
# Functions for the Sentiment analysis ala Jeff Breen's style 
# As taken from here: https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107
#################################################################
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}


###########################################
# Use Regex to clean the text fields  
###########################################

clean.text <- function(data){
  
  raw = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", raw)
  
  # remove aliases @
  raw = gsub("@\\w+", "", raw)
  # remove hashtags #
  raw = gsub("#\\w+", "", raw)
  # remove punctuation
  raw = gsub("[[:punct:]]", "", raw)
  # remove html links
  raw = gsub("http\\w+", "", raw)
  # remove unnecessary spaces
  raw = gsub("[ \t]{2,}", "", raw)
  raw = gsub("^\\s+|\\s+$", "", raw)
  
  # make sure the object (data) to be analyse on is a character type
  raw <- as.character (raw)
  return(raw)
}