#-------------------------------------------------------------------------------
# Name:        Twitter Demo.R 
# Purpose:     R meetup presentation - Mining Tweets with R
#
# Author:      Eu Jin Lok 
#
# Created:     19/04/2014
# Copyright:   (c) Eu Jin Lok 2014
# Licence:     GPL
#-------------------------------------------------------------------------------

Load the library
library(twitteR)

#####################################
# Load twitter credential object
#####################################
#ftp://cran.r-project.org/pub/R/web/packages/twitteR/twitteR.pdf

# Reload the authorisation
load("\\Twitter Auth Credentials.Rdata")

# get authorisation
registerTwitterOAuth(cred)

# Set up the certificate - required if run on Windows
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


##############################
# Option 1 
# Get Tweets about topic 
##############################
# search tweeter feeds based on users
who <- "@MAS" 
start <- '2014-03-20'
end <- '2014-03-21'

raw <- searchTwitter(who,  since=start, until=end, n=3200)  #current limit of 3200 tweets 
MAS.df <- twListToDF(raw)  # convert to proper dataframe structure

attributes(MAS.df$created)$tzone <- "Australia/sydney"
MAS.df <- MAS.df[MAS.df$created >= as.POSIXct("2014-02-01 00:00:00",tz = "Australia/sydney"),]

##############################
# Option 2 
# Get Tweets from user's official account 
##############################
# search tweeter feeds based on users
who <- "@MAS" 
raw <- userTimeline(who, n=3200)  #current limit of 3200 tweets 
MAS.df <- twListToDF(raw)  

attributes(MAS.df$created)$tzone <- "Australia/sydney"
MAS.df <- MAS.df[MAS.df$created >= as.POSIXct("2014-02-01 00:00:00",tz = "Australia/sydney"),]




