#Load the library
library(twitteR)

#####################################
# Setup twitter access
#####################################
#ftp://cran.r-project.org/pub/R/web/packages/twitteR/twitteR.pdf

# Reload the authorisation
load("\\Twitter Auth Credentials.Rdata")

# get authorisation
registerTwitterOAuth(cred)

# Set up the certificate - required if run on Windows
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

##############################
# Get Tweets about topic 
##############################
# search tweeter feeds based on users
who <- "@MAS" 
start <- '2014-03-20'
end <- '2014-03-21'

some_tweets <- searchTwitter(who,  since=start, until=end, n=3200)  #current limit of 3200 tweets 
MAS.df <- twListToDF(some_tweets)  # convert to proper dataframe structure

attributes(MAS.df$created)$tzone <- "Australia/sydney"
MAS.df <- MAS.df[MAS.df$created >= as.POSIXct("2014-02-01 00:00:00",tz = "Australia/sydney"),]

##############################
# Get Tweets from user's official account 
##############################
# search tweeter feeds based on users
who <- "@MAS" 
some_tweets <- userTimeline(who, n=3200)  #current limit of 3200 tweets 
MAS.df <- twListToDF(some_tweets)  

attributes(MAS.df$created)$tzone <- "Australia/sydney"
MAS.df <- MAS.df[MAS.df$created >= as.POSIXct("2014-02-01 00:00:00",tz = "Australia/sydney"),]




