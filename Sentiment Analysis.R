#-------------------------------------------------------------------------------
# Name:        Sentiment Analysis.R 
# Purpose:     R meetup presentation - Mining Tweets with R
#
# Author:      Eu Jin Lok 
#
# Created:     19/04/2014
# Copyright:   (c) Eu Jin Lok 2014
# Licence:     GPL
#-------------------------------------------------------------------------------

########################################################
# I'm using Jeffrey Breen's approach to this sentiment analysis. Credit to Jeffrey for the precedent:
# https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107
########################################################

# Step 1: Load all the necessary libraries, Oauth and etc
source("Utilities.R")

# Step 2: Download the word dictionaries, which is available on the 'dictionaries' folder


# Step 3: We need to import the files containing the positive and negative words
# import positive and negative words
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")


########################################################
# Sentiment analysis of MAS from 10th March to 20th March
########################################################

# Step 4: Let's harvest tweets talking about the local 4 big airlines. Plot the before and after sentiments. 
MAS_tweets = searchTwitter("@mas", n=3200, lang="en")    
SIA_tweets = searchTwitter("@SingaporeAir", n=3200, lang="en")
Qantas_tweets = searchTwitter("@QantasAirways", n=3200, lang="en")
Virgin_tweets = searchTwitter("@Virgin", n=3200, lang="en")

# get text
MAS_txt = sapply(MAS_tweets, function(x) x$getText())
SIA_txt = sapply(SIA_tweets, function(x) x$getText())
Qantas_txt = sapply(Qantas_tweets, function(x) x$getText())
Virgin_txt = sapply(Virgin_tweets, function(x) x$getText())


# how many tweets of each drink
nd = c(length(MAS_txt), length(SIA_txt), length(Qantas_txt), length(Virgin_txt))

# join texts
Airline = c(MAS_txt, SIA_txt, Qantas_txt, Virgin_txt) 

#####################################
# Apply score.sentiment and calculate more results
#####################################
# apply function score.sentiment
scores = score.sentiment(Airline, pos, neg, .progress='text')

# add variables to data frame
scores$Airline = factor(rep(c("MAS", "SIA", "Qantas", "Virgin"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

# how many very positives and very negatives
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

# global score
global_score = round( 100 * numpos / (numpos + numneg) )


# Step 6: Get a boxplot
# colors
cols = c("#7CAE00", "#00BFC4", "#F8766D", "#C77CFF")
names(cols) = c("MAS", "SIA", "Qantas", "Virgin")

windows()

# boxplot
ggplot(scores, aes(x=Airline, y=score, group=Airline)) +
  geom_boxplot(aes(fill=Airline)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) +
  labs(title = "Boxplot - Airline's Sentiment Scores")



#####################################
#Step 7: Make some barplots
#####################################
# barplot of average score
meanscore = tapply(scores$score, scores$Airline, mean)
df = data.frame(Airline=names(meanscore), meanscore=meanscore)
df$Airline <- reorder(df$Airline, df$meanscore)

ggplot(df, aes(y=meanscore)) +
  geom_bar(data=df, aes(x=Airline, fill=Airline)) +
  scale_fill_manual(values=cols[order(df$meanscore)]) +
  labs(title = "Average Sentiment Score",
       legend.position = "none")


# barplot of average very positive
Airline_pos = ddply(scores, .(Airline), summarise, mean_pos=mean(very.pos))
Airline_pos$Airline <- reorder(Airline_pos$Airline, Airline_pos$mean_pos)

ggplot(Airline_pos, aes(y=mean_pos)) +
  geom_bar(data=Airline_pos, aes(x=Airline, fill=Airline)) +
  scale_fill_manual(values=cols[order(Airline_pos$mean_pos)]) +
  labs(title = "Average Very Positive Sentiment Score",
       legend.position = "none")

# barplot of average very negative
Airline_neg = ddply(scores, .(Airline), summarise, mean_neg=mean(very.neg))
Airline_neg$Airline <- reorder(Airline_neg$Airline, Airline_neg$mean_neg)


ggplot(Airline_neg, aes(y=mean_neg)) +
  geom_bar(data=Airline_neg, aes(x=Airline, fill=Airline)) +
  scale_fill_manual(values=cols[order(Airline_neg$mean_neg)]) +
  labs(title = "Average Very Negative Sentiment Score",
       legend.position = "none")







