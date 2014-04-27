#-------------------------------------------------------------------------------
# Name:        Hashtag Network.R 
# Purpose:     R meetup presentation - Mining Tweets with R
#
# Author:      Eu Jin Lok 
#
# Created:     19/04/2014
# Copyright:   (c) Eu Jin Lok 2014
# Licence:     GPL
#-------------------------------------------------------------------------------

# Load all the necessary libraries, Oauth and etc
source("Utilities.R")

######################
# Hashtag network plot
# helpful resources from: http://jkunst.com/
# NOTE: You need to first run line 20 to 25 from the 'Twitter Demo.R' script
######################

tweets <- tolower(MAS.df$text)
head(tweets)
hashtags_remove <- c("#r")

# Cleaning the tweets
for(term in hashtags_remove) tweets <- gsub(term, "", tweets)

# Extract the hastags
hashtags <- unique(unlist(str_extract_all(tolower(tweets), "#\\w+")))
hashtags <- setdiff(hashtags, hashtags_remove)

# Capture the node size according the amount that appear
nodesizes <- laply(hashtags, function(hashtag){
  sum(grepl(hashtag, tweets))
})

#Create frequency table for plots 
freq.table <- cbind(hashtags,nodesizes)
freq.table <- freq.table[order(nodesizes,decreasing = TRUE),]

# scaling  sizes
nodesizes <-  1 + log(nodesizes, base = 3)
nodes <- data.frame(id = c(1:length(hashtags)), label = hashtags, stringsAsFactors=F)

# Get the relations table
relations <- ldply(hashtags, function(hashtag){
  hashtag_related <- unlist(str_extract_all(tweets[grepl(hashtag, tweets)], "#\\w+"))
  hashtag_related <- setdiff(hashtag_related, hashtag) 
  if(length(hashtag_related)==0){
    return(data.frame())
  }
  data.frame(source = which(hashtags==hashtag),
             target =  which(hashtags %in% hashtag_related))
})


for(row in 1:nrow(relations)){  
  relations[row,] <- sort(relations[row,])
}

relations <- unique(relations)

# Some colors
nodecolors <- data.frame(r = sample(1:249, size = nrow(nodes), replace=T),
                         g = sample(1:249, size = nrow(nodes), replace=T),
                         b = sample(1:249, size = nrow(nodes), replace=T),
                         a = 1)


links <- matrix(rep(0, length(hashtags)^2), ncol = length(hashtags))
for(edge in 1:nrow(relations)){
  links[(relations[edge,]$target), (relations[edge,]$source)] <- 1
}

positions <- gplot.layout.kamadakawai(links, layout.par=list())
#http://igraph.sourceforge.net/doc/R/layout.html
positions <- cbind(positions, 0) # needs a z axis


# The exciting part
graph <- write.gexf(nodes=nodes,
                    edges=relations,
                    nodesVizAtt=list(
                      color=nodecolors,
                      size=nodesizes,
                      position=positions))

plot(graph)




