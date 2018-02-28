library(tm)
library(SnowballC)
library(wordcloud)

library(twitteR)
 # retrieve the first 100 tweets (or all tweets if fewer than 100)
   # from the user timeline of @rdatammining
   rdmTweets <- userTimeline("trump", n=100)
   n <- length(rdmTweets)
  rdmTweets[1:50]

  df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
  dim(df)

  
  df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #remove emoticon
  df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text) #remove URL
myCorpus <- Corpus(VectorSource(df$text))



 # remove punctuation
   myCorpus <- tm_map(myCorpus, removePunctuation)
 # remove numbers
   myCorpus <- tm_map(myCorpus, removeNumbers)
   
   # remove stopwords
    myStopwords <- c(stopwords('english'),"available", "via")
    idx <- which(myStopwords == "r")
    # keep "r" by removing it from stopwords
      myStopwords <- myStopwords[-idx]
   myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

dictCorpus <- myCorpus
 # stem words in a text document with the snowball stemmers,
   # which requires packages Snowball, RWeka, rJava, RWekajars
   myCorpus <- tm_map(myCorpus, stemDocument)
 # inspect the first three "document"
 inspect(myCorpus[1:50])
 
 
 
 
 sample <- df$text
 