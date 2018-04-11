# Installing package if not already installed 
EnsurePackage<-function(x)
{x <- as.character(x)
if (!require(x,character.only=TRUE))
{
  install.packages(pkgs=x,repos="http://cran.r-project.org")
  require(x,character.only=TRUE)
}
}

#Identifying packages required  
PrepareTwitter<-function()
{
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("RJSONIO")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  #EnsurePackage("gplots") Not required... ggplot2 is used
  EnsurePackage("plyr")
  EnsurePackage("e1071")
  EnsurePackage("RTextTools")
}

PrepareTwitter()

shinyServer(function(input, output) {
  
  #Search tweets and create a data frame 
  # Clean the tweets
  TweetFrame<-function(twtList)
  {
    #tweet <- strip_retweets(twtList, strip_manual = TRUE, strip_mt = TRUE)
    df<- do.call("rbind",lapply(twtList,as.data.frame))
    #removes emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    return (df$text)
  }
  
  
  # Function to create a data frame from tweets
  extra.words = scan('C:/Users/acerpc/Documents/Big5/extraversion.txt', what='character', comment.char=';') 
  agree.words = scan('C:/Users/acerpc/Documents/Big5/agreeableness.txt', what='character', comment.char=';')
  open.words = scan('C:/Users/acerpc/Documents/Big5/openness.txt', what='character', comment.char=';')
  cons.words = scan('C:/Users/acerpc/Documents/Big5/conscientious.txt', what='character', comment.char=';')
  neuro.words = scan('C:/Users/acerpc/Documents/Big5/neuroticism.txt', what='character', comment.char=';')
  
  
  wordDatabase<-function()
  {
    extra.words<<-c(extra.words,'extraversion')
    agree.words<<-c(agree.words,'agreeableness')
    open.words<<-c(open.words, 'openness')
    cons.words<<-c(cons.words, 'conscientiousness')
    neuro.words<<-c(neuro.words, 'neuroticism')
    
  }
  
  score.sentiment = function(sentences, extra.words, agree.words,open.words,cons.words,neuro.words, .progress='none')
  {
    require(plyr)                  # extra agree open cons neuro
    require(stringr)
    
    
    list=lapply(sentences, function(sentence, extra.words, agree.words, cons.words, open.words, neuro.words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)  #removes decimal number
      sentence = gsub('\n','',sentence)    #removes new lines
      
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)  #changes a list to character vector
      extra.matches = match(words, extra.words)
      agree.matches = match(words, agree.words)
      open.matches = match(words, open.words)
      cons.matches = match(words, cons.words)
      neuro.matches = match(words, neuro.words)
      agree.matches = !is.na(agree.matches)
      extra.matches = !is.na(extra.matches)
      open.matches = !is.na(open.matches)
      cons.matches = !is.na(cons.matches)
      neuro.matches = !is.na(neuro.matches)
      ee = sum(extra.matches)
      aa = sum(agree.matches)
      oo = sum(open.matches)
      cc = sum(cons.matches)
      nn = sum(neuro.matches)
      
      list1 = c(ee,aa,oo,cc,nn)
      return (list1)
    }, extra.words, agree.words, open.words, cons.words, neuro.words)
    ee1 = lapply(list, `[[`, 1)
    aa1 = lapply(list, `[[`, 2)
    oo1 = lapply(list, `[[`, 3)
    cc1 = lapply(list, `[[`, 4)
    nn1 = lapply(list, `[[`, 5)
    
    extraversion.df = data.frame(Extraversion = ee1, text=sentences)
    openness.df = data.frame(Openness = oo1, text=sentences)
    agreeableness.df = data.frame(Agreeableness = aa1, text=sentences)
    conscientiousness.df = data.frame(Conscientiousness = cc1, text=sentences)
    neuroticism.df = data.frame(Neuroticism = nn1, text=sentences)
    
    list_df = list(extraversion.df, openness.df, agreeableness.df, conscientiousness.df, neuroticism.df)
    return(list_df)
  }
  
  #TABLE DATA	
  
  library(reshape)
  sentimentAnalyser<-function(result)
  {
    library(reshape)
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    test4=result[[4]]
    test5=result[[5]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    test4$text=NULL
    test5$text=NULL
    
    
    
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    q4=test4[1,]
    q5=test5[1,]
    qq1=melt(q1, , var='Extraversion')
    qq2=melt(q2, , var='Agreeableness')
    qq3=melt(q3, , var='Openness') 
    qq4=melt(q4, , var='Conscientiousness')
    qq5=melt(q5, , var='Neuroticism')
    qq1['Extraversion'] = NULL
    qq2['Agreeableness'] = NULL
    qq3['Openness'] = NULL
    qq4['Conscientiousness']=NULL
    qq5['Neuroticism']=NULL
    
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    table4 = data.frame(Text=result[[4]]$text, Score=qq4)
    table5 = data.frame(Text=result[[5]]$text, Score=qq5)
    
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Extraversion=table1$value, Agreeableness=table2$value, Openness=table3$value, Conscientiousness=table3$value, Neuroticism=table5$value)
  }
  
  percentage<-function(table_final)
  {
    #Positive Percentage
    
    #Renaming
    extraSc=table_final$Extraversion
    agreeSc=table_final$Agreeableness
    openSc=table_final$Openness
    consSc=table_final$Conscientiousness
    neuroSc=table_final$Neuroticism
    
    
    #Adding column
    table_final$ExtPercent = extraSc/ (extraSc+agreeSc+openSc+consSc+neuroSc) *100
    
    #Replacing Nan with zero
    ee= table_final$ExtPercent
    ee[is.nan(ee)] <- 0
    table_final$ExtPercent = ee
    
    
    
    #Adding column
    table_final$AgrPercent = agreeSc/ (extraSc+agreeSc+openSc+consSc+neuroSc) *100
    
    #Replacing Nan with zero
    aa = table_final$AgrPercent
    aa[is.nan(aa)] <- 0
    table_final$AgrPercent = aa
    
    #Adding column
    table_final$OpnPercent = openSc/ (extraSc+agreeSc+openSc+consSc+neuroSc) *100
    
    #Replacing Nan with zero
    oo = table_final$OpnPercent
    oo[is.nan(oo)] <- 0
    table_final$OpnPercent = oo
    
    #Adding column
    table_final$ConPercent = consSc/ (agreeSc+extraSc+consSc+openSc+neuroSc) *100
    
    #Replacing Nan with zero
    cc = table_final$ConPercent
    cc[is.nan(cc)] <- 0
    table_final$ConPercent = cc
    
    #Adding column
    table_final$NuroPercent = neuroSc/ (agreeSc+extraSc+openSc+consSc+neuroSc) *100
    
    #Replacing Nan with zero
    nn = table_final$NuroPercent
    nn[is.nan(nn)] <- 0
    table_final$NuroPercent = nn
    
    
    return(table_final)
  }
  
  wordDatabase()
  
  twtList<-reactive({twtList<-searchTwitter(input$searchTerm, n=input$maxTweets, lang="en") })
  tweets<-reactive({tweets<-TweetFrame(twtList() )})
  
  result<-reactive({result<-score.sentiment(tweets(), extra.words, open.words, agree.words, cons.words, neuro.words, .progress='none')})
  
  table_final<-reactive({table_final<-sentimentAnalyser(  result() )})
  table_final_percentage<-reactive({table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())	
  
  #WORDCLOUD
  wordclouds<-function(text)
  {
    library(tm)
    library(wordcloud)
    corpus <- Corpus(VectorSource(text))
    #clean text
    clean_text <- tm_map(corpus, removePunctuation)
    #clean_text <- tm_map(clean_text, content_transformation)
    clean_text <- tm_map(clean_text, content_transformer(tolower))
    clean_text <- tm_map(clean_text, removeWords, stopwords("english"))
    clean_text <- tm_map(clean_text, removeNumbers)
    clean_text <- tm_map(clean_text, stripWhitespace)
    return (clean_text)
  }
  text_word<-reactive({text_word<-wordclouds( tweets() )})
  
  output$word <- renderPlot({ wordcloud(text_word(),random.order=F,max.words=80, col=rainbow(100), scale=c(4.5, 1)) })
  
  #HISTOGRAM
  output$histExtra<- renderPlot({ hist(table_final()$Extraversion, col=rainbow(10), main="Histogram of Extraversion", xlab = "Extraversion Score") })
  output$histAgree<- renderPlot({ hist(table_final()$Agreeableness, col=rainbow(10), main="Histogram of Agreeableness Sentiment", xlab = "Agreeableness Score") })
  output$histOpen<- renderPlot({ hist(table_final()$Openness, col=rainbow(10), main="Histogram of Openness Sentiment", xlab = "Openness Score") })
  output$histCons<- renderPlot({ hist(table_final()$Conscientiousness, col=rainbow(10), main="Histogram of Conscientiousness Sentiment", xlab = "Conscientiousness Score") })
  output$histNeuro<- renderPlot({ hist(table_final()$Neuroticism, col=rainbow(10), main="Histogram of Neuroticism Sentiment", xlab = "Neuroticism Score") })  
    
  
  #Pie
  slices <- reactive ({ slices <- c(sum(table_final()$Extraversion), sum(table_final()$Agreeableness), sum(table_final()$Openness), sum(table_final()$Conscientiousness), sum(table_final()$Neuroticism)) })
  labels <- c("Extraversion", "Agreeableness", "Openness", "Conscientiousness", "Neuroticism")
  library(plotrix)
  output$piechart <- renderPlot({ pie3D(slices(), labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis") })

  piepercent <- reactive({piepercent<- round(100*slices()/sum(slices()), 1)})
  
  output$piechar <- renderPlot({ pie(slices(), labels = piepercent(), main = "Sentiment Analysis",col = rainbow(length(slices())))
    legend("topleft", c("Ext","Agree","Open","Cons","Neuro"), cex = 1, fill = rainbow(length(slices()))) })
  
  #BAr Plot
  
  # Plot the bar chart
  colors <- c("blue", "red","green", "yellow", "orange")
  slicess <- reactive({ slicess <- c(sum(table_final()$Extraversion), sum(table_final()$Agreeableness), sum(table_final()$Openness), sum(table_final()$Conscientiousness), sum(table_final()$Neuroticism))})
  labelss <- c("Extraversion", "Agreeableness", "Openness", "Conscientiousness", "Neuroticism")
  output$barplot <- renderPlot({ barplot(slicess(),names.arg = labelss,xlab = "Sentiment",ylab = "Score",col = colors,
                                         main = "Sentiment Analysis",border = "red") })
  
  #Top trending tweets
  toptrends <- function(place)
  {
    a_trends = availableTrendLocations()
    woeid = a_trends[which(a_trends$name==place),3]
    trend = getTrends(woeid)
    trends = trend[1:2]
    
    dat <- cbind(trends$name)
    dat2 <- unlist(strsplit(dat, split=", "))
    dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
    dat4 <- dat2[-dat3]
    return (dat4)
  }
  
  trend_table<-reactive({ trend_table<-toptrends(input$trendingTable) })
  output$trendtable <- renderTable(trend_table())
  
  #TOP TWEETERS
  
  # Top tweeters for a particular hashtag (Barplot)
  toptweeters<-function(tweetlist)
  {
    tweets <- twListToDF(tweetlist)
    tweets <- unique(tweets)
    # Make a table of the number of tweets per user
    d <- as.data.frame(table(tweets$screenName)) 
    d <- d[order(d$Freq, decreasing=T), ] #descending order of tweeters according to frequency of tweets
    names(d) <- c("User","Tweets")
    return (d)
  }
  
  # Plot the table above for the top 20
  
  d<-reactive({d<-toptweeters(  twtList() ) })
  output$tweetersplot<-renderPlot ( barplot(head(d()$Tweets, 20), names=head(d()$User, 20), horiz=F, las=2, main="Top Tweeters", col=1) )
  output$tweeterstable<-renderTable(head(d(),20))
  
  #TOP 10 HASHTAGS OF USER
  
  tw1 <- reactive({ tw1 = userTimeline(input$user, n = 3200) })
  tw <- reactive({ tw = twListToDF(tw1()) })
  vec1<-reactive ({ vec1 = tw()$text })
  
  extract.hashes = function(vec){
    
    hash.pattern = "#[[:alpha:]]+"
    have.hash = grep(x = vec, pattern = hash.pattern)
    
    hash.matches = gregexpr(pattern = hash.pattern,
                            text = vec[have.hash])
    extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
    
    df = data.frame(table(tolower(unlist(extracted.hash))))
    colnames(df) = c("tag","freq")
    df = df[order(df$freq,decreasing = TRUE),]
    return(df)
  }
  
  dat<-reactive({ dat = head(extract.hashes(vec1()),50) })
  dat2<- reactive ({ dat2 = transform(dat(),tag = reorder(tag,freq)) })
  
  p<- reactive ({ p = ggplot(dat2(), aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
  p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the tweeter") })
  output$tophashtagsplot <- renderPlot ({ p() })	
  })
#shiny server