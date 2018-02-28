# Clean the tweets and returns merged data frame
result = score.sentiment(df$text, extra.words, agree.words, open.words, cons.words, neuro.words)

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