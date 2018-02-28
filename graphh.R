#Histogram
hist(table_final$Extraversion, col=rainbow(10))
hist(table_final$Agreeableness, col=rainbow(10))
hist(table_final$Conscientiousness, col=rainbow(10))
hist(table_final$Openness, col=rainbow(10))
hist(table_final$Neuroticism, col=rainbow(10))

library(plotrix)
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")



#Pie
slices <- c(sum(table_final$Extraversion), sum(table_final$Agreeableness), sum(table_final$Openness), sum(table_final$Conscientiousness), sum(table_final$Neuroticism))
labels <- c("Extraversion", "Agreeableness", "Openness", "Conscientiousness", "Neuroticism")
library(plotrix)
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")

slices <- c(sum(table_final$Extraversion), sum(table_final$Agreeableness), sum(table_final$Openness), sum(table_final$Conscientiousness), sum(table_final$Neuroticism))
piepercent<- round(100*slices/sum(slices), 1)

pie3D(slices, labels = piepercent, main = "Sentiment Analysis",col = rainbow(length(slices)))
legend("left", c("Extraversion","Agreeableness","Openness","Conscientiousness","Neuroticism"), cex = 0.8,
       fill = rainbow(length(slices)))
colors <- c("blue", "red","black","yellow","green")

# Plot the bar chart.
barplot(slices,names.arg = labels,xlab = "Sentiment",ylab = "Score",col = colors,
        main = "Sentiment Analysis",border = "red")