score.sentiment = function(sentences, extra.words, agree.words,open.words,cons.words,neuro.words, .progress='none')
{
  require(plyr)                  # extra agree open cons neuro
  require(stringr)
  
  
  list=lapply(sentences, function(sentence, agree.words, extra.words, cons.words, open.words, neuro.words)
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