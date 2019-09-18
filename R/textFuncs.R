#' Word List Counter
#' @description background function to load
#' @param text character string (i.e. document to classify)
#' @param wordlist dictionary to be used
#' @param stop.words logical should stop words be kept? default is TRUE
#' @param number.words logical should numbers be converted to words? default is TRUE
#' @param minwords numeric all documents with less words than this return NA. default is 0 (i.e. keep all documents)
#' @return variance-weighted log odds ratio of prevalence across samples
#' @keywords internal
word_list<-function (text, wordlist=NULL, stop.words=TRUE, number.words=TRUE,minwords = 0){
  if(is.null(wordlist)){
    stop("must supply dictionary")
  }
  words <- strsplit(cleantext(text, stop.words=stop.words, number.words=number.words), " ")[[1]]
  words<-ifelse(words%in%wordlist$Word,words,textstem::lemmatize_words(words))
  words<-words[words%in%wordlist$Word]

  scores <- unlist(wordlist[match(words, wordlist$Word), 2])
  values<-ifelse(length(scores)>minwords,
                 mean(scores),NA)
  return(list(values=values,
              allscores=scores,
              hits=length(scores)))
}

#' Text Counter
#' @description background function to load
#' @param counted vector of character strings to count
#' @param texts vector of character strings (i.e. documents to classify)
#' @param fixed should regular expressions be ignored? default is TRUE
#' @return vector with same length as texts, counting all instances of counted
#' @keywords internal
textcounter<-function (counted, texts, fixed=TRUE) {
  counts<-rep(0,length(texts))
  for (x in counted){
    counts<-counts+sapply(gregexpr(x, texts, fixed = fixed), function(z) ifelse(z[1] == (-1), 0, length(z)))
  }
  return(counts)
}

#' Text Cleaner
#' @description background function to load
#' @param text character vector of strings to clean
#' @param language character language to use for cleaning (default is english)
#' @param stop.words logical should stop words be kept? default is TRUE
#' @param number.words logical should numbers be converted to words? default is TRUE
#' @keywords internal
cleantext<-function(text, language="english", stop.words=TRUE, number.words=TRUE){
  #PUTS ALL LETTERS IN LOWER CASE
  text<-tolower(text)
  #EXPANDS CONTRACTIONS
  if(language=="english"){
    text<-ctxpand(text)
  }
  #DELETES PUNCTUATION & HTML JUNK
  text<-gsub("[[:punct:]]", " ", text)
  text<-gsub("[[:cntrl:]]", " ", text)
  #DELETES STOP WORDS
  if(length(stop.words)>1){
    text<-tm::removeWords(text, stop.words)
  }else if(!stop.words){
    text<-tm::removeWords(text, tm::stopwords(language))
  }
  if(number.words){
    text<-strsplit(text," ")[[1]]
    nx<-suppressWarnings(as.numeric(text))

    text[!is.na(nx)]<-as.character(english::as.english(nx[!is.na(nx)]))
    text<-paste(text,collapse=" ")
  } else {  #DELETES NUMBERS
    text<-tm::removeNumbers(text)
  }

  text<-tm::stripWhitespace(text)
  return(as.character(text))
}

#' Contraction Expander
#' @description background function to load
#' @param text character vector of strings to un-contract
#' @keywords internal
ctxpand<-function(text){
  text<-sapply(text, function(x) gsub("let's", "let us", x, fixed=T))
  text<-sapply(text, function(x) gsub("i'm", "i am", x, fixed=T))
  text<-sapply(text, function(x) gsub("won't", "will not", x, fixed=T))
  text<-sapply(text, function(x) gsub("can't", "cannot", x, fixed=T))
  text<-sapply(text, function(x) gsub("shan't", "shall not", x, fixed=T))
  text<-sapply(text, function(x) gsub("'d", " would", x, fixed=T))
  text<-sapply(text, function(x) gsub("'ve", " have", x, fixed=T))
  text<-sapply(text, function(x) gsub("'s", " is", x, fixed=T))
  text<-sapply(text, function(x) gsub("'ll", " will", x, fixed=T))
  text<-sapply(text, function(x) gsub("'re", " are", x, fixed=T))
  text<-sapply(text, function(x) gsub("n't", " not", x, fixed=T))
  return(text)
}
