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
