#' Open-Domain Concreteness Dictionaries
#' @description background function to load
#' @param text character Vector of documents to classify
#' @param wordlist Dictionary to be used.
#' @param shrink logical should scores on shorter documents be regularized? default is TRUE#'
#' @param stop.words logical should stop words be kept? default is TRUE
#' @param number.words logical should numbers be converted to words? default is TRUE
#' @param minwords numeric all documents with less words than this return NA. default is 0 (i.e. keep all documents)
#' @return variance-weighted log odds ratio of prevalence across samples
#' @keywords internal
concDict<-function (texts, wordlist, shrink=TRUE, stop.words=TRUE, number.words=TRUE,minwords = 0){

  textstem=textstem::lemmatize_words(texts)
  ctx<-quanteda::dfm(textstem, remove=ifelse(stop.words,"",tm::stopwords()))

  concList<-parallel::mclapply(texts,word_list, wordlist=wordlist,
                               stop.words=stop.words,
                               number.words=number.words,
                               mc.cores=parallel::detectCores())

  cMeans<-unlist(lapply(concList,function(x) x$values))
  cHits<-unlist(lapply(concList,function(x) x$hits))

  conc<- dplyr::case_when(
    !is.na(cMeans) ~ cMeans,
    T ~ mean(cMeans,na.rm=T)
  )
  if(shrink){
    B=cHits/(5+cHits)
    conc=B*conc+(1-B)*mean(cMeans,na.rm=T)
  }

  conc<-as.numeric(conc)
  return(conc)
}
