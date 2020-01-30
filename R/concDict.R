#' Open-Domain Concreteness Dictionaries
#' @description background function to load
#' @param text character Vector of documents to classify
#' @param wordlist Dictionary to be used.
#' @param shrink logical should scores on shorter documents be regularized? default is FALSE
#' @param stop.words logical should stop words be kept? default is TRUE
#' @param number.words logical should numbers be converted to words? default is TRUE
#' @param minwords numeric all documents with less words than this return NA. default is 0 (i.e. keep all documents)
#' @param num.mc.cores numeric number of cores for parallel processing - see parallel::detectCores()
#' @return variance-weighted log odds ratio of prevalence across samples
#' @keywords internal
concDict<-function (texts, wordlist=NULL, shrink=FALSE,
                    stop.words=TRUE, number.words=TRUE,
                    minwords = 0, num.mc.cores=1){
  if(is.null(wordlist)){
    wordlist <- doc2concrete::mturk_list
  }
  textstem=textstem::lemmatize_words(texts)
  ctx<-quanteda::dfm(textstem, remove=ifelse(stop.words,"",tm::stopwords()))

  concList<-parallel::mclapply(texts,word_list, wordlist=wordlist,
                               stop.words=stop.words,
                               number.words=number.words,
                               mc.cores=1)

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
