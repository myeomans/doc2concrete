#' Open-Domain Concreteness Dictionaries
#' @description background function to load
#' @param text character Vector of documents to classify
#' @param wordlist Dictionary to be used.
#' @param stop.words logical should stop words be kept? default is TRUE
#' @param number.words logical should numbers be converted to words? default is TRUE
#' @param shrink logical should scores on shorter documents be regularized? default is FALSE
#' @param fill logical Should empty cells be assigned the mean rating? Default is FALSE.
#' @param minwords numeric all documents with less words than this return NA. default is 0 (i.e. keep all documents)
#' @param num.mc.cores numeric number of cores for parallel processing - see parallel::detectCores()
#' @return concreteness score for each document
#' @keywords internal
concDict<-function (texts, wordlist=NULL,
                      stop.words=TRUE,
                      number.words=TRUE,
                      shrink=FALSE,
                      fill=FALSE,
                      minwords = 0,
                      num.mc.cores=1){

  if(is.null(wordlist)){
    wordlist <- doc2concrete::mturk_list
  }

  ctext<-unlist(lapply(texts, cleantext, stop.words=stop.words, number.words=number.words))

  qtd<-quanteda::dfm(quanteda::tokens(ctext),tolower=TRUE)
  wordlist$Word<-tolower(wordlist$Word)

  qNames<-colnames(qtd)
  qGap<-!(qNames%in%wordlist$Word)
  qNames[qGap]<-textstem::lemmatize_words(qNames[qGap])

  sSet<-wordlist$Conc.M[match(qNames, wordlist$Word)]

  sProd<-as.matrix(apply(qtd, 1, function(x) x*sSet))

  cHits<-rowSums(as.matrix(qtd))

  conc<-ifelse(cHits>minwords,
               colSums(sProd,na.rm=TRUE)/cHits,NA)

  if(fill & sum(is.na(conc))<length(conc)){
    conc[is.na(conc)]<-mean(conc,na.rm=TRUE)
  }
  if(shrink){
    B=cHits/(5+cHits)
    conc=B*conc+(1-B)*mean(conc,na.rm=TRUE)
  }

  conc<-as.numeric(conc)
  return(conc)
}
