#' Concreteness Scores for Advice
#'
#' @description Model pre-trained on advice data.
#' @param texts character A vector of texts, each of which will be tallied for concreteness.
#' @param num.mc.cores numeric number of cores for parallel processing - see parallel::detectCores(). Default is 1.
#' @return numeric Vector of concreteness ratings.
#' @keywords internal

adviceModel<-function(texts,num.mc.cores=1){
  .single=0
  if(length(texts)==1) {
    .single=1
    texts<-c(texts,"")
  }
  bootC<-concDict(texts=texts,
                  wordlist=doc2concrete::bootstrap_list,
                  shrink=FALSE,
                  stop.words=TRUE,
                  number.words=TRUE,
                  num.mc.cores=num.mc.cores)
  brysC<-concDict(texts=texts,
                  wordlist=doc2concrete::mturk_list,
                  shrink=FALSE,
                  stop.words=TRUE,
                  number.words=TRUE,
                  num.mc.cores=num.mc.cores)
  testX<-as.matrix(cbind(ngramTokens(texts, ngrams=1:3, stop.words = TRUE,sparse=1,
                                     vocabmatch = doc2concrete::adviceNgrams,
                                     num.mc.cores=num.mc.cores),
                         bootC,brysC))
  conc<-stats::predict(doc2concrete::adviceModel, newx = testX,
                       s="lambda.min", type="response")[,1]
  if(.single==1){
    conc<-conc[1]
  }
  return(conc)
}

#' Concreteness Scores for plans
#'
#' @description Model pre-trained on planning data.
#' @param texts character A vector of texts, each of which will be tallied for concreteness.
#' @param num.mc.cores numeric number of cores for parallel processing - see parallel::detectCores(). Default is 1.
#' @return numeric Vector of concreteness ratings.
#' @keywords internal
planModel<-function(texts,num.mc.cores=1){
  .single=0
  if(length(texts)==1) {
    .single=1
    texts<-c(texts,"")
  }
  bootC=concDict(texts=texts,
                 wordlist=doc2concrete::bootstrap_list,
                 shrink=FALSE,
                 stop.words=TRUE,
                 number.words=TRUE,
                 num.mc.cores=num.mc.cores)
  brysC=concDict(texts=texts,
                 wordlist=doc2concrete::mturk_list,
                 shrink=FALSE,
                 stop.words=TRUE,
                 number.words=TRUE,
                 num.mc.cores=num.mc.cores)
  testX<-as.matrix(cbind(ngramTokens(texts, ngrams=1:3,
                                     stop.words = TRUE,number.words = TRUE,
                                     sparse=1, vocabmatch = doc2concrete::planNgrams,
                                     num.mc.cores=num.mc.cores),
                         bootC,brysC))
  conc<-stats::predict(doc2concrete::planModel, newx = testX,
                       s="lambda.min", type="response")[,1]
  if(.single==1){
    conc<-conc[1]
  }
  return(conc)
}
