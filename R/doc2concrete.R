utils::globalVariables(c("mturk_list","bootstrap_list","adviceModel","adviceNgrams","planModel","planNgrams")) # prevent incorrect "no global binding" note

#' Concreteness Scores
#'
#' @description Detects linguistic markers of concreteness in natural language.
#'     This function is the workhorse of the \code{doc2concrete} package, taking a vector of text documents and returning an equal-length vector of concreteness scores.
#' @param texts character A vector of texts, each of which will be tallied for concreteness.
#' @param domain character Indicates the domain from which the text data was collected (see details).
#' @param wordlist Dictionary to be used. Default is the Brysbaert et al. (2014) list.
#' @param stop.words logical Should stop words be kept? default is TRUE
#' @param number.words logical Should numbers be converted to words? default is TRUE
#' @param shrink logical Should open-domain concreteness models regularize low-count words? Default is FALSE.
#' @details In principle, concreteness could be measured from any english text. However, the
#' definition and interpretation of concreteness may vary based on the domain. Here, we provide
#' a domain-specific pre-trained classifier for concreteness in advice & feedback data, which we have
#' empirically confirmed to be robust across a variety of contexts within that domain (Yeomans, 2019).
#'
#' There are many domains where such pre-training is not yet possible. Accordingly, we provide
#' support for two off-the-shelf concreteness "dictionaries" - i.e. document-level aggregations of
#' word-level scores. We found that that have modest (but consistent) accuracy across domains and contexts.
#' However, we still encourage researchers to train a model of concreteness in their own domain, if possible.
#'
#' @references
#' Yeomans, M. (2020). Concreteness, Concretely. Working Paper.
#'
#' Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. Behavior Research Methods, 46(3), 904-911.
#'
#' Paetzold, G., & Specia, L. (2016, June). Inferring psycholinguistic properties of words. In Proceedings of the 2016 Conference of the North American Chapter of the Association for Computational Linguistics: Human Language Technologies (pp. 435-440).
#'
#' @return A vector of concreteness scores, with one value for every item in `text`.
#' @examples
#'
#'\dontrun{
#'
#' data("feedback_dat")
#'
#' doc2concrete(feedback_dat$feedback, domain="open")
#'
#' hist(doc2concrete(feedback_dat$feedback, domain="open"))
#'
#' cor(doc2concrete(feedback_dat$feedback, domain="open"),feedback_dat$concrete)
#'
#'
#'
#'}
#'
#'@export

doc2concrete<-function(texts, domain=c("open","advice","plans"),
                       wordlist=NULL,
                       stop.words=TRUE, number.words=TRUE,
                       shrink=FALSE){
  texts<-iconv(texts,to="ASCII",sub=" ")
  texts[is.na(texts) | texts==""] <- "   "

  if(domain[1]=="advice"){
    testX<-as.matrix(cbind(ngramTokens(texts, ngrams=1:3, stop.words = T,
                                       vocabmatch = doc2concrete::adviceNgrams),
                           data.frame(bootC=concDict(texts=texts,
                                                     wordlist=doc2concrete::bootstrap_list,
                                                     shrink=shrink,
                                                     stop.words=stop.words,
                                                     number.words=number.words),
                                      brysC=concDict(texts=texts,
                                                     wordlist=doc2concrete::mturk_list,
                                                     shrink=shrink,
                                                     stop.words=stop.words,
                                                     number.words=number.words))))
    conc<-stats::predict(doc2concrete::adviceModel, newx = testX,
                  s="lambda.min", type="response")[,1]
  } else if (domain[1]=="plans"){
    testX<-as.matrix(cbind(ngramTokens(texts, ngrams=1:3, stop.words = T,
                                       vocabmatch = doc2concrete::planNgrams),
                           data.frame(bootC=concDict(texts=texts,
                                                     wordlist=doc2concrete::bootstrap_list,
                                                     shrink=shrink,
                                                     stop.words=stop.words,
                                                     number.words=number.words),
                                      brysC=concDict(texts=texts,
                                                     wordlist=doc2concrete::mturk_list,
                                                     shrink=shrink,
                                                     stop.words=stop.words,
                                                     number.words=number.words))))
    conc<-stats::predict(doc2concrete::planModel, newx = testX,
                  s="lambda.min", type="response")[,1]
  } else {
    conc=concDict(texts=texts,
                  wordlist=doc2concrete::mturk_list,
                  shrink=shrink,
                  stop.words=stop.words,
                  number.words=number.words)
  }
  return(conc)
}
###############################################################
