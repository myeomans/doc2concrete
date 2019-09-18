utils::globalVariables(c("mturk_list")) # prevent incorrect "no global binding" note

#' Concreteness Scores
#'
#' @description Detects linguistic markers of politeness in natural language.
#'     This function is the workhorse of the \code{doc2concrete} package, taking an N-length vector of text documents and returning an N-length vector of concreteness scores.
#' @param texts character A vector of texts, each of which will be tallied for concreteness.
#' @param domain character Indicates the domain from wihch the text data was collected (see details).
#' @param stop.words logical should stop words be kept? default is TRUE
#' @param number.words logical should numbers be converted to words? default is TRUE
#' @param shrink logical Should open-domain concreteness models regularize low-count words? Default is TRUE.
#' @param length logical Should open-domain concreteness models treat document length as a feature of concreteness? Default is TRUE
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
#' Yeomans, M. (2019). Concreteness, Concretely. Working Paper.
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

doc2concrete<-function(texts, domain=c("open","advice"),
                       stop.words=TRUE, number.words=TRUE,
                       shrink=TRUE, length=TRUE){
  texts<-iconv(texts,to="ASCII",sub=" ")
  texts[is.na(texts) | texts==""] <- "   "

  if(domain[1]=="advice"){
    stop("functionality in progress...")
  } else {
    textstem=textstem::lemmatize_words(texts)
    ctx<-quanteda::dfm(textstem, remove=ifelse(stop.words,"",tm::stopwords()))

    concList<-parallel::mclapply(texts,word_list, wordlist=mturk_list,
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
    conctable=data.frame(words=stringr::str_count(texts,"[[:alpha:]]+"),
                         hits=cHits,
                         concrete=conc)
    c.scores=conctable$concrete
  }
  return(c.scores)
}
###############################################################
