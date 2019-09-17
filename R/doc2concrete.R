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
#' @details
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

doc2concrete<-function(texts, domain=c("advice","open"),
                       stop.words=TRUE, number.words=TRUE,
                       shrink=TRUE, length=TRUE){
  texts<-iconv(texts,to="ASCII",sub=" ")
  texts[is.na(texts) | texts==""] <- "   "

  if(domain[1]=="advice"){

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

    if(length(conc)>1) conc<-as.numeric(scale(conc))
    conctable=data.frame(words=stringr::str_count(texts,"[[:alpha:]]+"),
                         hits=cHits,
                         concrete=conc)
    c.scores=conctable$concrete
  }
  return(c.scores)
}
###############################################################
