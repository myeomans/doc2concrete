utils::globalVariables(c("mturk_list","bootstrap_list","adviceModel","adviceNgrams","planModel","planNgrams")) # prevent incorrect "no global binding" note

#' Concreteness Scores
#'
#' @description Detects linguistic markers of concreteness in natural language.
#'     This function is the workhorse of the \code{doc2concrete} package, taking a vector of text documents and returning an equal-length vector of concreteness scores.
#' @param texts character A vector of texts, each of which will be tallied for concreteness.
#' @param domain character Indicates the domain from which the text data was collected (see details).
#' @param wordlist Dictionary to be used. Default is the Brysbaert et al. (2014) list.
#' @param stop.words logical Should stop words be kept? Default is TRUE
#' @param number.words logical Should numbers be converted to words? Default is TRUE
#' @param shrink logical Should open-domain concreteness models regularize low-count words? Default is FALSE.
#' @param fill logical Should empty cells be assigned the mean rating? Default is TRUE.
#' @param uk_english logical Does the text contain any British English spelling? Including variants (e.g. Canadian). Default is FALSE
#' @param num.mc.cores numeric number of cores for parallel processing - see parallel::detectCores(). Default is 1.
#' @details In principle, concreteness could be measured from any english text. However, the
#' definition and interpretation of concreteness may vary based on the domain. Here, we provide
#' a domain-specific pre-trained classifier for concreteness in advice & feedback data, which we have
#' empirically confirmed to be robust across a variety of contexts within that domain (Yeomans, 2021).
#'
#' The training data for the advice classifier includes both second-person (e.g. "You should") and
#' third-person (e.g. "She should") framing, including some names (e.g. "Riley should"). For consistency,
#' we anonymised all our training data to replace any names with "Riley". If you are working with a
#' dataset that includes the names of advice recipients, we recommend you convert all those names to
#' "Riley" as well, to ensure optimal performance of the algorithm (and to respect their privacy).
#'
#' There are many domains where such pre-training is not yet possible. Accordingly, we provide
#' support for two off-the-shelf concreteness "dictionaries" - i.e. document-level aggregations of
#' word-level scores. We found that that have modest (but consistent) accuracy across domains and contexts.
#' However, we still encourage researchers to train a model of concreteness in their own domain, if possible.
#'
#' @references
#' Yeomans, M. (2021). A Concrete Application of Open Science for Natural Language Processing. Organizational Behavior and Human Decision Processes, 162, 81-94.
#'
#' Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. Behavior Research Methods, 46(3), 904-911.
#'
#' Paetzold, G., & Specia, L. (2016, June). Inferring psycholinguistic properties of words. In Proceedings of the 2016 Conference of the North American Chapter of the Association for Computational Linguistics: Human Language Technologies (pp. 435-440).
#'
#' @return A vector of concreteness scores, with one value for every item in `text`.
#' @examples
#'
#'
#' data("feedback_dat")
#'
#' doc2concrete(feedback_dat$feedback, domain="open")
#'
#'
#' cor(doc2concrete(feedback_dat$feedback, domain="open"),feedback_dat$concrete)
#'
#'
#'@import glmnet
#'@export

doc2concrete<-function(texts,
                       domain=c("open","advice","plans"),
                       wordlist=doc2concrete::mturk_list,
                       stop.words=TRUE,
                       number.words=TRUE,
                       shrink=FALSE,
                       fill=FALSE,
                       uk_english=FALSE,
                       num.mc.cores=1){

  texts<-iconv(cleanpunct(texts),to="ASCII",sub=" ")
  texts[is.na(texts) | stringr::str_count(texts, "[[:alpha:]]+")==0] <- " .  "
  if(uk_english){
    texts<-usWords(texts)
  }
  if(length(texts)<4000){
    if(domain[1]=="advice"){
      conc<-adviceModel(texts=texts,
                        num.mc.cores=num.mc.cores)
    } else if (domain[1]=="plans"){
      conc<-planModel(texts=texts,
                      num.mc.cores=num.mc.cores)
    }else {
      conc<-concDict(texts=texts,
                     wordlist=wordlist,
                     shrink=shrink,
                     fill=fill,
                     stop.words=stop.words,
                     number.words=number.words,
                     num.mc.cores=num.mc.cores)
    }
  } else{
    # Batched loop to minimize memory load
    textList<-split(texts, ceiling(seq_along(texts)/2000))
    concList<-lapply(1:length(textList),function(x) NA)
    tpb<-utils::txtProgressBar(0,length(textList))
    for (x in 1:length(textList)){
      if(domain[1]=="advice"){
        concList[[x]]<-adviceModel(texts=textList[[x]],
                                   num.mc.cores=num.mc.cores)
      } else if (domain[1]=="plans"){
        concList[[x]]<-planModel(texts=textList[[x]],
                                 num.mc.cores=num.mc.cores)
      } else {
        concList[[x]]<-concDict(texts=textList[[x]],
                                wordlist=wordlist,
                                shrink=shrink,
                                fill=fill,
                                stop.words=stop.words,
                                number.words=number.words,
                                num.mc.cores=num.mc.cores)
      }
      utils::setTxtProgressBar(tpb,x)
    }
    conc<-do.call(c,concList)
  }
  return(conc)
}
###############################################################
