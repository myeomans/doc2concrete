#' Ngram Tokenizer
#' @description Tally bag-of-words ngram features
#' @param texts character vector of texts.
#' @param wstem character Which words should be stemmed? Defaults to "all".
#' @param ngrams numeric Vector of ngram lengths to be included. Default is 1 (i.e. unigrams only).
#' @param language Language for stemming. Default is "english"
#' @param punct logical Should punctuation be kept as tokens? Default is TRUE
#' @param stop.words logical Should stop words be kept? Default is TRUE
#' @param number.words logical Should numbers be kept as words? Default is TRUE
#' @param overlap numeric Threshold (as cosine distance) for including ngrams that constitute other included phrases. Default is 1 (i.e. all ngrams included).
#' @param sparse maximum feature sparsity for inclusion (1 = include all features)
#' @param verbose logical Should the package report token counts after each ngram level? Useful for long-running code. Default is FALSE.
#' @param vocabmatch matrix Should the new token count matrix will be coerced to include the same tokens as a previous count matrix? Default is NULL (i.e. no token match).
#' @param num.mc.cores numeric number of cores for parallel processing - see parallel::detectCores(). Default is 1.
#' @details This function produces ngram featurizations of text based on the quanteda package. This provides a complement to the doc2concrete function by demonstrating
#' How to build a feature set for training a new detection algorithm in other contexts.
#'
#'
#' @return a matrix of feature counts
#' @examples
#'
#' dim(ngramTokens(feedback_dat$feedback, ngrams=1))

#' dim(ngramTokens(feedback_dat$feedback, ngrams=1:3))
#' @export
ngramTokens<-function(texts,
                      wstem="all",
                      ngrams=1,
                      language="english",
                      punct=TRUE,
                      stop.words=TRUE,
                      number.words=TRUE,
                      overlap=1,
                      sparse=0.995,
                      verbose=FALSE,
                      vocabmatch=NULL,
                      num.mc.cores=1){

  cleanertext<-unlist(parallel::mclapply(texts, cleantext, language=language,
                                         stop.words=stop.words, punct=punct,
                                         number.words=number.words,
                                         mc.cores = num.mc.cores))

  dgm<-lapply(ngrams, function(x) as.matrix(array(NA, c(length(texts),100))))
  stemtokens<-quanteda::tokens(lapply(cleanertext, stemmer,wstem=wstem,language=language))
  for (ng in 1:length(ngrams)){
    if (ng==1) {
      dgm[[ng]] <-quanteda::dfm(stemtokens)
    }else{
      dgm[[ng]] <- quanteda::dfm(quanteda::tokens_ngrams(stemtokens,ng))
    }
    if ((sparse<1)&is.null(vocabmatch)) dgm[[ng]]<-quanteda::dfm_trim(dgm[[ng]],sparsity=sparse)
    if (ng==1) dtm<-dgm[[1]]
    if ((ng>1)&(overlap<1)&(!is.null(dim(dgm[[ng]])))) dtm<-overlaps(high=dgm[[ng]],low=dtm,
                                                                     cutoff=overlap,verbose=verbose)

    if (verbose) print(paste(c(ng,"-grams ", dim(dtm)),collapse=" "))
  }
  dtm<-doublestacker(dtm)

  if(!is.null(vocabmatch)) dtm<-vocabmatcher(vocabmatch, dtm)
  return(dtm)
}
