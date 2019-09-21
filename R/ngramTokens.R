#' Ngram Tokenizer
#' @description Tally bag-of-words ngram features
#' @param texts character vector of texts.
#' @param wstem character Which words should be stemmed? Defaults to "all".
#' @param ngrams numeric Vector of ngram lengths to be included. Default is 1 (i.e. unigrams only).
#' @param language Language for stemming. Default is "english"
#' @param punct logical Should punctuation be kept as tokens? Default is TRUE
#' @param stop.words logical Should stop words be kept? Default is TRUE
#' @param overlap numeric Threshold (as cosine distance) for including ngrams that constitute other included phrases. Default is 1 (i.e. all ngrams included).
#' @param sparse numeric Threshold (as percent sparsity) for keeping rare tokens. Default is .99 (i.e. keep all ngrams that appear in at least 1/100 texts).
#' @param verbose logical Should the package report token counts after each ngram level? Useful for long-running code. Default is FALSE.
#' @param vocabmatch matrix Should the new token count matrix will be coerced to include the same tokens as a previous count matrix? Default is NULL (i.e. no token match).
#' @return a matrix of feature counts
#' @keywords internal
ngramTokens<-function(texts,
                      wstem="all",
                      ngrams=1,
                      language="english",
                      punct=TRUE,
                      stop.words=TRUE,
                      overlap=1,
                      sparse=0.99,
                      verbose=FALSE,
                      vocabmatch=NULL){
  if(length(texts)<2) {
    stop("must include multiple texts")
  }

  cleanertext<-unlist(parallel::mclapply(texts, cleantext, language, stop.words, punct,
                                         mc.cores = parallel::detectCores()))

  dgm<-lapply(ngrams, function(x) as.matrix(array(NA, c(length(texts),100))))
  token.list<-list()
  for (ng in 1:length(ngrams)){
    tokens<-unlist(parallel::mclapply(cleanertext, gramstem, wstem=wstem, ngrams=ngrams[ng], language=language,
                                      mc.cores= parallel::detectCores()))
    dgm[[ng]] <- as.matrix(quanteda::dfm(tokens))
    dgm[[ng]]<-dgm[[ng]][,colSums(dgm[[ng]])>floor(length(texts)/200)]
    if ((sparse<1)) dgm[[ng]]<-dgm[[ng]][,colMeans(dgm[[ng]]>0)>=(1-sparse)]
    if (ng==1) dtm<-dgm[[1]]
    if ((ng>1)&(!is.null(dim(dgm[[ng]])))) dtm<-overlaps(dtm, dgm[[ng]], overlap)

    if (verbose) print(paste(c(ng, dim(dtm),dim(dgm[[ng]]))))
  }
  dtm<-doublestacker(dtm)

  if(!is.null(vocabmatch)) dtm<-vocabmatch(vocabmatch, dtm)
  return(dtm)
}
