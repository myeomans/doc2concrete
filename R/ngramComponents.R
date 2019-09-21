############################################################################
# Underlying functions for Tokenizer
############################################################################
#' Text Cleaner
#' @description background function to load.
#' @param text character Vector of strings to clean.
#' @param language character Language to use for cleaning. Default is "english".
#' @param punct logical Should punctuation be kept as tokens? Default is TRUE.
#' @param stop.words logical Should stop words be kept? default is TRUE.
#' @param number.words logical Should numbers be converted to words? default is TRUE.
#' @return character Vector of cleaned strings.
#' @keywords internal
cleantext<-function(text, language="english", punct=FALSE,
                    stop.words=TRUE, number.words=TRUE){
  #PUTS ALL LETTERS IN LOWER CASE
  text<-tolower(text)
  text<-textformat(text, punct)
  #EXPANDS CONTRACTIONS
  if(language=="english"){
    text<-ctxpand(text)
  }
  #DELETES PUNCTUATION & HTML JUNK
  text<-gsub("[[:punct:]]", " ", text)
  text<-gsub("[[:cntrl:]]", " ", text)
  #DELETES STOP WORDS
  if(length(stop.words)>1){
    text<-tm::removeWords(text, stop.words)
  }else if(!stop.words){
    text<-tm::removeWords(text, tm::stopwords(language))
  }
  if(number.words){
    text<-strsplit(text," ")[[1]]
    nx<-suppressWarnings(as.numeric(text))

    text[!is.na(nx)]<-as.character(english::as.english(nx[!is.na(nx)]))
    text<-paste(text,collapse=" ")
  } else {  #DELETES NUMBERS
    text<-tm::removeNumbers(text)
  }

  text<-tm::stripWhitespace(text)
  return(as.character(text))
}

############################################################################
#' Text Formatter
#' @description background function to load.
#' @param text character Vector of strings to clean.
#' @param punct logical Should punctuation be kept as tokens? Default is TRUE.
#' @return character Vector of cleaned strings.
#' @keywords internal
textformat<-function(text, punct=FALSE){
  text <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", text)
  text <- gsub("www.(.*)[.][a-z]+", "", text)
  text <- gsub("\u201D", "\"", text)
  text <- gsub("\u201C", "\"", text)
  text <- gsub("\u2019", "\'", text)

  text<-gsub("ha ha"," haha ",text,fixed=T)
  text<-gsub("lol "," haha ",text,fixed=T)
  text<-gsub("lol."," haha.",text,fixed=T)
  text<-gsub("lol!"," haha!",text,fixed=T)
  text<-gsub("Lol "," haha ",text,fixed=T)
  text<-gsub("Lol."," haha.",text,fixed=T)
  text<-gsub("Lol!"," haha!",text,fixed=T)
  text<-gsub("LOL"," haha ",text,fixed=T)
  text<-gsub("LOl"," haha ",text,fixed=T)
  text<-gsub("LOl"," haha ",text,fixed=T)
  text<-gsub("LoL"," haha ",text,fixed=T)
  text<-gsub("ROFL"," haha ",text,fixed=T)
  text<-gsub("rofl"," haha ",text,fixed=T)
  for (x in 1:8){
    text<-gsub(".?","?",text,fixed=T)
    text<-gsub("?.","?",text,fixed=T)
    text<-gsub("!?","?",text,fixed=T)
    text<-gsub("?!","?",text,fixed=T)
    text<-gsub("??","?",text,fixed=T)
    text<-gsub("!!","!",text,fixed=T)
  }
  if(punct){
    text<-gsub("!"," xmark.",text,fixed=T)
    text<-gsub("?"," qmark.",text,fixed=T)
  }
  text<-gsub("||",". ",text,fixed=T)
  text<-gsub("|",". ",text,fixed=T)
  text<-gsub("[[:cntrl:]]", " ", text)
  return(text)
}

############################################################################
#' Contraction Expander
#' @description background function to load.
#' @param text character vector of sentences to un-contract.
#' @return character Vector of sentences without contractions.
#' @keywords internal
ctxpand<-function(text){
  text <- gsub("let's", "let us", text, fixed=T)
  text <- gsub("i'm", "i am", text, fixed=T)
  text <- gsub("won't", "will not", text, fixed=T)
  text <- gsub("can't", "cannot", text, fixed=T)
  text <- gsub("Let's", "Let us", text, fixed=T)
  text <- gsub("I'm", "I am", text, fixed=T)
  text <- gsub("Won't", "Will not", text, fixed=T)
  text <- gsub("Can't", "Cannot", text, fixed=T)
  text <- gsub("shan't", "shall not", text, fixed=T)
  text <- gsub("'d", " would", text, fixed=T)
  text <- gsub("'ve", " have", text, fixed=T)
  text <- gsub("'s", " is", text, fixed=T)
  text <- gsub("'ll", " will", text, fixed=T)
  text <- gsub("'re", " are", text, fixed=T)
  text <- gsub("n't", " not", text, fixed=T)
  text <- gsub("u.s.", "US", text, fixed=T)
  text <- gsub("U.S.", "US", text, fixed=T)
  text <- gsub("e.g.", "eg", text, fixed=T)
  text <- gsub("i.e.", "ie", text, fixed=T)
  return(text)
}

############################################################################
#' Stemmer
#' @description background function to load.
#' @param text character vector of strings to clean.
#' @param wstem character Which words should be stemmed? Defaults to "all".
#' @param ngrams numeric Vector of ngram lengths to be included. Default is 1 (i.e. unigrams only).
#' @param language Language for stemming. Default is "english".
#' @return Sentence of stemmed words.
#' @keywords internal

gramstem<-function(text, wstem="all", ngrams=1, language="english"){
  if(nchar(text)%in%c(NA,NULL,0:2)){
    return(text)
  }else{
    xes<-(strsplit(text, split=" ")[[1]])
    xes<-xes[which(nchar(xes)>0)]
    if(length(wstem)>1) xes<-sapply(xes, function(x) stemexcept(x, wstem, language), USE.NAMES=F)
    if(wstem=="all") xes<-sapply(xes, SnowballC::wordStem, language=language, USE.NAMES=F)
    xret<-" "
    if (1 %in% ngrams) xret<-paste(c(xret, xes), collapse=" ")
    if (2 %in% ngrams) xret<-paste(c(xret, ngrammer(xes, 2)), collapse=" ")
    if (3 %in% ngrams) xret<-paste(c(xret, ngrammer(xes, 3)), collapse=" ")
    return(xret)
  }
}

############################################################################
#' Phraser
#' @description background function to load
#' @param onewords character Vector of words to combine into phrases.
#' @param ngram numeric Length of phrases to create.
#' @return Sentence of stemmed ngrams.
#' @keywords internal
ngrammer <- function (onewords, ngram){
  len<-length(onewords)
  if(len<ngram){
    return(" ")
  }else{
    if (ngram==1){
      words<-onewords
    }
    if (ngram==2){
      twowords<-cbind(onewords[1:(len-1)], onewords[2:len])
      words<-apply(twowords, 1, function(x) paste0(x, collapse="_"))
    }
    if (ngram==3){
      threewords<-cbind(onewords[1:(len-2)], onewords[2:(len-1)], onewords[3:len])
      words<-apply(threewords, 1, function(x) paste0(x, collapse="_"))
    }
    return(paste(words, collapse=" "))
  }
}

############################################################################
#' Conditional Stemmer
#' @description background function to load
#' @param sentence character Vector of sentences to stem.
#' @param excepts character Vector of words that should not be stemmed.
#' @param language Language for stemming. Default is "english".
#' @return Sentence of stemmed words.
#' @keywords internal
stemexcept<-function(sentence, excepts, language="english"){
  words<-strsplit(sentence, split=" ")[[1]]
  SS<-which(!(words %in% excepts))
  words[SS]<-SnowballC::wordStem(words[SS], language)
  return(paste(words, collapse=" "))
}
############################################################################
#' Overlap cleaner
#' @description background function to load
#' @param high matrix Token counts that will all be kept.
#' @param low matrix Token counts that will evaluated (and pruned) for overlapping.
#' @param cutoff numeric Threshold (as cosine distance) for including overlapping tokens. Default is 1 (i.e. all tokens included).
#' @return Combined token count matrix.
#' @keywords internal
overlaps<-function(high, low, cutoff=1){
  if(cutoff==1){
    combined<-cbind(as.matrix(high),as.matrix(low))
  } else {
    high<-as.matrix(high)
    low_l<-data.frame(lapply(colnames(low),function(x) as.vector(low[,x])))
    colnames(low_l)<-colnames(low)
    peaks<-apply(low_l, 2, function(x) max(apply(high, 2, function(y) x %*% y / sqrt(x%*%x * y%*%y))))
    remaining<-low_l[,peaks<=cutoff]
    combined<-cbind(remaining,high)
  }
  return(combined)
}

############################################################################
#' Doublestacker
#' @description background function to load
#' @param wdcts matrix Token counts that will have doubled column names condensed.
#' @return Token count matrix with no doubled column names.
#' @keywords internal
doublestacker<-function (wdcts){
  wdcts<-as.matrix(wdcts)
  words<- colnames(wdcts)
  for (Q in words[duplicated(words)]) {
    wdcts[, (words== Q) & (!duplicated(words))] <- as.numeric(rowSums(wdcts[,(words== Q)]))
    wdcts[, ((words== Q) & (duplicated(words)))] <- NA
  }
  return(wdcts[, !is.na(colMeans(wdcts))])
}
############################################################################

############################################################################
#' Feature Count Matcher
#' @description background function to load
#' @param hole matrix Token counts in model data.
#' @param peg matrix Token counts in new data.
#' @return Token counts matrix from new data, with column names that match the model data.
#' @keywords internal
vocabmatch<-function(hole, peg){
  peg<-doublestacker(peg)
  newpeg<-array(0, c(nrow(peg), ncol(hole)))
  for (i in 1:ncol(newpeg)){
    if(colnames(hole)[i] %in% colnames(peg)){
      newpeg[,i]<-peg[,which(colnames(peg)==colnames(hole)[i])]
    }
  }
  dimnames(newpeg)<-list(rownames(peg), colnames(hole))
  return(as.matrix(newpeg))
}
############################################################################
