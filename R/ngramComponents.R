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
  text<-gsub("[[:punct:]]", " ", text,perl=TRUE)
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
  text <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", text,perl=TRUE)
  text <- gsub("www.(.*)[.][a-z]+", "", text,perl=TRUE)
  text <- gsub("\u201D", "\"", text,perl=TRUE)
  text <- gsub("\u201C", "\"", text,perl=TRUE)
  text <- gsub("\u2019", "\'", text,perl=TRUE)

  text<-gsub("ha ha"," haha ",text,fixed=TRUE)
  text<-gsub("lol "," haha ",text,fixed=TRUE)
  text<-gsub("lol."," haha.",text,fixed=TRUE)
  text<-gsub("lol!"," haha!",text,fixed=TRUE)
  text<-gsub("Lol "," haha ",text,fixed=TRUE)
  text<-gsub("Lol."," haha.",text,fixed=TRUE)
  text<-gsub("Lol!"," haha!",text,fixed=TRUE)
  text<-gsub("LOL"," haha ",text,fixed=TRUE)
  text<-gsub("LOl"," haha ",text,fixed=TRUE)
  text<-gsub("LOl"," haha ",text,fixed=TRUE)
  text<-gsub("LoL"," haha ",text,fixed=TRUE)
  text<-gsub("ROFL"," haha ",text,fixed=TRUE)
  text<-gsub("rofl"," haha ",text,fixed=TRUE)
  for (x in 1:8){
    text<-gsub(".?","?",text,fixed=TRUE)
    text<-gsub("?.","?",text,fixed=TRUE)
    text<-gsub("!?","?",text,fixed=TRUE)
    text<-gsub("?!","?",text,fixed=TRUE)
    text<-gsub("??","?",text,fixed=TRUE)
    text<-gsub("!!","!",text,fixed=TRUE)
  }
  if(punct){
    text<-gsub("!"," xmark.",text,fixed=TRUE)
    text<-gsub("?"," qmark.",text,fixed=TRUE)
  }
  text<-gsub("||",". ",text,fixed=TRUE)
  text<-gsub("|",". ",text,fixed=TRUE)
  text<-gsub("[[:cntrl:]]", " ", text,perl=TRUE)
  return(text)
}

############################################################################
#' Contraction Expander
#' @description background function to load.
#' @param text character vector of sentences to un-contract.
#' @return character Vector of sentences without contractions.
#' @keywords internal
ctxpand<-function(text){
  text <- gsub("let's", "let us", text, fixed=TRUE)
  text <- gsub("i'm", "i am", text, fixed=TRUE)
  text <- gsub("won't", "will not", text, fixed=TRUE)
  text <- gsub("can't", "cannot", text, fixed=TRUE)
  text <- gsub("Let's", "Let us", text, fixed=TRUE)
  text <- gsub("I'm", "I am", text, fixed=TRUE)
  text <- gsub("Won't", "Will not", text, fixed=TRUE)
  text <- gsub("Can't", "Cannot", text, fixed=TRUE)
  text <- gsub("shan't", "shall not", text, fixed=TRUE)
  text <- gsub("'d", " would", text, fixed=TRUE)
  text <- gsub("'ve", " have", text, fixed=TRUE)
  text <- gsub("'s", " is", text, fixed=TRUE)
  text <- gsub("'ll", " will", text, fixed=TRUE)
  text <- gsub("'re", " are", text, fixed=TRUE)
  text <- gsub("n't", " not", text, fixed=TRUE)
  text <- gsub("u.s.", "US", text, fixed=TRUE)
  text <- gsub("U.S.", "US", text, fixed=TRUE)
  text <- gsub("e.g.", "eg", text, fixed=TRUE)
  text <- gsub("i.e.", "ie", text, fixed=TRUE)
  return(text)
}

############################################################################
#' Stemmer
#' @description background function to load.
#' @param text character vector of strings to clean.
#' @param wstem character Which words should be stemmed? Defaults to "all".
#' @param language Language for stemming. Default is "english".
#' @return Sentence of stemmed words.
#' @keywords internal

stemmer<-function(text, wstem="all", language="english"){
  if(nchar(text)%in%c(NA,NULL,0:2)){
    return(text)
  }else{
    xes<-(strsplit(text, split=" ")[[1]])
    xes<-xes[which(nchar(xes)>0)]
    if(length(wstem)>1) xes<-sapply(xes, function(x) stemexcept(x, wstem, language), USE.NAMES=F)
    if(wstem=="all") xes<-sapply(xes, SnowballC::wordStem, language=language, USE.NAMES=F)
    return(xes)
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
overlaps<-function(high, low, cutoff=1,verbose=FALSE){
  if(cutoff==1){
    combined<-cbind(high,low)
  } else {
    hM=as.matrix(high)
    keep=rep(TRUE,ncol(low))
    xnames=unlist(colnames(low))
    ynames=unlist(colnames(high))
    for(x in 1:ncol(low)){
      lV=as.vector(low[,x])
      yflag="go"
      y=1
      while(yflag!="break"){
        if(xnames[x]%in%strsplit(ynames[y],"_")[[1]]){
          hV=as.vector(hM[,y])
          cossim=sum(lV*hV)/(sqrt(sum(lV^2))*sqrt(sum(hV^2)))
          if(cossim>cutoff){
            if(verbose) message(paste(xnames[x],ynames[y]))
            keep[x]=FALSE
            yflag="break"
          }
        }
        if(y==ncol(high)){
          yflag="break"
        }
        y=y+1
      }
    }
    combined<-cbind(low[,keep],high)
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
  if(ncol(wdcts)>1){
    wdcts<-as.matrix(wdcts)
    words<- colnames(wdcts)
    for (Q in words[duplicated(words)]) {
      wdcts[, (words== Q) & (!duplicated(words))] <- as.numeric(rowSums(wdcts[,(words== Q)]))
      wdcts[, ((words== Q) & (duplicated(words)))] <- NA
    }
    wdcts<-wdcts[, !is.na(colMeans(wdcts))]
  }
  return(wdcts)
}
############################################################################

############################################################################
#' Feature Count Matcher
#' @description background function to load
#' @param hole matrix Token counts in model data.
#' @param peg matrix Token counts in new data.
#' @return Token counts matrix from new data, with column names that match the model data.
#' @keywords internal
vocabmatcher<-function(hole, peg){
  peg<-doublestacker(as.matrix(peg))
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

#' UK to US conversion
#' @description background function to load.
#' @param text character Vector of strings to convert to US spelling.
#' @return character Vector of Americanized strings.
#' @keywords internal
usWords<-function(text){
toks <- quanteda::tokens(text)
tokUS<-quanteda::tokens_lookup(toks, doc2concrete::uk2us,
                               exclusive = FALSE,capkeys = FALSE)
sentUS<-unlist(lapply(tokUS,paste, collapse=" "),use.names = F)
}

#' Cleaning weird encodings
#' @description Handles curly quotes, umlauts, etc.
#' @param text character Vector of strings to clean.
#' @return character Vector of clean strings.
#' @keywords internal
cleanpunct<-function(text){
  # text<- gsub("‘", "'",text)
  # text<-gsub("’", "'", text)
  # text<-gsub("“", '"', text)
  # text<-gsub("”", '"', text)
  text<-gsub("[\x84\x93\x94]", '"', text)
  text<-gsub("[\u201C\u201D\u201E\u201F\u2033\u2036]", '"', text)
  text<-gsub("[\x82\x91\x92]", "'", text)
  text<-gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]", "'", text)
  text<-stringi::stri_trans_general(text, "latin-ascii")
  return(text)
}

