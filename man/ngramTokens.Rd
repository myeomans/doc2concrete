% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ngramTokens.R
\name{ngramTokens}
\alias{ngramTokens}
\title{Ngram Tokenizer}
\usage{
ngramTokens(
  texts,
  wstem = "all",
  ngrams = 1,
  language = "english",
  punct = TRUE,
  stop.words = TRUE,
  number.words = TRUE,
  per.100 = FALSE,
  overlap = 1,
  sparse = 0.995,
  verbose = FALSE,
  vocabmatch = NULL,
  num.mc.cores = 1
)
}
\arguments{
\item{texts}{character vector of texts.}

\item{wstem}{character Which words should be stemmed? Defaults to "all".}

\item{ngrams}{numeric Vector of ngram lengths to be included. Default is 1 (i.e. unigrams only).}

\item{language}{Language for stemming. Default is "english"}

\item{punct}{logical Should punctuation be kept as tokens? Default is TRUE}

\item{stop.words}{logical Should stop words be kept? Default is TRUE}

\item{number.words}{logical Should numbers be kept as words? Default is TRUE}

\item{per.100}{logical Should counts be expressed as frequency per 100 words? Default is FALSE}

\item{overlap}{numeric Threshold (as cosine distance) for including ngrams that constitute other included phrases. Default is 1 (i.e. all ngrams included).}

\item{sparse}{maximum feature sparsity for inclusion (1 = include all features)}

\item{verbose}{logical Should the package report token counts after each ngram level? Useful for long-running code. Default is FALSE.}

\item{vocabmatch}{matrix Should the new token count matrix will be coerced to include the same tokens as a previous count matrix? Default is NULL (i.e. no token match).}

\item{num.mc.cores}{numeric number of cores for parallel processing - see parallel::detectCores(). Default is 1.}
}
\value{
a matrix of feature counts
}
\description{
Tally bag-of-words ngram features
}
\details{
This function produces ngram featurizations of text based on the quanteda package. This provides a complement to the doc2concrete function by demonstrating
How to build a feature set for training a new detection algorithm in other contexts.
}
\examples{

dim(ngramTokens(feedback_dat$feedback, ngrams=1))
dim(ngramTokens(feedback_dat$feedback, ngrams=1:3))
}
