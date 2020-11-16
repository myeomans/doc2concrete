#' Concreteness mTurk Word List
#'
#' Word list from Brysbaert, Warriner & Kuperman (2014). A list of 39,954 words that have been hand-annotated by crowdsourced workers for concreteness.
#'
#' @format A data frame with 39,954 rows and 2 variables.
#' \describe{
#'   \item{Word}{character text of a word with an entry in this dictionary}
#'   \item{Conc.M}{average concreteness score for that word (from 1-5)}
#' }
#' @source Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. Behavior Research Methods, 46(3), 904-911.
#'
"mturk_list"

#' Concreteness mTurk Word List
#'
#' Word list from Paetzold & Specia (2016). A list of 85,942 words where concreteness was imputed using word embeddings.
#'
#' @format A data frame with 85,942 rows and 2 variables.
#' \describe{
#'   \item{Word}{character text of a word with an entry in this dictionary}
#'   \item{Conc.M}{predicted concreteness score for that word (from 100-700)}
#' }
#' @source #' Paetzold, G., & Specia, L. (2016, June). Inferring psycholinguistic properties of words. In Proceedings of the 2016 Conference of the North American Chapter of the Association for Computational Linguistics: Human Language Technologies (pp. 435-440).
#'
"bootstrap_list"

#' Personal Feedback Dataset
#'
#' A dataset containing responses from people on Mechanical Turk, writing
#' feedback to a recent collaborator, that were then scored by other Turkers
#' for feedback specificity.
#'
#' @format A data frame with 171 rows and 2 variables:
#' \describe{
#'   \item{feedback}{character text of feedback from writers}
#'   \item{concrete}{numeric average specificity score from readers}
#' }
#' @source Blunden, H., Green, P., & Gino, F. (2018).
#'
#' "The Impersonal Touch: Improving Feedback-Giving with Interpersonal Distance."
#'
#' Academy of Management Proceedings, 2018.
#'
"feedback_dat"

#' Pre-trained Concreteness Detection Model for Advice
#'
#' This model was pre-trained on 3289 examples of feedback on different tasks (e.g. writing a cover letter, boggle, workplace annual reviews). All of those documents were annotated by research assistants for concreteness, and this model simulates those annotations on new documents.
#'
#' @format A pre-trained glmnet model
#' @source Yeomans (2020). A Concrete Application of Open Science for Natural Language Processing.
#'
"adviceModel"

#' Pre-trained Concreteness Detection Model for Plan-Making
#'
#' This model was pre-trained on 5,172 examples of pre-course plans from online courses at HarvardX. Each plan was annotated by research assistants for concreteness, and this model simulates those annotations on new plans.
#'
#' @format A pre-trained glmnet model
#' @source Yeomans (2020). A Concrete Application of Open Science for Natural Language Processing.
#'
"planModel"


#' Pre-trained advice concreteness features
#'
#' For internal use only. This dataset demonstrates the ngram features that are used for the pre-trained adviceModel.
#'
#' @format A (truncated) matrix of ngram feature counts for alignment to the pre-trained advice glmnet model.
#' @source Yeomans (2020). A Concrete Application of Open Science for Natural Language Processing.
#'
"adviceNgrams"


#' Pre-trained plan concreteness features
#'
#' For internal use only. This dataset demonstrates the ngram features that are used for the pre-trained planModel.
#'
#' @format A (truncated) matrix of ngram feature counts for alignment to the pre-trained planning glmnet model.
#' @source Yeomans (2020). A Concrete Application of Open Science for Natural Language Processing.
#'
"planNgrams"

#' UK to US Conversion dictionary
#'
#' For internal use only. This dataset contains a quanteda dictionary for converting UK words to US words. The models in this package were all trained on US English.
#'
#' @format A quanteda dictionary with named entries. Names are the US version, and entries are the UK version.
#' @source Borrowed from the quanteda.dictionaries package on github (from user kbenoit)
#'
"uk2us"
