#' Concreteness mTurk Word List
#'
#' Word list from Brysbaert, Warriner & Kuperman (2014)
#'
#' @format A list of 39,954 words that have been hand-annotated for concreteness
#' @source Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. Behavior Research Methods, 46(3), 904-911.
#'
"mturk_list"

#' Concreteness mTurk Word List
#'
#' Word list from Paetzlod & Specia (2017)
#'
#' @format A list of 85,942 words where concreteness was imputed using word embeddings
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

#' Pre=trained Concreteness Detection Model for Advice
#'
#' Trained on advice datasets
#'
#' @format A pre-trained glmnet model
#' @source Yeomans (2019). Concreteness, Concretely.
#'
"adviceModel"

#' Pre=trained Concreteness Detection Model for Plan-Making
#'
#' Trained on planning datasets
#'
#' @format A pre-trained glmnet model
#' @source Yeomans (2019). Concreteness, Concretely.
#'
"planModel"


#' Pre-trained advice concreteness features
#'
#' @format A (truncated) matrix of ngram feature counts for alignment to the pre-trained advice glmnet model.
#' @source Yeomans (2019). Concreteness, Concretely.
#'
"adviceNgrams"


#' Pre-trained plan concreteness features
#'
#' @format A (truncated) matrix of ngram feature counts for alignment to the pre-trained planning glmnet model.
#' @source Yeomans (2019). Concreteness, Concretely.
#'
"planNgrams"
