#' Concreteness mTurk Word List
#'
#' Word list from Brysbaert, Warriner & Kuperman (2014)
#'
#' @format A list of 39,954 words that have been hand-annotated for concreteness
#'
"mturk_list"

#' Concreteness mTurk Word List
#'
#' Word list from Paetzlod & Specia (2017)
#'
#' @format A list of 85,942 words where concreteness was imputed using word embeddings
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
