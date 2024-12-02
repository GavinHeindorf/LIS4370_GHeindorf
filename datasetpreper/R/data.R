#' Example Experimental Data
#'
#' Includes randomly sampled values and conditions, as if experimental data was put in long format.
#'
#' @format ## `ex_ExperimentalData`
#' A data frame with 50 rows and 2 columns:
#' \describe{
#'   \item{Cond}{column with four unique, two letter mappings for 4 conditions}
#'   \item{value}{Randomly sampled values from 0 to 21}
#' }
"ex_ExperimentalData"

#' Example Questionnaire Data
#'
#' Sample questionnaire data, with the range of options being 1-5 and NAs included.
#'
#' @format ## `ex_QuestData`
#' A data frame with 20 rows and 5 columns:
#' \describe{
#'   \item{Q1, Q2, Q3, Q4, Q5}{Each column represents a question. Possible values were 1-5, randomly sampled, and NAs included}
#' }
"ex_QuestData"
