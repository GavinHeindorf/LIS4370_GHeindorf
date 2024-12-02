#' Recode Values
#'
#' @param variable Vector containing the values to be recoded.
#' @param reverse T/F, should values simply be recoded? Default is True.
#' Assumes current levels are numeric.
#' @param curlevels Optional: vector containing the current levels of
#' your variable.
#' @param newlevels Optional: vector containing the new levels for the variable,
#' should correspond directly with 'curlevels'.
#' @param labels Optional: if you would like the returned variable to be a
#' factor with labels, provide a vector containing labels equal to the length
#' of 'newlevels'.
#' @param remove Level(s) from original variable that you would like removed
#' before the variable is recoded (do not include the level(s) in the
#' 'curlevels' or 'newlevels' arguments.
#'
#' @return 'recode_values()' returns the original variable recoded and
#' formatted as a factor, without labels if none are provided for 'labels'
#' in the function call.
#'
#' @examples
#' datapreper::ex_QuestData$Q1
#' recode_values(datapreper::ex_QuestData$Q1,
#'               reverse = TRUE)
#'
#' datapreper::ex_QuestData$Q2
#' recode_values(datapreper::ex_QuestData$Q2,
#'               reverse = FALSE,
#'               curlevels = c(1, 2, 3, 4, 5),
#'               newlevels = c(0, 1, 2, 3, 4),
#'               remove = NA)
#'
#' datapreper::ex_QuestData$Q3
#' recode_values(datapreper::ex_QuestData$Q3,
#'               reverse = FALSE,
#'               curlevels = c(1, 2, 3, 4, 5),
#'               newlevels = c(10, 20, 30, 40, 50),
#'               labels = c("Low", "Mid-Low", "Mid",
#'                           "Mid-High", "High"),
#'               remove = FALSE)
#'
#' @export




recode_values <- function(variable,
                          reverse = TRUE,
                          curlevels = FALSE,
                          newlevels = FALSE,
                          labels = FALSE,
                          remove = FALSE){

  if (missing(variable)){
    stop("Error: No variable provided")
  }

    if (!isFALSE(remove)){
    stopifnot(is.vector(remove), remove %in% variable,
              (length(variable)-length(remove)) > 0)
    if (!isFALSE(curlevels)){
      stopifnot((length(unique(variable))-length(remove)) == length(curlevels),
                all(curlevels %in% setdiff(variable, remove)))
    }
    newvar <- variable[which(variable %in% setdiff(variable, remove))]
  } else {
    newvar <- variable
  }

  if (!isFALSE(curlevels) | !isFALSE(newlevels)){
    stopifnot(is.vector(curlevels), is.vector(newlevels),
              length(curlevels) == length(newlevels),
              length(curlevels) > 0)
    reverse <- F
  }

  stopifnot(is.logical(reverse))

  if (reverse){
    stopifnot(is.numeric(variable))
    if (!isFALSE(labels)){
      print("WARNING: Labels is being ignored, values are being reverse coded")
    }
  }

  if (!isFALSE(labels)){
    stopifnot(is.vector(labels))
    if (!isFALSE(curlevels)){
      stopifnot(length(labels) == length(newlevels))
    }
  }


  if (!isFALSE(curlevels)){
    for (i in 1:length(curlevels)){
      newvar[which(newvar == curlevels[i])] <-
        newlevels[i]
    }

    if (!isFALSE(labels)){
      return(factor(x = newvar, levels = newlevels,
                  labels = labels))
    } else{
      return(factor(x = newvar, levels = newlevels))
    }
  } else if (reverse){
    curs <- order(levels(factor(newvar)))
    curs <- rev(curs)
    indices <- list()
    final <- newvar
    for (i in 1:length(curs)){
      indices <- which(newvar == curs[i])
      final[indices] <- rev(curs)[i]
    }
    return(factor(x = final))
  }
}
