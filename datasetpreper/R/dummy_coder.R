#' Dummy Coder
#'
#' @description
#' 'dummy_coder()' allows one to quickly and easily create a dummy coded variable
#' based off another vector.
#'
#'
#' @param regexps A vector of strings containing the regular
#' expressions for each respective level one would like to
#' designate in the new dummy coded variable.
#' @param baseoff The vector to which the regular expressions
#' are applied to create the dummy coded variable.
#' @param levels The levels, each of which should correspond
#' to one of the 'regexps' provided, that will be included in
#' the new dummy coded variable.
#' @param type What variable type would you like the dummy
#' coded variable to be returned as? Options: factor,
#' integer, numeric, or character.
#' @param labels If 'type' is factor, optional labels
#' for the levels of the new dummy coded variable.
#'
#' @return 'dummy_coder()' returns the dummy
#' coded variable as a vector, type determined
#' by 'type', that contains each of the corresponding values
#' based off 'baseoff'.
#'
#' @examples
#' exp_data <- datapreper::ex_ExperimentalData
#'
#' exp_data$dummyone <- dummy_coder(regexps = c("^H", "^L"),
#'                             baseoff = exp_data$Cond,
#'                             levels = c(1, 0),
#'                             type = "factor",
#'                             labels = c("High", "Low"))
#'
#' exp_data$dummytwo <- dummy_coder(regexps = c("W$", "L$"),
#'                             baseoff = exp_data$Cond,
#'                             levels = c(1, 0),
#'                             type = "numeric")
#'
#'
#' @export
#'


dummy_coder <- function(regexps,
                        baseoff,
                        levels,
                        type = "factor",
                        labels = FALSE){

  stopifnot(!missing(regexps), length(regexps) > 0,
            is.vector(baseoff), !missing(levels), is.vector(baseoff),
            length(regexps) == length(levels))

  if (!(type %in% c("factor", "numeric", "integer", "character"))){
    stop("ERROR: 'type' must be one of 'factor', 'numeric', 'integer', or 'character'")
  } else{
    if (identical(type, "numeric")){
      labels = F
      stopifnot(is.numeric(try(as.numeric(levels),
                               silent = T)))
    } else if (identical(type, "integer")){
      stopifnot(is.integer(try(as.integer(levels),
                               silent = T)))
      labels = F
    } else if (identical(type, "character")){
      stopifnot(is.character(try(as.character(levels),
                                 silent = T)))
      labels = F
    }
  }

  if (!isFALSE(labels)){
    stopifnot(is.vector(labels), length(labels) == length(levels))
  }

  newvec <- rep(NA, length(baseoff))
  remslabs <- c()

  for (i in 1:length(regexps)){
    inds <- grep(regexps[i], baseoff)
    if (length(i) == 0){
      cat("WARNING: no matches for '", regexps[i], "' in 'baseoff' vector")
      remslabs <- c(remslabs, i)
    } else {
      newvec[inds] <- levels[i]
    }
  }

  if (identical(type, "factor")){
    if (length(remslabs) > 0){levels <- levels[-remslabs]}
    if (!isFALSE(labels)){
      if (length(remslabs) > 0){labels <- labels[-remslabs]}
      finalvec <- factor(newvec, levels = levels, labels = labels)
    } else{
      finalvec <- factor(newvec, levels = levels)
    }
  } else if (identical(type, "numeric")){
    finalvec <- as.numeric(newvec)
  } else if (identical(type, "integer")){
    finalvec <- as.integer(newvec)
  } else if (identical(type, "character")){
    finalvec <- as.character(newvec)
  }

  return(finalvec)
}
