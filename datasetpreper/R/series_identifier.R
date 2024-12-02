#' Series Identifier
#'
#' @description
#' 'series_identifier()' allows for the quick counting and location of
#' an overlapping or non-overlapping series within a vector of
#' elements.
#'
#'
#' @param vector Vector within which to identify the series.
#' @param series Vector of elements matching the series that
#' one wants to count or locate.
#' @param overlap T/F, can series within 'vector' overlap and still count?
#' @param indices T/F, do you want the indices of each identified series
#' to be returned?
#'
#' @return 'series_identifier()' returns a list with 'total' equaling
#' the number of series identified within 'vector'; and if 'indices' is
#' True, then 'matchindices' is included in the list. 'matchindices' is itself
#' a list, with each element being one of the respective matches and its'
#' associated indices.
#'
#' @examples
#' apply(datapreper::ex_QuestData, 1,
#'             series_identifier,
#'             series = c(4, 4),
#'             overlap = TRUE,
#'             indices = FALSE)
#'
#' series_identifier(series = c(1, 3, 4),
#'                   vector = datapreper::ex_QuestData$Q5,
#'                   overlap = TRUE,
#'                   indices = TRUE)
#'
#' @export



series_identifier <- function(vector,
                              series,
                              overlap = FALSE,
                              indices = FALSE){
  if (missing(series)){
    stop("Error: No series provided")
  }
  if (missing(vector)){
    stop("Error: No vector provided")
  }
  stopifnot(is.vector(vector), is.logical(overlap))
  if (length(series) >= length(vector)){
    stop("Error: Series provided is longer than or the same length as the vector provided")
  }

  slength <- length(series)
  inds <- 1:length(vector)
  keepers <- 1:slength

  names(vector) <- inds

  totalmatches <- 0
  foundindices <- list()

  if (overlap){
    i = 1
    while (length(keepers) >= slength){
      curset <- vector[as.character(i:(i+slength-1))]
      if (setequal(curset, series)){
        totalmatches <- totalmatches + 1
        if (indices){foundindices[[totalmatches]] <-
          as.character(i:(i+slength-1))}
      }
      keepers <- setdiff(inds, 1:i)
      vector <- vector[as.character(keepers)]
      i <- i + 1
    }
  } else {
    i = 1
    while (length(keepers) >= slength){
      curset <- vector[as.character(i:(i+slength-1))]
      if (setequal(curset == series)){
        keepers <- setdiff(inds, c(1:(i+slength-1)))
        totalmatches <- totalmatches + 1
        if (indices){foundindices[[totalmatches]] <-
          as.character(i:(i+slength-1))}
        vector <- vector[as.character(keepers)]
        i <- i + (slength)
      } else{
        keepers <- setdiff(inds, 1:i)
        vector <- vector[as.character(keepers)]
        i <- i + 1
      }
    }
  }
  if (indices){return(list(total = totalmatches,
                           matchindices = foundindices))}
  else{return(list(total = totalmatches))}
}
