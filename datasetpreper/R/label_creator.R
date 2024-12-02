

#' Label Creator
#'
#' @description
#' 'label_creator()' allows one to quickly and personally create a
#' sequence of unique labels by merging a common base(s) with each
#' unique element from a vector.
#'
#'
#'
#' @param base Base(s) that are common to all labels.
#' @param unique Vector of unique elements for each unique label desired.
#' @param order Order in which to use the unique elements provided, by
#' default it applies unique elements in the order their provided.
#' @param uniquefront T/F, should unique element be put at the front
#' of the labels.
#' @param base_sep String used to separate base elements, if multiple
#' are provided.
#' @param base_to_unique_sep String use to separate base element(s) from
#' unique element.
#' @param copies Integer, how many copies of the labels should be returned.
#'
#' @return 'label_creator()' returns a vector of labels (strings)
#' with a length equal to the length of 'unique'. If 'copies' is
#' greater than 1, then a list with the number of elements equal to
#' the number of copies requested will be returned, each element
#' being a vector according to the description provided above.
#'
#' @examples
#' label_creator(base = "BFI",
#'               unique = c(1, 2, 3, 4, 5),
#'               uniquefront = FALSE,
#'               base_to_unique_sep = "_",
#'               copies = 2)
#'
#' label_creator(base = c("PID", "Gran"),
#'               unique = c(1, 2, 3, 4, 5),
#'               order = c(5, 4, 3, 2, 1),
#'               uniquefront = FALSE,
#'               base_sep = "_",
#'               base_to_unique_sep = "#",
#'               copies = 1)
#'
#' label_creator(base = "GYM",
#'               unique = c("North", "South", "East", "West"),
#'               uniquefront = TRUE,
#'               base_to_unique_sep = " ",
#'               copies = 1)
#'
#'
#' @export



label_creator <- function(base,
                          unique = NULL,
                          order = 1:length(unique),
                          uniquefront = FALSE,
                          base_sep = " ",
                          base_to_unique_sep = " ",
                          copies = 1){

  ###Argument Checking
  if (missing(base)){
    stop("Argument 'base' is missing without default")
  }
  stopifnot(is.numeric(order), is.character(base_sep),
            is.character(base_to_unique_sep), is.numeric(copies),
            is.logical(uniquefront), copies >= 1)
  if (!(is.null(unique)) & length(order) != length(unique)){
    stop("Argument 'order' must be same length as 'unique'")
  }
  if (length(base_to_unique_sep) != 1 &
      length(base_to_unique_sep) != length(unique)){
    stop("Argument 'base_to_unique_sep' does not equal 1 or length of 'unique'")
  }

  order = round(order)

  ###Functional Code
  new_labels <- c()
  if (!(is.null(unique))){
    if (uniquefront){
      for (i in order){
        new_labels <- c(new_labels, paste(unique[i],
                                  paste(base, collapse = base_sep),
                                  sep = base_to_unique_sep))
      }
    } else{
      for (i in order){
        new_labels <- c(new_labels,
                        paste(paste(base, collapse = base_sep),
                                  unique[i],
                                  sep = base_to_unique_sep))
      }
    }
  } else {
    order = 1
      for (i in order){
        new_labels <- c(new_labels, paste(base, collapse = base_sep))
      }
  }
  ###Finalize copies and return
  if (copies != 1){
    final_copies <- list()
    for (i in 1:copies){
      final_copies[[i]] <- new_labels
    }
    return(final_copies)
  } else {
    return(new_labels)
  }
}
