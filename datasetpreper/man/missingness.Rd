% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missingness.R
\name{missingness}
\alias{missingness}
\title{Missingness}
\usage{
missingness(
  data,
  missing = NA,
  returnFulls = FALSE,
  histogram = FALSE,
  axis = 1
)
}
\arguments{
\item{data}{A dataframe or vector which is to be checked for missing data.}

\item{missing}{A single value or special character (default = NA)
that is to be treated as a missing value (e.g., 0, NULL, 99).}

\item{returnFulls}{T/F, would you like only full instances, those with
no missing data, to be returned?}

\item{histogram}{T/F, should a histogram illustrating the amount of
missing data per observation or variable be printed? (only an option
if 'data' is a dataframe or matrix)}

\item{axis}{1 = determine missingness by row; 2 = determine
missingness by column. (only necessary if 'data' is a dataframe or
matrix)}
}
\value{
'missingness()' returns a numerical value equal to the
number of missing data points if 'data' is a vector. If 'data' is a
matrix or dataframe, a vector equal to the number of rows (axis = 1) or
columns (axis = 2) is returned with the number of missing values for
each instance or variable. If 'returnFulls' is True, then a list is
returned with 'missingness' containing the vector of counts for each
instance or variable, and 'newdata'-consisting of all instances or
variables from the original dataset that had no missing values.
}
\description{
'missingness()' provides one the opportunity to very easily
summarize the amount of missing values in their data and to
conveniently remove variables or observations that are not
full cases.
}
\examples{

missingness(datapreper::ex_QuestData$Q2,
            missing = NA,
            returnFulls = FALSE)

missingness(datapreper::ex_QuestData,
            missing = NA,
            histogram = TRUE,
            axis = 2)

clean_quest_data <- missingness(datapreper::ex_QuestData,
                                missing = NA,
                                returnFulls = TRUE,
                                histogram = FALSE,
                                axis = 1)


}
