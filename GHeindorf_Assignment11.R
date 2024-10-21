library(lares)

tukey_multiple <- function(x) {
   outliers <- array(TRUE,dim=dim(x))
   for (j in 1:ncol(x)){
     outliers[,j] <- outliers[,j] & outlier_turkey(x[,j])
    }
    outlier.vec <- vector(length=nrow(x))
    for (i in 1:nrow(x)){
      outlier.vec[i] <- all(outliers[i,])
    }
    return(outlier.vec) 
    }


tester <- matrix(c(1, 2, 3, 10, 20, 40, 20, 4, 20, 30, -20, 0), nrow = 6)
tukey_multiple(tester)

###Initial Debugging Code:

#debug(tukey_multiple)
#tukey_multiple(tester)
#Error in outliers[, j] && tukey.outlier(x[, j]) : 
#  'length = 2' in coercion to 'logical(1)'
#undebug(tukey_multiple)

####Code used in command line:

# setBreakpoint("GHeindorf_Assignment11.R", 5)
# debug(tukey_multiple)
# tukey_multiple(tester)
# undebug(tukey_multiple)
# untrace(setBreakpoint)

tukey_multiple(tester)

