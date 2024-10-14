raindata <- precip
mode(raindata)
typeof(raindata)
methods(class = class(raindata))
getS3method("as.data.frame", class(raindata))
str(raindata)
attributes(raindata)
isS4(raindata)


###Create S3 classes and method
pops <- list(name = "Chocolate", price = 23.20, calories = 120)
class(pops) <- "popsicles"
attributes(pops)

storms <- list(name = c("Chuck", "Brownie", "Derek"),
               maxwind = c(175, 95, 135), 
               year = c(2020, 1975, 2007))
class(storms) <- "hurricanes"
sort.hurricanes <- function(storms){
  sorted <- order(storms$maxwind, decreasing = T)
  for (i in 1:length(sorted)){
    ind <- which(sorted == i)
    cat(storms$name[ind], " (", storms$year[ind], "): ", 
        storms$maxwind[ind], "\n", sep = "")
  }
  sorted_storms <- storms
  sorted_storms$name <- storms$name[sorted]
  sorted_storms$maxwind <- storms$maxwind[sorted]
  sorted_storms$year <- storms$year[sorted]
  return(sorted_storms)
}
sorted <- sort(storms)

###Create S4 classes and method
setClass("player",
         representation(
           name = "character",
           team = "character",
           number = "numeric",
           contract = "numeric"))
tom <- new("player", name = "Tom Dunk", team = "Mavs", 
           number = 27, contract = 2026)
tom
tom@name

setClass("address",
         representation(
           name = "character",
           code = "numeric",
           street = "character",
           zip = "numeric",
           city = "character",
           state = "character"))
amy <- new("address", name = "Amy Chobe", code = 21045, 
           street = "Long Leaf Drive", zip = 29871, 
           city = "Omaha", state = "Nebraska")
amy
show(amy)
setMethod("show", "address",
          function(object){
            cat(object@name, " lives at ", object@code, " ", 
                object@street, " ", object@city, ", ", 
                object@state, " ", object@zip, "\n", sep = "")
          }
)
show(amy)
