## R Programming Assignment 2
## Butterworks, Rudolf J
## From the discussion board; thank you to Semhar for the great post to explain the example functions.
## 4 functions:
##  makeVector, from assignment 2 example but added commentry to help me understand what whats being achieved.
##  cacheMean, from assignment 2 example
##  makeCacheMatrix, function that creates special matrix cache to be solved by solveCache,
##  solveCache, takes the special matrix cache and solves the inverse.


## Copied from makeVector, this function sets out 4 functions within it self, set, get, setInv and getInv.
## makeCacheMatrix does not calculate anything, it stores the given matrix or matrix via $set

makeCacheMatrix <- function(x = matrix()) {
  #Function to create a special matrix object that can cache its inverse
  #body copied / edited from makeVector example
  i <- NULL
  #same as set in makeVector, set changed the vector in the main "makeCacheMatrix" function
  set <- function(y) {
    x <<- y
    i <<- NULL #inverse set to null, as set will change the vector and the inverse must be recalculated.
  }
  get <- function () x #same as example
  setInv <- function(inv) i <<- inv #taken from mean example in setMean.  
  getInv <- function () i
  list(set = set, get = get, setInverse = setInv, getInv = getInv)
}

## copied from cacheMean, this function uses the function solve to return the inverse of x 

cacheSolve <- function(x, ...) {
 
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setInv(i)
  i
}

#Example Functions (from Assignment two outline)
makeVector <- function(x = numeric()) {
  m <- NULL
  #set function changes the vector stored in the main "makeVector" function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean  #Does not calculate the mean, store the value of the input in a variable m into the main fuction
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
      getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}