## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is to create a matrix object that can cache its inverse.
## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The functions below will works as follows:

makeCacheMatrix <- function(x = matrix()) {
              ivs <- NULL
              set <- function (y){
                  x <<- y
                  ivs <<- NULL
              }
              get <- function()x
              setivs <- function(inverse) ivs <<- inverse
              getivs <- function()ivs
              list(set = set, get= get, setivs = setivs, getivs = getivs)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## this function will retrieve the inverse from the cache.
## This function below wil works as follows:
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       ivs <- x$getivs()
       if(!is.null(ivs)){
               print("Cached Data!")
               return(ivs)
    }

       else {
         print("New Data Calculating!")
         data <- x$get()
         ivs <- solve(data, ...)
         x$setivs(ivs)
         ivs
    }
}
