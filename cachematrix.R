# This file contains two functions: cacheMatrix and cacheInverse.
# Combined, they make it possible to cache a matrix along with its inverse.
# Below are two examples to show how the functions work.

##  Example 1 - Use the function cacheMatrix with some data, and retrieve cached information.

# > mymatrix = CacheMatrix(matrix(c(1,3,3,1), nrow=2, ncol=2))
# > mymatrix$get()
#       [,1] [,2]
#  [1,]    1    3
#  [2,]    3    1

##   Example 2 - Use the function CacheInverse to obtain the inverse of the matrix stored as mymatrix.

#  > CacheInverse(amatrix)
#       [,1]   [,2]
#  [1,] -0.125  0.375
#  [2,]  0.375 -0.125

## Function 1 - CacheMatrix
#  This function takes a matrix as its argument.
#  Such a matrix can be produced by using the built-in R function matrix()
CacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # CacheMatrix applies the built-in R function 
  # solve() to obtain the matrix inverse
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function 2 - CacheInverse
#  This function takes a cached matrix as its argument
#  and produces the inverse of the matrix
CacheInverse <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}





