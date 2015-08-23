## The task is to construct an R function that is able to cache potentially time-consuming computations. 
## Matrix inversion is usually considered to be such a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## For this assignment, the assumption is that the matrix supplied is always invertible.


## Here two functions are written that calculate and cash the inverse of a matrix, and if 
## the matrix has been cached before, the inverse of a matrix will be returned 
## instead of computing it repeatedly.

## The following function makeCacheMatrix creates a special “matrix” object that can cash its inverse.
## It is really a list containing a function to   1.set the value of the matrix
##                                                2.get the value of the matrix
##                                                3.set the inverse of the matrix
##                                                4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y)
  {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mat <<- inverse
  getinverse <- function() mat
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function 'cacheSolve' calculates the inverse of the special “matrix” 
## created with the first function, it first checks to see if the inverse of the matrix 
## has already been calculated. If it's been calculated, it gets the values, if not, it calculates
## the inverse of the data and sets the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
## Sample run:
data <- rbind(c(1, -1/2), c(-1/2, 1))
matrix <- makeCacheMatrix(data)
matrix$get()
##       [,1]  [,2]
## [1,]  1.00 -0.5
## [2,] -0.5  1.00

## No cache in the first run
cacheSolve(matrix)
##            [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

## Retrieving from the cache in the second run
cacheSolve(matrix)
## cached data displayed