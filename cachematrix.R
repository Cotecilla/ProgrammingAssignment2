## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special matrix object that can cache its inverse.
## cacheSolve calculates the inverse of the matrix returned by makeCacheMatrix.
## If the matrix inverse has already been computed, 
## cacheSolve will find it in the cache and return the inverse of the special matrix, instead of recalculating it.

## Write a short comment describing this function
## R Programming Assignment2 Lexical Scoping: Question 1. 

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  # This function sets the value of the matrix.
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  # This function gets the value of the matrix.
  get <- function() x
  # This function sets the value of the inverse of the matrix
  setinverse <- function(inverse) inversematrix <<- inverse
  # This function gets the value of the inverse of the matrix.
  getinverse <- function() inversematrix
  # The following code returns a list with the four functions (set, get, setinverse and getinverse)
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## R Programming Assignment2 Lexical Scoping: Question 2. 

cacheSolve <- function(x, ...) {
  inversematrix <- x$getinverse()
  if (!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  # The following code computes the inverse of the matrix
  inversematrix <- solve(data, ...)
  # This caches the result of the inverse of the matrix
  x$setinverse(inversematrix)
  # ...and this is the inverse matrix
  inversematrix
}


## Just to be sure my code works... here i provide an example:
ex_matrix <- makeCacheMatrix(matrix(c(4,2,7,6), 2, 2))
ex_matrix$get()
cacheSolve(ex_matrix)
cacheSolve(ex_matrix)
ex_matrix$getinverse()

