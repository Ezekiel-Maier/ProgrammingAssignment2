## makeCacheMatrix creates an object that can cache a matrix an its inverse
## cacheSolve returns the inverse of the matrix stored in the makeCacheMatrix object. The
## inverse matrix is only computed if it is not stored in the makeCacheMatrix object.
##
## Example:
## mat <- matrix(data = c(4,2,7,6), nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(mat)
## cacheMatrix$get()                        ## returns original matrix
## cacheMatrix$getInverseMatrix()           ## returns NULL
## cacheSolve(cacheMatrix)                  ## computes and returns the inverse matrix
## cacheSolve(cacheMatrix)                  ## returns the cached inverse matrix

## This function creates a list containing functions to
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inv_mat) inverse_matrix <<- inv_mat
  getInverseMatrix <- function() inverse_matrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## This function returns the inverse of the matrix stored in the cache object created by makeCacheMatrix
## The function will:
## 1. Check the cache to see if the inverse has been computed and stored
## 2. If the cache contains the inverse matrix, then it is returned from the cache
## 3. If the cache doesn't contain the inverse matrix, then:
## 3.1. The (non-inverse) matrix is taken from the cache
## 3.2. The inverse of the matrix is computed (using solve) 
## 3.3. The inverse matrix value is stored in the cache
## 3.4. The inverse matrix is returned
cacheSolve <- function(x, ...) {
  inv_mat <- x$getInverseMatrix()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  reg_matrix <- x$get()
  inv_mat <- solve(reg_matrix, ...)
  x$setInverseMatrix(inv_mat)
  inv_mat
}
