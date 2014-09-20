## The functions created here allow to cache the inverse of a matrix
## in order to avoid computing it repeatly which is computationally expensive.
## The first function (makeCacheMatrix) creates a special matrix object that
## associates a regular matrix with a variable in which the inverse of the matrix can be stored.
## The second function (cacheSolve) calculates the inverse from this special matrix
## but will check first if the inverse has been calculated and cached.
##

## The first function, makeCacheMatrix creates a special "matrix", 
## which is a list containing a function to:
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
##
## because these functions are created in the makeCacheMatrix function, 
## the matrix 'x' and mean 'm' will be free variables in the environment 
## of these four functions. However, the set-functions ('set' and 'setinv')
## will not be called from within the makeCacheMatrix environment but
## from the environment in which the list object is created (usually this is the global environment).
## therefore, the operator '<<-' is used to assign the input arguments to the free variables.
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matrix.inverse) inv <<- matrix.inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##The second function, cacheSolve calculates the inverse of the matrix using the 
##special matrix objects created with the makeCacheMatrix as input. The inverse
## of the matrix is returned as output.
##First, it is checked whether the inverse is already cached. If so, then the cached
##inverse is extracted and returned. If not, the inverse is calculated, cached and returned.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(mat)
  inv
 
}
