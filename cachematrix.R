## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ###initializing a vector inv for the functions operations
  set <- function(y) { ###sets the value of the matrix into another internal cached memory 
    x <<- y ###pulling the original data into the given function
    inv <<- NULL###take the value of NULL if the solve has not cached any value inside
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()###gets the value from the makeCacheMatrix in order to verify whether inverse was already calculated
  if(!is.null(inv)) { ###verification whether inverse was precalculated
    message("getting cached data") ###message to denote this is the cached data
    return(inv) ###returning cached inverse
  }
  data <- x$get()###retrieves the data or original matrix from the makeCacheMatrix
  m <- solve(data, ...) ###calculates the inverse
  x$setinv(m)###sends the inverse to get inserted into the cache
  m
        ## Return a matrix that is the inverse of 'x'
}
