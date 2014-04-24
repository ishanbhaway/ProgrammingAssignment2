## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ###setting initial value to NULL
  set <- function(y) { ###sets the value of the matrix into another internal cached memory 
    x <<- y ###
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinv()###gets the value from the makeCacheMatrix in order to verify whether inverse was already calculated
  if(!is.null(m)) { ###verification whether inverse was precalculated
    message("getting cached data")
    return(m) ###returning cached inverse
  }
  data <- x$get()###retrieves the data or original matrix from the makeCacheMatrix
  m <- solve(data, ...) ###calculates the inverse
  x$setinv(m)###sends the inverse to get inserted into the cache
  m
        ## Return a matrix that is the inverse of 'x'
}
