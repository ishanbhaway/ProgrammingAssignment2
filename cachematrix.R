

## makeCacheMatrix : it is a function to cache the inverse of a given matrix post its calculation of inverse from cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL           ### initializing a vector inv for the functions operations
  set <- function(y) {  ### sets the value of the matrix into another internal cached memory 
    x <<- y             ### pulling the original data into the given function if not initialized
    inv <<- NULL        ### take the value of NULL if the solve has not cached any value inside
  }
  get <- function() x                     ### function to transfer data from this function to another
  setinv <- function(solve) inv <<- solve ### function to cache the inverse of the matrix
  getinv <- function() inv                ### function to transfer inverse or NULL based on whether it has been already processed or not
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)                   ### creates a list of the funtions with the values stored as default

}


## cacheSolve : is a function to give inverse of a matrix created using makeCacheMatrix.
##              this function will return cached inverse value if inverse was already calculated, otherwise will calculate the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()                 ### gets the value from the makeCacheMatrix. will store cached inverse if it was already calculated, otherwise will be set NULL
  if(!is.null(inv)) {               ### verification whether inverse was precalculated
    message("getting cached data")  ### message to denote this is the cached data
    return(inv)                     ### returning cached inverse
  }
  data <- x$get()                   ### retrieves the data or original matrix from the makeCacheMatrix
  inv <- solve(data, ...)           ### calculates the inverse
  x$setinv(inv)                     ### sends the inverse to get inserted into the cache
  inv                               ### Return a matrix that is the inverse of 'x'
}