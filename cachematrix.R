## code for R Programming Course, week 3 programming assignment
## make functions to cache inverses of matrices

## create "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## compute inverse of "matrix" returned by makeCacheMatrix
## if inverse has been computed, retrieve it from cache
## if inverse has not been computed, calculate it
## must take input of the type makeCacheMatrix() 
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
