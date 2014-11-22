## set of function to store in cache and calculate the inverse of a numeric matrix
## 

## makeCacheMatrix
## input : numeric matrix
## output : a special matrix which is a list of function to set or get the matrix and set or get
## the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {     # set the value of the matrix and the cache
    x <<- y
    m <<- NULL      # m and x are  out of the function set
  }
  get <- function() x # return the value of the matrix
  setsolve       <- function(solve) m <<- solve  # calculate and store the inverse matrix
  getsolve <- function() m  # return the inverse matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve
## input : an object created by the first function makeCacheMatrix
## output : the inverse matrice

cacheSolve <- function(x, ...) {
        ## Return  the inverse matrix of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {     # if the cache is not null return the cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()  
  # if the cache is Null calculate the inverse matrix and store it  in the cache
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
