## Function "makeVector" creates a  list containing a function to:
## 1:   set the value of the Matrix - set
## 2:   get the value of the Matrix - get
## 3:   set the value of the Inverse of matrix - getinv
## 4:   get the value of the Inverse of matrix - setinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solvedMatrix) inv <<- solvedMatrix
  getinv <- function() inv
  list(
    set = set,
    get = get,
    getinv = getinv,
    setinv = setinv)
}


## Function calculates inverse of matrix object retuned by previous finction.
# If inverse is calculated previously then result is returned from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("Inverese retrieved from cache.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
