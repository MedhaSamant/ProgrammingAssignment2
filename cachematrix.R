makeCacheMatrix <- function(x = matrix()) {
  ##This function creates a special "matrix" object that can cache its inverse.
  m <- NULL
  
  set <- function(y = matrix()) {
    ## set the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                                      ## get the matrix
  setInverse <- function(inverse = matrix()) m <<- inverse ## set inverse
  getInverse <-function() m                                ## get inverse
  
  list(set = set, get = get, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  savedInv<-x$getInverse()
  
  if(!is.null(savedInv)){  ##return saved inverse
    print("getting cached data")
    return(savedInv)
  }
  data <- x$get()
  invMatrix <- solve(data, ...) ## create inverse
  x$setInverse(invMatrix)       ## set inverse
  invMatrix
  
}
