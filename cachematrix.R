## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  x <<- NULL
  
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  
  setInvertedMatrix <- function(invertedMatrix) x <<- invertedMatrix
  
  getInvertedMatrix <- function() z
  
  list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  im <- x$getInvertedMatrix()
  if(!is.null(im)) {
    message("cached data returned")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInvertedMatrix(im)
  im
  
}
