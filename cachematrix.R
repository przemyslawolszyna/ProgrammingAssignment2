#This function store calculated inverted matrix - steInvertedMatrix and 
#allowes to use it later on without recalculation from storage by calling getInvertedMatrix

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



## This function calculate inverted Matrix by calling getInvertedMatrix

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
