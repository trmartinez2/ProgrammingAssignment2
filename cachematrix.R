## Creating an inverse matrix and caching it for future use

## Creating an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  
  # Setting matrix values
  set <- function(y){
    x <<- y
    mat <<- NULL
  }
  
  # Fetching the matrix values
  get <- function(x)
  
  # Setting inverse matrix values  
  setmat <- function(solve) mat <<- solve
  
  # Fetching the inverse matrix values
  getmat <- function() mat
    
  list(set = set, get = get,
    setmat = setmat,
    getmat = getmat)
}
  

## Computing the result from 'makeCacheMatrix' to determine to either use the cached inverse or to create a new inverse
## The orginal martix "x" is assumed to be invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mat <- x$getmat()
  
  # Fetching cached inverse, if applicable
  if(!is.null(mat)){
    message("Getting cached data")
    return(mat)}
 
  # Calculating inverse, if not already cached
  data <- x$get()
  mat <- solve(data, ...)
  x$setmat(mat)
  mat
}
