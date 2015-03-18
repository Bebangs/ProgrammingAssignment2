##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
 
makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
      x <<- y
      mtrx <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(solve) mtrx <<- solve
  getmatrix <- function() mtrx
  
  list (
    set = set,
    get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix)
  
  
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mtrx <- x$getmatrix()
  
    if(!is.null(mtrx)){
        message("getting from cache")
        return (mtrx)
    }
    
    data <- x$get()
    mtrx <- solve (data, ...)
    x$setmatrix(mtrx)
    
    mtrx
    
  
}
