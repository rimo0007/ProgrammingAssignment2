## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  
  #If anything is in the cache then hold that value otherwise hold NULL for the first time
  #Because first time cache will be NULL.
  cachevalue <- NULL
  
  # Set the value of the Matrix
  setMatrix <- function(val){
    
    x <<- val
    cachevalue  <<- NULL
  
  }
  
  # GEt the Value of the Matrix
  getMatrix <- function() x
  
  #Put the value into the cache
  putChache <- function(solve) {
    cachevalue <<- solve 
  }
  
  # Get the Cache value the 
  getCacheValue <- function() cachevalue   
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, putChache = putChache, getCacheValue = getCacheValue)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Get the cahce value
  inverse <- x$getCacheValue()
  
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #If cache is empty then calculate the inverse and put the value into the cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$putChache(inverse)

  # return the inverse
  inverse
  
}
