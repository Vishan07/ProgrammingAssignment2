#Answer for assignment two of 'R Programming'
#This answer uses the example as given as a basis
#The MakeCacheMatrix creates the matrix that can cache its inverse
#The cacheSolve function computes the inverse of the matrix returned by MakeCacheMatrix

MakeCacheMatrix <- function(x = numeric()) {
  #initially nothing is cached, so this one is NULL
  cache <- NULL
  #store the matrix
  setMatrix <- function(y) {
    x <<- y
    #The matrix is assigned a new value, so the cache is set to NULL
    cache <<- NULL
  }
  #Get the matrix returned
  getMatrix <- function() {
    x
  }
  #Cache it
  setinverse <- function(solve) {
    cache <<- solve
  }
  #Get the cached value
  getinverse <- function() {
    cache
  }
  #return a list 
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse,getinverse = getinverse)
}


cachesolve <- function(x, ...) {
  #get the cached value
  inverse <- x$getinverse()
  #check whether cached value exists
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  #If there exists no cached data, get the matrix, 
  #calculate the inverse and store it in the cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$setinverse(inverse)
  #return the inverse
  inverse
}
