## This function will create an enhanced matrix with capability to cache its inverse

#' Create a new matrix type with builing cache for inverse
#' @param x A matrix
#' @return A matrix with cache capability
makeCacheMatrix <- function(x = matrix()) {
  
  mxCache <- NULL
  
  
  #Generic getter/setter methods for the enhanced Matrix type
  set <- function(paramMatrix)
  {
    x<<-paramMatrix
    mxCache<<- NULL
  }
  
  get <- function() x
  
  
  #Set inverse and get inverse functions to facilitate access to inverse functionality in the extended Matrix type
  setinverse<- function(inverse) { mxCache <<- inverse 
                                   message("Inverse added in cache")
  }
  getinverse<- function() mxCache
  
  #Exposes methods to the external world
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Enhances Solver function with cache capability
#' Extends 'solve' function with cache capability
#' @param x A matrix created with 'makeCacheMatrix' function. Please note primitive matrices will not work with thos function
#' @return Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  #Attempt retrieval of inverse from the matrix object
  mxCache<-x$getinverse()
  
  #check if we're succesful in retrieving the inverse
  if(!is.null(mxCache))
  {
    #Found the cached inverse. Return the inverse
    message("Cache hit - return from cache")
    return(mxCache)
    
  }
  
  #We're hit by a cache miss. Needs to initialize cache
  #--
  #Retrieve original matrix from the extended matrix parameter
  srcMat <- x$get()
  
  #Generate inverse of the matrix - with solve funcion
  mxCache<-solve(srcMat) #Solver function will generate inverse of the MX
  
  x$setinverse(mxCache) #Push the result to cache
  return (mxCache) # Retrun the inverse
}