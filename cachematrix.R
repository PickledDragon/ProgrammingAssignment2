## Put comments here that give an overall description of what your
## functions do

## This function will create an enhanced matrix with capability to cache its inverse

#' Create a new matrix type with builing cache for inverse
#' @param x A matrix
#' @return A matrix with cache capability
makeCacheMatrix <- function(x = matrix()) {
  
  mxInstance <- NULL
  
  set <- function(paramMatrix)
  {
    x<<-paramMatrix
    mxInstance<<- NULL
  }
  
  get <- function() x
  
  setinverse<- function(inverse) { mxInstance <<- inverse 
                                   message("Inverse added in cache")
  }
  getinverse<- function() mxInstance
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Enhances Solver function with cache capability. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mxInstance<-x$getinverse()
  
  if(!is.null(mxInstance))
  {
    message("Cache hit - return from cache")
    return(mxInstance)
    
  }
  
  srcMat <- x$get()
  mxInstance<-solve(srcMat) #Solver function will generate inverse of the MX
  x$setinverse(mxInstance) #Push the result to cache
  return (mxInstance)
}


#------------------------------------
# Test fixture - Uncomment 
#------------------------------------
