## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## The following two functions support the caching of the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.


## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## The following two functions support the caching of the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  
  matrixinverse <- NULL                     
  
  set <- function(y) {                      
    x <<- y
    matrixinverse <<- NULL              
  }
  
  get <- function() x                           
  
  setinverse <- function(solve) matrixinverse <<- solve 
  
  getinverse <- function() matrixinverse        
  
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {   
  ## Return a matrix that is the inverse of 'x'              
  matrixinverse <- x$getinverse()
  
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  
  data <- x$get()                               
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
