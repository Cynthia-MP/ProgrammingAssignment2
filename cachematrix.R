## This file contains a pair of functions (makeCacheMatrix & cacheSolve) 
##  that create and cache the inverse of a matrix: 



## 1. makeCacheMatrix: This function creates a special "matrix" object 
##    that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
# Set and initialize a variable that will store the invers of the matrix
  m <- NULL

##  set the value of the matrix
 set <- function(y){
    x <<- y
    m <<- NULL
  }
  
##  get the value of the matrix
get <- function() x 
  
##  set the value of the inversed matrix
  
setinverse <- function(inverse) m <<- inverse
##  get the value of the inversed matrix
  
getinverse <- function() m
 
 list(set = set, get = get, 
  setinverse = setinverse,
  getinverse = getinverse)
    
}


## cacheSolve: This function computes the inverse of the special 
##    "matrix" returned by makeCacheMatrix above. If the inverse 
##     has already been calculated (and the matrix has not changed), 
##     then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  
## Store the value of the matrix in a variable
  m <- x$getinverse()
  
## Check to see if the inverse has alreay been calculated
    if(!is.null(m)){
      message(" getting cached data")
      return(m)
    }
## Set the inverse if it has not already been set
  data<- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  return(m)
}
