#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
#rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #gest the matrix
  get <- function() x
  
  #set and get the inverse
  setinv = function(solve) m <<- solve
  getinv = function() m
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  
  m<-x$getinv()
  
  #if inverse has been caculated
  if(!is.null(m)){
    #get from cache
    message("getting cached data")
    return(m)
  }
  
  # if not, calculate the inverse
  matrix<-x$get()
  m<-solve(matrix, ...)
  
  #set the value in the cache
  x$setinv(m)
  return(m)
}

