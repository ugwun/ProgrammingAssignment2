## Functions to cache potentially time-consuming computations of matrix inversion

## makeCacheMatrix() function is able to store a matrix and also its inverted matrix if needed

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setInverse<-function(solve) inverse<<- solve
  getInverse<-function() inverse
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## cacheSolve() attempts to return cashed inverted matrix from object makeCacheMatrix().
## If an inverted matrix is not stored, the function computes one

cacheSolve <- function(x, ...) {
  inverse<-x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix<-x$get()
  inverse<-solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
