## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

#makeCacheMatrix holds the setter/getter methods for the matrix that needs to be placed in 
#cache. if the matrix is available in cache, get & getInverseMatrix are called.
# if the matrix is not available in cache, set & setInverseMatrix are called to set the matrix
#in cache.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
# cacheSolve function reads the matrix given as input, and looks in cache if the inverse already
#exists. If the inverse exits, it is retrieved.
#else the inverse is calculated and placed in cache. Output of the inverse is displayed.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat<-x$get()
  m<-solve(mat, ...)
  x$setmatrix(m)
  m
}