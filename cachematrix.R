## This function/code returns the inverse of a matrix. 
##This code caches the inverse of the unchanged matrix in order to avoid the repititions of the calculations.
 

## The first function makeCahceMatrix creates a list of 4 functions:- 
## set: set the value of the matrix
## get: get the value of the matrix
## setinv: set the inverse of the matrix  and 
## getinv: get the inverse of the matrix

makeCacheMatrix<-function (x=matrix()) {
  inv<-NULL
  
set<-function(y) {
    x<<-y
    inv<<-NULL
  }
get<-function() x 
setinv<-function(inverse) inv<<-inverse
getinv<-function() inv

list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## The following function cacheSolve calculates the inverse of the special matrix created with the above function (makeCacheMatrix).
## However it checks if the inverse has already been calculated. If so, it gets the inverse from the cache and does not calculate the inverse again and gives the results from the cache.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function  	


cacheSolve<-function(x, ...) {
  
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("throwing cached results")
    return(inv)
  }
  
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
  
}
