## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

inv<-NULL

  set <-function(y){
    x<<-y
    inv<<-NULL
 
  }
  get<-function()x
  setinv<-function(solve) inv<<-solve  ## function determines inverse of matrix using solve
  getinv<-function() inv
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)

}

## This function computes the inverse of a matrix returned  bymakeCacheMatrix
cacheSolve <- function(x, ...) {

  inv<-x$getinv()
  if(!is.null(inv)){
      message("getting cached data") ## if matrix inverse has already been calculated and is already cached, a message is given
      return(inv) ## if inverse of matrix was previously caculated, function returns cached minverse
  }
  data <- x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv


}
