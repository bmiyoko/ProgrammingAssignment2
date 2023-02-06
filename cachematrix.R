## functions makeCacheMatrix and cacheSolve

makeCacheMatrix <- function(x = matrix()) { # creates a special "matrix" object that can cache its inverse
  inver <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function()inver
  setmean <- function(inverse)inver <<- inverse
  getmean <- function()inver
  list(set-set, get=get, setmean=setmean, getmean=getmean)
}


cacheSolve <- function(x, ...) {
  inver <- x$getmean()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  new_matrix <- x$get()
  inver <- solve(new_matrix, ...)
  x$setinverse(inver)
  inver
}
}
