## Lam Vo
## 06/18/17


## makeCacheMatrix creates a list that contain
##            a function that set value of a matrix, get value 
##            from the vector, set the value of the inverse, and
##            get the value of the inverse

makeCacheMatrix <- function( x= matrix() ) {
  m <- NULL 
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  getmatrix <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set=set,getmatrix=getmatrix,
       setinv=setinv, getinv=getinv)
}


## cacheSolve returns the inverse matrix if the inverse
##            is not calculated; if the inverse is already calculated,
##            the function would cache the inverse.


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data,...)
  x$setinv(m)
  m
}
