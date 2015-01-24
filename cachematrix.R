## The functions below calculate the inverse of a matrix and save the calculation
## results to the cache. It creates a list with the following functions: set, get, setinverse, getinverse.
## Create a matrix object x and define the cache m  
makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <- NULL
  }
  get <- function() x ## return to the matrix x 
  setinverse <- function(inverse)m ## set the cache m identical to the inverse of the matrix x  
  getinverse <-function()m ## return to the cached inverse of x
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## Compute the inverse of the special matrix
cacheSolve <- function(x,...){ 
## Return to the matrix inverse of x  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
## Compute the inverse of a square matrix with the solved function
x <- matrix(c(3,1,6,5), nrow=2, ncol=2)
y <- makeCacheMatrix(x)
z <- cacheSolve(y, x)
print(z)
m = x %*% z
print(m)
y$set(matrix(c(2,1,6,5), nrow=2, ncol=2))
z <- cacheSolve(y, x)
print(z)
m = y$get() %*% z
print(m)