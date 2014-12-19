## The following function (makeCacheMatrix) creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix())  {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get, 
       setmatrix=setmatrix, getmatrix=getmatrix)
}



## The inverse of the matrix created in the previous code will be computed in the following function.

cacheSolve <- function(x=matrix(),...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
