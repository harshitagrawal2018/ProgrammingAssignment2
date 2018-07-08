## makeChacheMatrix creates a special "matrix", which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of matrix  
##get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                                ## m is a null matrix which will contain inverse of martix "x"
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(inverse) 
    m <<- inverse
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}



##cacheSolve is a function which computes the inverse of the special "matrix" 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setm(m)
  m
}
