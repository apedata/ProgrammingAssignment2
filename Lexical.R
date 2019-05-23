
##Functions which cache the inverse of a matrix 

##Following function Creates a special matrix that can cache
##it's inverse for input
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- null 
  }
  get <- function() x
  setinv <- function(inverse) inv <- inverse
  getinv <- function() inv
  list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## Following function computes inverse of special matrix
##returned by makeCachematrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv 
  
}
