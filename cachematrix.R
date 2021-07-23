##Well I did nothing but just replaced the variables of the original cryptic
##code with new variables required for matrix inversion
##Before-->After
##m-->the_inv
##setmean-->set_inv
##getmean-->get_inv
##mean-->matinv
##
makeCacheMatrix <- function(x = matrix()) {
  the_inv <- NULL
  set <- function(y) {
    x <<- y
    the_inv <<- NULL
  }
  get <- function() x
  set_inv <- function(matinv) the_inv <<- matinv
  get_inv <- function() the_inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  the_inv <- x$getinv()
  if(!is.null(the_inv)) {
    message("getting cached data")
    return(the_inv)
  }
  data <- x$get()
  the_inv <- solve(data)
  x$set_inv(the_inv)
  the_inv
}
##Have a good day!!