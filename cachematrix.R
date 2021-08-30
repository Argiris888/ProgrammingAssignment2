## In this session we gonna use 2 functions ("makeCacheMatrix" , "cacheSolve")
## that cache the inverse of a matrix



## makeCacheMatrix is a function which creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    x <<- matrix
    inv <<-NULL
  }
  get<-function() {x}
  set_inverse <-function(inverse) {inv <<- inverse}
  get_inverse <- function() {inv}
  list( set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}




## cacheSolve function gives us (or is used for the calculation of) the inverse of the special "matrix" object

cacheSolve <- function(x, ... ) {
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  my_matrix <- x$get() 
  inv <- solve(my_matrix) 
  x$set_inverse(inv)
  inv
}




############## trials #################
trial_matrix <- makeCacheMatrix(matrix (1:4, nrow = 2, ncol = 2, byrow = T))
trial_matrix$get()

trial_matrix$get_inverse()
cacheSolve(trial_matrix)
v <- cacheSolve(trial_matrix)
b <- trial_matrix$get()
v
v %*% b
