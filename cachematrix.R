## These two functions just mimic the example function "makeVector",
## you can use "makeCacheMatrix" to build a "matrix object" which can carry 
## its inversion. At first, its inversion is NULL. When you use "cacheSolve" to
## the matrix object for the first time, its inversion will be actually computed 
## and stored in the matrix object. When you use "cacheSolve" for the second time,
## its inversion will not be computed and will be retrived from the object since
## the inversion has been cached.

## This function just like a "construction function" in OOP.It takes a matrix object 
## as argument, and return a "matrix object" which can cache its inversion.
## The returned "matrix object" is actually a list, and has several functions as
## its elements.

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) mat_inv <<- inverse
  get_inverse <- function() mat_inv
  list(set = set, get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## This function takes a "matrix object" build by "makeCacheMatrix".
## If the "matrix object" has cached inversion, it will simply return it.
## If the "matrix object" hasn't cached inversion, it will actually compute 
## it and store the inversion in the "matrix object".
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$get_inverse()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- solve(data)
  x$set_inverse(mat_inv)
  mat_inv
}
