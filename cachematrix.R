## Put comments here that give an overall description of what your
## functions do


#--------------------------------------------------------------------------------
# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL                                         # m : The Inverse of the Matrix
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                              # Returns the Matrix (x)
  setinv <- function(inv) m <<- inv                # Set the Inverse value as m
  getinv <- function() m                           # Returns the Matrix's inverse: m
  list(set = set, get = get,                       # Out put of the function as alist of other required functions
       setinv = setinv,
       getinv = getinv)
  
}


#--------------------------------------------------------------------------------
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data)
  x$setinv(m)
  m
       
}


#--------------   Define the Result Function  ------------
result<-function(x){
L<-makeCacheMatrix(x)
cacheSolve(L)
}
#----------------   Define the Result Function   ---
B <- matrix(c(1,2,3,4),2,2)
#B = matrix(c(4, 2, 7, 6), nrow=2, ncol=2)
#B = matrix(c(3,2,0,0,0,1,2,-2,1), nrow=3, ncol=3)
result(B)





