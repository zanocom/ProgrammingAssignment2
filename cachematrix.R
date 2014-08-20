## Programming Assignment 2 



## makeCacheMAtrix create an object that is a list of functions 
## It stores the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m_inv <- NULL
  
  set <- function(y) {
    x <<- y 
    m_inv <<- NULL
  }
  
  get <- function() x
  setinvmat <- function(inv_mat) m_inv <<- inv_mat # assign a value to an object in an environment 
                                       # that is different from the current environment
  getinvmat <- function() m_inv
  
  # returned list of attributes
  list(set = set, 
       get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
  
}


## This function calculates the inverse of makeCacheMatrix
## only if inverse matrix does not exist, otherwise it gets cached data

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmat()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  original_mat <- x$get()
  m <- solve(original_mat)
  x$setinvmat(m)
  
  m
}
