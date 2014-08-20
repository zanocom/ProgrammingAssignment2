## Programming Assignment 2 



## makeCacheMAtrix create an object that is a list of functions 
## It stores the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m_inv <- NULL      # initialize the inverse to NULL
  
  set <- function(y) {    
    x <<- y 
    m_inv <<- NULL
  }
  
  get <- function() x  # reads original matrix
  setinvmat <- function(inv_mat) m_inv <<- inv_mat # assign a value to inverse matrix in an environment 
                                       # that is different from the current environment
  getinvmat <- function() m_inv   # reads the inverse of makeCahceMatrix object
  
  # returned list of attributes
  list(set = set, 
       get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
  
}



cacheSolve <- function(x, ...) {
  ## This function calculates the inverse of makeCacheMatrix
  ## only if inverse matrix does not exist, otherwise it gets cached data
  
  m <- x$getinvmat()   # gets invers matrix
  
  if(!is.null(m)) {   # check if inverse is already cached
    message("getting cached data")
    return(m)
  }
  
  original_mat <- x$get()   # calulates inverse matricìx if it is not cached
  m <- solve(original_mat)
  x$setinvmat(m)    # assign calulcated inverse matrix ti makeCacheMatrix object
  
  m               # return inverse matrix
}
