#   this file is script containing functions for efficient computing of
#   matrix inverses. Since computing the inverse of a matrix is
#   computationally expensive (especially for large matrices) the functions
#   defined here cache the inverse of a given matrix. Anytime the inverse of
#   that particular matrix is needed it is taken from the cache. The inverse is
#   only computed for new matrices (i.e., matrices other than the one stored in
#   the special matrix object. 
#   And all the completed code is completed by imitating the introduction.

makeCacheMatrix <- function(x = matrix()) {  # This function creates a special "matrix"
    m<- NULL                                 # which is really a list containing a
  set <- function(y) {                       # function to set the matrix
          x <<- y                             
          m <<- NULL
  }
  get <- function() x                        # get the matrix
  setinverse <- function(inverse) m <<- inverse  # set the inverse matrix
  getinverse <- function() m                     # get the inverse matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#   The first fuction splits the data to 4 sections(set,get,setinverse,getinverse) 
    to store the inverse matrix, and for getting the unchanged inverse matrix we can 
    directly read the data stored in the makeCacheMatrix without useless repeated calculation.
cacheSolve <- function(x, ...) {       # This function calculates the inverse matrix created by
  m <- x$getinverse()                  # above function 
  if (!is.null(m)) {                   # check if the inverse matrix has been calculated, so get it 
          message("getting cached data")
          return(m)
  }
  data <- x$get()                      # otherwise it calculates inverse matrix created by above function and 
  m <- solve(data, ...)                # set it
  x$setinverse(m)
  m
}
