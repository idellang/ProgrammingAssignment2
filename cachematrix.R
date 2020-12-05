## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initialize null inverse
  i <- NULL
  
  #set function for setting the matrix
  set <- function(y){
    
    x <<- y
    #if a matrix is set, make the inverse NULL
    i <<- NULL
  }
  
  #set function to get matrix 
  get <- function() x
  
  ##set inverse of the matrix using an input
  set_inverse <- function(inverse) i <<- inverse
  
  
  #get the inverse
  get_inverse <- function() i
  
  #return list of functions
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # get the inverse value to m
  m <- x$get_inverse()
  
  #check if m is not null then just return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if there is no inverse get the data
  data <- x$get()
  
  #solve the inverse matrix
  m <- solve(data)
  
  #set the inverse using set_inverse
  x$set_inverse(m)
  m
  
}
