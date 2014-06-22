## The overall goal of this program is to create our own special matrix
## and use cacheing to calcualte its inverse 

## Example: 
## m <- matrix (rnorm(16), nrow = 4)
## sm <- makeCacheMatrix(m)
## sm$get()
## cacheSolve (sm)
## cacheSolve (sm)
## cacheSolve(specialMatrix)
## ...retrieving cache     ## when called second time, it retrives 
## from cache

makeCacheMatrix <- function(x = matrix()) {
  
  #create empty matrix to store cache
  inverse <-NULL
  
  #setter for the matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  #getter for the matrix
  get <- function () x
  
  #setting inverse of the matrix
  setInverse <- function(inv) inverse <<-inv
  
  #getting inverse of the matrix
  getInverse <- function() inverse
  
  #return the matrix and the new functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix. If the inverse is already calculated, it will return its cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse ()
  
  #if the inverse already calcualted, return it
  if (!is.null(inverse)){
    
    message ("...retrieving cache")
    return (inverse)
  }
  
  #if not calcualted, calculate the inverse
  data <-x$get()
  inverse <- solve (data, ...)
  
  #cache the inverse here
  x$setInverse (inverse)
  
  #return the inverse
  inverse
}