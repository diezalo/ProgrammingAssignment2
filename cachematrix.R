## The functions bellow create a special object that is able to store a given matrix and cache its inverse.

## This function creates a special matrix that can cache its inverse.
makeCacheMat <- function(x=matrix(), ...) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #create function to calculate the inverse
  getinverse <- function() m #create function to store the inverse
  list(set = set, get = get, #display the four created elements as a list
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special matrix created with the function above
cacheSolve <- function(x, ...) {
  m <- x$getinverse() # create local object 'm' that contains the inverse calculated with the previous function
  #If 'm' is not empty, give the message "getting cached data" and show 'm'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() #create object data to store the matrix created in the function 'makeCacheMatrix'
  m <- solve(data, ...) # store the inverse of the function cached in the previous line, as 'm'
  x$setinverse(m) #display the function setinverse coming from makeCacheMatrix
  m #display the cached inverse of the original matrix
  
}
