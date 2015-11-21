## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#cacheMatrix <- makeCacheMatrix(matrix) will create the function
#cacheSolve(cacheMatrix) will take the inverse if not exist
#cacheMatrix$set(matrix)  will initialize the inverse of the matrix    
#cacheMatrix$get()  will show the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cInverse <- NULL #begins by setting the mean to NULL as a placeholder for a future value
  set <- function(y) {
    x <<- y
    cInverse <<- NULL
  } #defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, m, to NULL
  get <- function() x #returns the matrix x
  setInverse <- function(inverse) cInverse <<- inverse #sets cInverse to inverse
  getInverse <- function() cInverse #returns cInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) #returns a vector of the functions that we just defined
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse() #gets cInverse
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  } #if the inverse exists return it and stop "return()", otherwise proceed 
  data <- x$get() #get the matrix
  invFunc <- solve(data, ...) #inverse the matrix
  x$setInverse(invFunc) #pass the inverse of the matrix to cInverse
  invFunc #return the inverted matrix
}
