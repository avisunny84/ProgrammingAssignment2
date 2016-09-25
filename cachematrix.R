## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function 
## makeCacheMatix funtion takes matix x as an input and stores its value in cache   

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL 
  set <- function(y){
  x <<- y
  invrs <<- NULL 
  }
  
  get <- function() x 
  
  setinverse <- function(minverse) invrs <<- minverse
  getinverse <- function() invrs
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
##cacheSolve funtion first check if the matrice is available in in cache and returns inverse otherwise returns inverse of
## matrix x    

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 invrs <- x$getinverse()
      if(!is.null(invrs)){
        message(" gathering cached data")
        return(invrs)
      }
     
     data <- x$get()
     invrs <- solve(data)
     x$setinverse(invrs)
     invrs

}
