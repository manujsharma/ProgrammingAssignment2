# The function makeCacheMatrix stores a martix and a cached value of the inverse of the 
# matrix. 
# There are 4 sub functions in this function
# setMatrix:  Initialize the Matrix
# getMatrix:  Retrive the Matrix
# setInverse: Initilaize the Inverse
# getInverse: Retrieve the Inverse

makeCacheMatrix <- function(x = matrix()) {
  
# cache_val holds the cached value or NULL if nothing has been cached yet
# initialize to NULL  
          cache_val <- NULL
  
# Save the Matrix to be cached
          setMatrix <- function(new_val) {
          x <<- new_val
          cache_val <<- NULL
          }
  
# Return saved Matrix
          getMatrix <- function() {
            x
          }
  
# Set value of Inverse
          setInverse <- function(inverse){ 
                cache_val <<- inverse
    
          }
# Retrive cached value
          getInverse <- function() {
                cache_val
          }
# Return List of the values of the functions
          list(setMatrix = setMatrix, getMatrix = getMatrix,
              setInverse = setInverse,
              getInverse = getInverse)
}


# The function calcSolve calculates the inverse of a matrix created using makecacheMatrix

cacheSolve <- function(x, ...) {
# Retrieve cached value of the Inverse
          Inverse <- x$getInverse()
  
# Check if Inverse is not null and retrive cached value without calculating Inverse
          if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
          }
  
# If Inverse is null, Retrive the value of the matrix, calculate its Inverse and save 
# the Inverse in the cache
          data <- x$getMatrix()
          Inverse <- solve(data)
          x$setInverse(Inverse)

# Return the Inverse        
          Inverse
}