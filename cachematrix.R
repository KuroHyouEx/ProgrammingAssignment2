# Used to calculate the inverse of a quadratic matrix and cache the inverse
# if it was calculated second time in a row
# Use:
#     var <- makeCacheMatrix()
#     var$set(`insert a matrix`)
#     cacheSolve(var)


# For caching optimized matrix
makeCacheMatrix <- function(matrix = matrix()) 
{
      # Initialize matrix
      cachedMatrix <- NULL
      
      # Gives the optimized matrix a new value
      # and undo the cache-status
      # The value of the optimized value is a normal matrix
      set <- function(m){
            matrix <<- m
            cachedMatrix <<- NULL
      }
      # Return the value of the optimized matrix
      get <- function(){
            matrix
      }
      
      # Caches the inverse of the matrix
      setInverse <- function(inverse){
            cachedMatrix <<- inverse
      }
      # Return the cached inverse of the matrix
      getInverse <- function(){
            cachedMatrix
      }
      
      # Return the list of functions from this function
      list(set = set, 
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

# Calculate the inverse of a matrix and uses a optimized matrix
# from makeCacheMatrix(), with it help the inverse can be cached
cacheSolve <- function(x, ...) 
{
      # Get the cached inverse
      inverse <- x$getInverse()
      
      # If the inverse is already in the cache, return it
      if (!is.null(inverse))
      {
            print("Cached version.")
            return(inverse)
      }
      # else calculate it, cache it and return it
      else
      {
            matrix <- x$get()
            inverse <- solve(matrix)
            x$setInverse(inverse)
            inverse
      }
}
