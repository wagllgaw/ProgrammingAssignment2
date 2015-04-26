## THis is the solution to programming assignment 2 on Caching the inverse of a matrix
## Functions:
## makeCacheMatrix: creates a list of functions for getting/setting mean
## cacheSolve: finds the inverse of an input x

## MakeCacheMatrix does returns a list that contains the matrix and functions that access it
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() 
  {
    x
  }
  setInverse <- function(inverse)
  {
    inv <<- inverse
  }
  getInverse <- function()
  {
    inv
  }

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## CacheSolve inputs a list of functions that get/set inverses, it checks the object to see if the
## inverse has been calculated and if not, does the calculation returning the inverse
cacheSolve <- function(y, ...) 
{
  m <- y$getInverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data <- y$get()
  m <- solve(data,...)
  
  y$setInverse(m)
  m
}
