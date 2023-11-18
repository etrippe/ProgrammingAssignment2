## Overall Description: these functions cache the inverse 
## of a matrix and then calculate or cache the result 
## for a new matrix

## example usage
# x <- rbind( c(2,7), c(2,8))
# # create a makeCacheMatrix object
# x_cache <- makeCacheMatrix(x)
# # find the inverse of the matrix and cache result
# x_cache_solve <- cacheSolve(x_cache)
# # display result
# x_cache_solve
## [,1] [,2]
## [1,]  4.0   -1
## [2,] -3.5    1
# # find the inverse again (uses cached result)
# x_cache_solve_1 <- cacheSolve(x_cache)
## getting cached inverse . . .
# x_cache_solve_1
## [,1] [,2]
## [1,]  4.0   -1
## [2,] -3.5    1

## makeCacheMatrix

## creates a makeCacheMatrix object(list), which can be
## used with cacheSolve to calculate/cache the inverse of a
## matrix
## Arguments:
##  x: an invertable matrix
## Functions:
##  get: returns the input matrix arguement
##  set: stores the input matrix arguement
##  getinv: retrieves the inverse
##  setinv: stores the inverse
## Return value: 
##  list, a makeCacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
        
  inv <- NULL
  
  set <- function(y){
              x <<- y
              inv <<- NULL
            }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve

## uses a makeCacheMatrix object to solve/retrieve the 
## inverse of a matrix
## Arguments:
##  x: a makeCacheMatrix object
## Return value: 
##    inv: matrix
##      - the inverse of the matrix, x, from makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  inv <- x$getinv() # find the cached inverse
  
  if(!is.null(inv)){ # check if the cached inverse exists
    message("getting cached inverse . . .")
    return(inv) # return the cached inverse
  } else{
  
    data <- x$get()
    
    inv <- solve(data) # find the inverse
    
    x$setinv(inv) #caches the inverse
    }
  
  inv # returns inverse matrix
}
