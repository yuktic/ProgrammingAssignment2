## The functions below return the inverse of a matrix. But, if the inverse has been calculated before
## they return the cached value of the matrix inverse, and skip the re-calculation of the matrix inverse.
## makeCacheMatrix() creates a special object (a list) that stores a matrix and cache's its inverse.
## cacheSolve() checks if inverse has been calculated and either returns the cached inverse or calculates the 
## inverse and returns it

## makeCacheMatrix() is a function that takes as argument a matrix, stores the matrix, and caches its inverse
## It returns a list which contains functions which do four things: 
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the matrix inverse and
## 4. gets the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {

  i<-NULL ## Creates a placeholder matrix for a future matrix inverse
  
  ## set is a function that sets the matrix 'x' to a new matrix 'y' and resets the inverse matrix 'i' to NA
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
 
  get <- function() x   ## Function that returns the matrix 'x'
  setinverse <- function(inverse) i <<- inverse  ## Function that set the inverse matrix 'i' to inverse
  getinverse <- function() i  ## Function that returns the inverse matrix 'i' 
  
  ## This returns the list containing the functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}    


## cacheSolve is a function that takes as argument the list returned by makeCacheMatrix() and returns the matrix
## inverse 

cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()  ## Gets the matrix inverse stored in the list 'x' and stores it in 'i'
  
  ## Checks if 'i' already contains the matrix inverse, if yes, it returns 'i' and exits the function
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If 'i' does not contain inverse, it is calculated for the matrix data, matrix inverse is stored and returned
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
  ## Return a matrix that is the inverse of 'x'
  
  }
  
  
  

