
## makeCacheMatrix is a type of "closure" function that will 
## take a matrix as an argument, search for a value for "i", 
## the inverse of that matrix, and if not present, will set it as NULL
## The double assignment symbol allows the internal functions to assign values
## to the parent function "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
            x <<- y
            i <<- NULL
        }
    get  <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix provided
## but first checks to see if there is an "i" , then delivers the 
## inverse of the matrix using solve()

cacheSolve <- function(x, ...) {
     i  <- x$getinverse()
     if(!is.null(i)) {
         message("getting cached data")
         return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setinverse(i)
     i
}
