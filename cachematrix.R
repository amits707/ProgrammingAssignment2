
## makeCacheMatrix function takes a special "matrix" object as input 
## calulates the inverse of the input matrix and stores (cache) it
 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}		  


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve retrieves the inverse from the cache
## if not in cached then it calculates the inverse and sets it in the special martix 
## object and returns the inverse matrix
   
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'		          
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached result")
        return(inv)
    }else{
        message("NOT cached")
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## ---------------Checking the program------------------------
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
## first time "NOT cached"
cached <- cacheSolve(m1)
cached

## second time "getting cached result"
cached <- cacheSolve(m1)
cached