## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
 
### The first function will create a list of that functions 
### in an own environment (different from Global Environment):
###             "set" 
###             "get" 
###             "setimatrix" 
###             "getimatrix"
### and the objects: 
###             "im" 
###             "x"

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setimatrix <- function(solve) im <<- solve 
        getimatrix <- function() im   
        list(set = set, get = get,
             setimatrix = setimatrix,
             getimatrix = getimatrix)
}


## Write a short comment describing this function

### This function looks after the cached inverse matrix, 
### and returns "im" if it exists (and the function ends!)  
### If there is nothing cached "im" will be created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getimatrix() 
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...) 
        x$setimatrix(im)  
        im
}


