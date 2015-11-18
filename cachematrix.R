## The following functions can compute and cache the inverse of a matrix. The goal is to avoid 
##  repeatedly running a time consuming computations.


## This function  creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        in_Verse <- NULL
        set <- function(y){
                x <<- y
                in_Verse <<- NULL
                
        }
        get <- function() x
        setin_Verse <- function(inverse) in_Verse <<- inverse
        getin_Verse <- function() in_Verse
        list(set = set, get = get,
             setin_Verse = setin_Verse,
             getin_Verse = getin_Verse)

}


## This function computes the inverse of the special "matrix" returned by the first function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        in_Verse <- x$getin_Verse()
        if(!is.null(in_Verse)){
                message("getting cached data")
                return(in_Verse)
        }
        data <- x$get()
        in_Verse <- solve(data, ...)
        x$setin_Verse(in_Verse)
        
        in_Verse
}
