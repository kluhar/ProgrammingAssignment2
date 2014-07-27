## A pair of function to find and cache the inverse of a matrix, If inverse
## of matrix is already in cache returns the cached value

## Function to cache the inverse of matrix

makeCacheMatrix <- function(m = matrix()) {
        
        im<-NULL
        set<-function(mat){
                m <<- mat
                im <<- NULL
        }
        get<-function() m
        setInverse<-function(solve) im<<- solve
        getInverse<-function() im
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function to find the inverse of matrix returned by 'makeCacheMatrix'
## Returns the cached value if solution is calculated previously otherwise 
## calculate and return the inverse of matrix and cache the value

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        
        ## returns the inverse of matrix if cached
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mat<-x$get()
        m<-solve(mat, ...)
        x$setInverse(m)
        m
}
