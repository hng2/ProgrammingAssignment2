## The following functions compute the inverse of a given matrix (we assume the
## input matrix is invertible). The first function caches the input matrix and 
## its computed inverse. In cases where the same matrix is input again, the cached 
## values are retrieved by the second function without having to compute the 
## inverse of the same matrix again, thereby saving computation time.


## this function caches and retrives the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        }
        setinv <- function(inverse){
                inv <<- inverse
        }
        getinv <- function(){
                inv
        }
        
        list(set=set, get=get, getinv=getinv, setinv=setinv)

}


## This function compares the input matrix to a previous input matrix (if any).
## If they are the same it retrives the inverse from the cached variable by
## calling the previous function, and computes the inverse, otherwise. It also
## stores the input matrix and its inverse in cache by calling the previous 
## function.

cacheSolve <- function(x, mat) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        prevmat <- x$get()
        x$set(mat)
        
        ## compare current matrix and previous input matrix
        sameDim <- all(dim(mat) == dim(prevmat))
        if(sameDim){
                ## compare values cell by cell
                changed = !all(apply(mat==prevmat, 2, all))
        } else {
                changed = TRUE
        }
        
        if(!is.null(inv) && !changed){
                message('\ngetting cached data ...')
                return(inv)
        }
        
        inv <- solve(mat)
        x$setinv(inv)
        inv
        
}
