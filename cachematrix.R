## After understanding what's going on in the example code, I follow the same pattern

## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    set.inverse<-function(solve) inv<<-solve
    get.inverse<-function() inv
    list(set=set,get=get,set.inverse=set.inverse,get.inverse=get.inverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$get.inverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$set.inverse(inv)
    inv
}

