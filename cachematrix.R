## makeCachMatrix creates a special matrix that can cach the inverse of the input matrix
## cachSolve calculates the inverse of the input matrix in makecachMatrix

## makeCachMatrix creates variable, in preparation of the later cachSolve function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function()m
        list(set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse)  

}


## cachSolve calculates the inverse of the input in the makeCachMatrix function

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
