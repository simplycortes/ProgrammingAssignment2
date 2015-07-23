## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix
## which is really a list containing a function

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        ## Here the value of the matrix is set
        set<- function(y){
          x<<-y
          m<<-NULL
        }
        ## Here the value of the matrix is retrieved
        get<- function()x
        ## Here the inverse of the matrix is set
        setinv <- function(solve) m<<-solve
        ## Here the inverse of the matrix is retrieved
        getinv <- function() m
        list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## cacheSolve calculates the inverse of the special
## matrix created in the above function.  It first,
## however, checks to see if the inverse was already
## calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinv()
        ## if the data has already been calculated, retrieve data
        if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        ##otherwise, calculate the data
        data<- x$get()
        m<-solve(data,...)
        x$setinv(m)
        m
}
  
