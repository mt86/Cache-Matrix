## Creating a special "matrix" object that caches its inverse 
##and then computing the inverse of that special "matrix" object

##Creating a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        invert<-null
        set<-function(y){
                x<<-y
                invert<<-null
        }
        get<-function()x
        set_innverse<-function(inverse)invert<<-inverse
        get_inverse<-function()invert
        list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## calculating the special matrix created above

cacheSolve <- function(x, ...) {
        inverse<-x$get_inverse()
        if(!is.null(invert)){
                message("getting cached data")
                return(invert)
        }
        matrix<-x$get()
        invert<-solve(matrix,...)
        x$set_inverse(invert)
        invert
}
