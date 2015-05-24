## all code is commented, almost line by line to explain what is happening. i think its very simple and straightforward

makeCacheMatrix <- function(x = matrix()) {
        ## @x: invertible and square matrix
        ## return: functions to get/set the matrix and its inverse.
        ## use: input of cacheSolve() function            
        
        inv_matrix = NULL
        set_matrix = function(y) {
                # operator `<<-` assigns a value to an object in a different environment 
                
                x <<- y
                inv_matrix <<- NULL
        }
        get_matrix = function() x
        setinv = function(inverse) inv_matrix <<- inverse 
        getinv = function() inv_matrix
        list(set_matrix=set_matrix, get_matrix=get_matrix, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## @x: makeCacheMatrix()'s output
        ## return: original matrix input's inverse
        
        inv_matrix = x$getinv()
        
        # given the inverse has been calculated
        if (!is.null(inv_matrix)){
                # get_matrix it from the cache. 
                message("getting cached data")
                return(inv_matrix)
        }
        
        # calculates the inverse only if needed
        mat.data = x$get_matrix()
        inv_matrix = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv_matrix)
        
        return(inv_matrix)
}