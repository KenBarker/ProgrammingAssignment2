## A matrix object is created and a list of function that can directly 
##      manipulate that matrix, including solving the inverse, is 
##      returned. Function items can be called by using subset 
##      notation (object$function).

## Creates a special "matrix" object that contains a list of functions 
##      to manipulate the matrix
makeCacheMatrix <- function( myMatrix = matrix() ) 
{
        ## Initializes the object to store the inverted matrix
        inverseMatrix <- NULL
        
        ## Sets the value of the matrix
        setMatrix <- function( thisMatrix ) 
        {
                myMatrix <<- thisMatrix
                inverseMatrix <<- NULL
        }
        
        ## Retrieves the value for the matrix
        getMatrix <- function()
                myMatrix
        
        ## Sets the value of the inverse matrix
        setInverse <- function( inverse )
                inverseMatrix <<- inverse
        
        ## Retrieves the value for the inverse matrix
        getInverse <- function()
                inverseMatrix
        
        ## Returns a list of the above functions
        return ( list( setMatrix = setMatrix,
                       getMatrix = getMatrix,
                       setInverse = setInverse, 
                       getInverse = getInverse ) )
}


## Solves the matrix for its inverse and sets the value of the inverse 
##      matrix if it is not already done
cacheSolve <- function( myObject, ... )
{
        ## Retrieves the cached value of the inverse matrix
        inverseMatrix <- myObject$getInverse()
        
        ## Checks whether the inverse matrix has actually been 
        ##      calculated
        if( !is.null( inverseMatrix ) )
        {
                ## Shows message and returns the value if the value is 
                ##      not NULL
                message( "getting cached data" )
                return( inverseMatrix )
        }
        ## Calculates the inverse matrix if it has not been calculated 
        ##      yet
        else
        {
                ## Retrieve matrix and solves it to create the inverse 
                ##      and returns the value of the inverse matrix
                dataMatrix <- myObject$getMatrix()
                inverseMatrix <- solve( dataMatrix, ... )
                myObject$setInverse( inverseMatrix )
                return( inverseMatrix )
        }
}
