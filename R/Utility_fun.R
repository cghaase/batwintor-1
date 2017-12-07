# utility functions

# Not in operator
'%!in%' <- function(x,y)!('%in%'(x,y))

# Defaults for NULL values
'%||%' <- function(a, b) if (is.null(a)) b else a

NULL
