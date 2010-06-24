## Access the package private environment.
## TODO: add system tests.

## Get a variable or an item from a variable in
## the package private environment.
.get <- function(name, idx = NULL)
{
    if (!is.null(.env) && exists(name, envir = .env))
    {
        var <- get(name, envir = .env)
        if (!is.null(idx))
        {
            if (is.vector(var))
            {
                idx <- as.integer(idx)
                if (idx > 0 && idx <= length(var))
                    var[[idx]]
                else
                    stop(idx, " is not a valid index.")
            }
            else
                stop(name, " is not a vector.")
        }
        else
            var
    }
    else
        stop(name, " is not in '.env'.")
}


## Set a variable or an item from a variable in
## the package private environment.
.set <- function(name, value, idx = NULL)
{
    if (is.null(idx))
        assign(name, value, envir = .env)
    else
    {
        if (!is.null(.env)&& exists(name, envir = .env))
        {
            var <- get(name, envir = .env)

            if (is.vector(var))
            {
                idx <- as.integer(idx)
                if (idx > 0)
                {
                    var[[idx]] <- value
                    assign(name, var, envir = .env)
                }
                else
                    stop(idx, " is not a valid index.")
            }
            else
                stop(name, " is not a vector.")
        }
        else
            stop(name, " is not in '.env'.")
    }
}


## Removed a variable or an item from a variable in
## the package private environment.
.rm <- function(name, idx = NULL)
{
    if (!is.null(.env) && exists(name, envir = .env))
    {
        if (is.null(idx))
            rm(list = name, envir = .env)
        else
        {
            var <- get(name, envir = .env)
            if (is.vector(var))
            {
                idx <- as.integer(idx)
                if (idx > 0 && idx <= length(var))
                {
                    assign(name, var[-idx], envir = .env)
                }
                else
                    stop(idx, " is not a valid index.")
            }
            else
                stop(name, " is not a vector.")
        }
    }
    else
        stop(name, " is not '.env'.")
}

.dump <- function()
{
    local(for (o in ls()) { cat(o, "\n"); print(get(o)) }, envir = .env)
}
