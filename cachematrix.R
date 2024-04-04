# Create a function that caches the result of a potentially time-consuming computation
make_cacheable_function <- function(computation_function) {
  # Create an environment to store cached results
  cache <- new.env()
  
  # Define the wrapper function that performs caching
  wrapper <- function(...) {
    # Convert the arguments to a string to use as a cache key
    args_string <- paste0(..., collapse = "|")
    
    # Check if the result is already cached
    if (exists(args_string, envir = cache)) {
      cat("Using cached result.\n")
      return(get(args_string, envir = cache))
    } else {
      cat("Computing and caching result.\n")
      result <- computation_function(...)
      assign(args_string, result, envir = cache)
      return(result)
    }
  }
  
  # Return the wrapper function
  return(wrapper)
}

# Example of a potentially time-consuming computation function
mean_slow <- function(x) {
  Sys.sleep(2)  # Simulate a slow computation
  return(mean(x))
}

# Create a cached version of the mean_slow function
cached_mean_slow <- make_cacheable_function(mean_slow)

# Test the cached function
x <- c(1, 2, 3, 4, 5)

# First call - should be slow
print(cached_mean_slow(x))

# Second call with the same argument - should be fast (using cached result)
print(cached_mean_slow(x))

# Another call with a different argument - should be slow again
print(cached_mean_slow(c(6, 7, 8, 9, 10)))
