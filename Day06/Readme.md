Day06
================
Johannes Friedrich
06.12.2023

## Read input

``` r
input <- read.table("input.txt")
```

## Part 1

``` r
get_roots <- function(data){
  discriminant <- sqrt(data[1]^2 - 4 * data[2])
  return((data[1] + c(-1,1) * discriminant)/2)
}

get_integer_limits <- function(data){
  c(floor(data[1]+1), ceiling(data[2]-1))
}

input[,-1] |> 
  apply(2, \(x) x |> 
          get_roots() |> 
          get_integer_limits() |> 
          diff()+1) |> 
  prod()
```

    ## [1] 505494

## Part 2

``` r
time <- as.numeric(paste0(input[1,-1], collapse = "")) 
distance <- as.numeric(paste0(input[2,-1], collapse = ""))

t(c(time, distance)) |> 
  apply(1, \(x) get_roots(x) |> 
          get_integer_limits() |> 
          diff()+1)
```

    ## [1] 23632299
