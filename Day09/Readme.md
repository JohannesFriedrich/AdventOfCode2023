Day09
================
Johannes Friedrich
09.12.2023

## Read input

``` r
input <- readLines("input.txt")
```

## Part 1

``` r
data <- input |> 
  strsplit(" ") |> 
  lapply(as.integer)

extrapolate <- function(vector){
  last_digit_original <- tail(vector, 1)
  vector <- diff(vector)
  last_digit <- c()
  while(!all(vector == 0)){
    last_digit <- c(last_digit, tail(vector,1))
     vector <- diff(vector)
  }
  last_digit_original + cumsum(last_digit)[length(last_digit)]
}

data |> 
  sapply(extrapolate) |> 
  sum()
```

    ## [1] 1974913025

## Part 2

``` r
extrapolate <- function(vector){
  first_digit_original <- vector[1]
  vector <- diff(vector)
  first_digit <- c()
  while(!all(vector == 0)){
    first_digit <- c(vector[1], first_digit)
    vector <- diff(vector)
  }
  value_to_subtract <- 0
  for(value in first_digit){
      value_to_subtract <- value-value_to_subtract
  }
  first_digit_original - value_to_subtract
}

data |> 
  sapply(extrapolate) |> 
  sum()
```

    ## [1] 884
