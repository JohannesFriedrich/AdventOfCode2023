Day02
================
Johannes Friedrich
02.12.2023

## Read input

``` r
input <- readLines("input.txt")
```

## Part 1

``` r
limits <- c(red = 12, green = 13, blue = 14)

extract_color_values <- function(color){
    regmatches(input, gregexpr(paste0("(?<=Game)\\s\\d+|\\d+\\s(?=",color,")"), input, perl = TRUE))
}

## identify wrong Game IDs
check_cubes <- function(color){
  extract_color_values(color) |> 
  sapply(\(x) {
    nums <- as.integer(x)
    if (any(nums[-1] > limits[color])) return(nums[1])
    }) |> 
    unlist()
}

## sum of all Game IDs minus the wrong game IDs
length(input)*(length(input)+1)/2 - c(check_cubes("blue"),check_cubes("green"),check_cubes("red")) |> 
  unique() |> 
  sum()
```

    ## [1] 2439

## Part 2

``` r
## same as in part1, but the Game ID is now not needed -> regexp changed
extract_color_values <- function(color){
    regmatches(input, gregexpr(paste0("\\d+\\s(?=",color,")"), input, perl = TRUE))
}

get_max <- function(color){
  extract_color_values(color) |> 
    sapply(\(x) max(as.integer(x)))
}

sapply(c("red", "green", "blue"), get_max) |> 
  apply(1, prod) |> 
  sum()
```

    ## [1] 63711
