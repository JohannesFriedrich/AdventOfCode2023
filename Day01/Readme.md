Day01
================
Johannes Friedrich
01.12.2023

## Read input

``` r
input <- readLines("input.txt")
```

## Part 1

``` r
get_nums <- function(list){ regmatches(list, gregexpr("[0-9]",list)) }

input |> 
  get_nums() |> 
  sapply(\(x) as.integer(paste0(x[1],  x[length(x)]))) |> 
  sum()
```

    ## [1] 54953

## Part 2

``` r
sub <- setNames(c("one","two","three","four","five","six","seven","eight","nine"),
                c("o1e", "t2o", "t3e", "f4r", "f5e", "s6x", "s7n", "e8t", "n9e"))

for(i in 1:9){
    input <- gsub(sub[i], names(sub[i]), input)
}

input |> 
  get_nums() |> 
  sapply(\(x) as.integer(paste0(x[1],  x[length(x)]))) |> 
  sum()
```

    ## [1] 53868
