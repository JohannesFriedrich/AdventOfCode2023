Day14
================
Johannes Friedrich
14.12.2023

## Read input

``` r
input <- readLines("input.txt")

data <- input |> 
  strsplit("") |> 
  unlist() |> 
  matrix(ncol = nchar(input[1]), byrow = TRUE)
```

## Part 1

``` r
shift_O <- function(col){
  for(idx in which(col == "O")){
    while (idx > 1 && col[idx-1] == "."){
        col[idx-1] <- "O"
        col[idx] <- "."
        idx <- idx-1
    }
  }
  return(col)
}

apply(data, 2, shift_O) |> 
  apply(1, \(row) sum(row == "O")) |> 
  "*"(lhs = _, nrow(data):1) |> 
  sum()
```

    ## [1] 109654

## Part 2
