Day11
================
Johannes Friedrich
11.12.2023

## Read input

``` r
input <- readLines("input.txt")

data <- input |>
 strsplit("") |>
 unlist() |>
 matrix(ncol = nchar(input[[1]]), byrow = TRUE)
```

## Part 1

``` r
idx_add_row <- sapply(seq_len(nrow(data)), \(x) if(all(grepl("[.]", data[x,]))) TRUE else FALSE) |>
 which()
idx_add_col <- sapply(seq_len(ncol(data)), \(x) if(all(grepl("[.]", data[,x]))) TRUE else FALSE) |>
 which()

coordinates_galaxy <- which(data == "#", arr.ind = TRUE)

calc_distance <- function(idx, factor){
  ## check if and how many additional rows and cols are added
  add_row <- (min(coordinates_galaxy[idx[1],"row"], coordinates_galaxy[idx[2],"row"]) < idx_add_row & idx_add_row < max(coordinates_galaxy[idx[1],"row"], coordinates_galaxy[idx[2],"row"])) |> 
    sum()
  add_col <- (min(coordinates_galaxy[idx[1],"col"], coordinates_galaxy[idx[2],"col"])  < idx_add_col & idx_add_col < max(coordinates_galaxy[idx[1],"col"], coordinates_galaxy[idx[2],"col"])) |> 
    sum()
  abs(coordinates_galaxy[idx[1],"row"] - coordinates_galaxy[idx[2],"row"]) + add_row*(factor-1) + abs(coordinates_galaxy[idx[1],"col"] - coordinates_galaxy[idx[2],"col"]) + add_col*(factor-1)
}

combn(seq_len(nrow(coordinates_galaxy)), 2, calc_distance, factor = 2) |> 
  sum()
```

    ## [1] 9795148

## Part 2

``` r
combn(seq_len(nrow(coordinates_galaxy)), 2, calc_distance, factor = 1000000) |> 
  sum()
```

    ## [1] 650672493820
