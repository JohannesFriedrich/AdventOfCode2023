Day13
================
Johannes Friedrich
13.12.2023

## Read input

``` r
grids <- readChar("input.txt", 1e5) |> 
  strsplit("\n\n")  |> 
  unlist() |> 
  sapply(strsplit, "\n", USE.NAMES = FALSE) |> 
  lapply(\(row){
    do.call(rbind, strsplit(row, ""))
  })
```

## Part 1

``` r
check_sym <- function(matrix, type = "row"){
  len <- ifelse(type == "row", nrow(matrix), ncol(matrix))
  for(sym_line in 1:(len-1)){
    are_symmetric <- TRUE
    for(row_index in 1:(sym_line)){
      if(1 <= (sym_line-row_index+1) && (sym_line+row_index) <= len && are_symmetric){
        if (type == "row"){
          are_symmetric <- are_symmetric && all(matrix[sym_line-row_index+1,] == matrix[sym_line+row_index,])
        } else {
          are_symmetric <- are_symmetric && all(matrix[,sym_line-row_index+1] == matrix[,sym_line+row_index])
        }
      } else {
        break
      }
    }
    if(are_symmetric) break
  }
  return(if(are_symmetric) sym_line else 0)
}


sum(sapply(grids, check_sym, "col")) + 100 * sum(sapply(grids, check_sym, "row"))
```

    ## [1] 36041

## Part 2

``` r
check_sym <- function(matrix, type = "row"){
  len <- ifelse(type == "row", nrow(matrix), ncol(matrix))
  for(sym_line in 1:(len-1)){
    diffs <- 0
    for(row_index in 1:(sym_line)){
      if(1 <= (sym_line-row_index+1) && (sym_line+row_index) <= len){
        if (type == "row"){
          diffs <- diffs + sum(matrix[sym_line-row_index+1,] != matrix[sym_line+row_index,])
        } else {
          diffs <- diffs + sum(matrix[,sym_line-row_index+1] != matrix[,sym_line+row_index])
        }
      } else {
        break
      }
    }
    if(diffs == 1) break
  }
  return(if(diffs == 1) sym_line else 0)
}

sum(sapply(grids, check_sym, "col")) + 100 * sum(sapply(grids, check_sym, "row"))
```

    ## [1] 35915
