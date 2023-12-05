Day04
================
Johannes Friedrich
04.12.2023

## Read input

``` r
input <- readLines("input.txt")
```

## Part 1

``` r
## helper function to extract nums from line
get_nums <- function(string, reg_exp){
  gregexpr(reg_exp, string, perl = TRUE) |> 
    regmatches(string, m=_) |> 
    unlist() |> 
    trimws() |> 
    strsplit("\\s+") |> 
    sapply(as.integer)
}

nr_of_matching_nums <- input |> 
sapply(\(x){
  win_nums <- get_nums(x, "(?<=:)(\\s+\\d+)+")
  my_nums <- get_nums(x, "(?<=\\|)(\\s+\\d+)+")
  ## compare the 2 vectors
  intersect(my_nums, win_nums) |>
    length() 
},USE.NAMES = FALSE) 
  
2**(nr_of_matching_nums[nr_of_matching_nums-1 >=0]-1) |> 
  sum()
```

    ## [1] 21568

## Part 2

``` r
instances <- rep(1, times = length(nr_of_matching_nums))

for (card in seq_along(nr_of_matching_nums)){
  if (nr_of_matching_nums[card] > 0){
    next_cards <- (card+1):(card+nr_of_matching_nums[card]) 
    instances[next_cards] <- instances[next_cards] + instances[card]
  }
}

sum(instances)
```

    ## [1] 11827296
