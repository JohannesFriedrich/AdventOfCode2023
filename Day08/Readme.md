Day08
================
Johannes Friedrich
08.12.2023

## Read input

``` r
input <- readLines("input.txt")
```

## Part 1

``` r
instructions <- input[[1]] |> 
  strsplit("") |> 
  unlist()

network <- regmatches(input[-(1:2)], gregexpr("\\w+", input[-(1:2)])) |> 
  unlist() |> 
  matrix(ncol=3, byrow = TRUE) |> data.frame()

colnames(network) <- c("Node", "L", "R")

next_node <- "AAA"
counter <- 1
while(next_node != "ZZZ"){
  next_instruction <- ((counter-1)%%(length(instructions)))+1
  counter <- counter + 1
  next_node <- network[which(network$Node %in% next_node), instructions[next_instruction]]
}
counter - 1
```

    ## [1] 14893

## Part 2
