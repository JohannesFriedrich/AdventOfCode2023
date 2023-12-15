Day15
================
Johannes Friedrich
15.12.2023

## Read input

``` r
input <- readLines("input.txt") |> 
  strsplit(",") |> 
  unlist()
```

## Part 1

``` r
calc_hash <- function(string){
  utf8ToInt(string) |> 
  Reduce(\(x,y) ((x+y)*17)%%256,x=_, init = 0)
}

sapply(input, calc_hash) |> 
  sum()
```

    ## [1] 516070

## Part 2

``` r
boxes <- vector(mode='list', length=256)

labels <- input |> 
  gregexpr("[a-z]+",text = _) |> 
  regmatches(input, m=_) |> 
  unlist()
operation <- input |> 
  gregexpr("[-|=]",text = _) |> 
  regmatches(input, m=_) |> 
  unlist()
focal_length <- input |> 
  gregexpr("\\d+",text = _) |> 
  regmatches(input, m=_) |> 
  as.integer()

data <- data.frame(labels, operation, focal_length)

for(i in seq_along(input)){
  line <- data[i,]
  box <- calc_hash(line$labels) + 1
  if(line$operation == "="){
    boxes[[box]][line$labels] = line$focal_length
  } else {
    if (!is.na(match(line$labels, names(boxes[[box]])))) {
      idx <- which(names(boxes[[box]]) == line$labels)
      boxes[[box]] <- boxes[[box]][-idx] 
    }
  }
}

sapply(seq_along(boxes), \(box_nr){
  if(length(boxes[[box_nr]] != 0)){
    return(box_nr * boxes[[box_nr]] * 1:length(boxes[[box_nr]]))
  } else {
    return(0)
  }
}) |>
  unlist() |> 
  sum()
```

    ## [1] 244981
