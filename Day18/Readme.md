Day18
================
Johannes Friedrich
18.12.2023

## Read input

``` r
input <- read.table("input.txt",comment.char = "", col.names = c("direction", "size", "RGB"))
input$RGB <- gsub("[()]", "", input$RGB)
```

## Part 1

``` r
cur_pos <- 1+1i
directions <- c("R" = 1+0i, "L" = -1+0i, "U" = 0-1i, "D" = 0+1i)
end_pts <- vector("complex", nrow(input)+1)
end_pts[1] <- cur_pos
perimeter_pts <- sum(input$size)

for(i in seq_len(nrow(input))){
  next_pos <- cur_pos+input[i,"size"] * directions[input[i,"direction"]]
  end_pts[i+1] <- next_pos
  cur_pos <- next_pos
}

calc_points <- function(end_pts, perimeter_pts){
  
  ## https://de.wikipedia.org/wiki/Gau%C3%9Fsche_Trapezformel
  ## Area = 1/2 * sum((xi*xi+1) * (yi*yi+1) + .....) 
  x_ <- Re(end_pts) *  c(tail(Im(end_pts),-1), head(Im(end_pts),1))
  y_ <- Im(end_pts) *  c(tail(Re(end_pts),-1), head(Re(end_pts),1))
  area <- 0.5 * abs(sum(x_) - sum(y_))
  
  ## Picks formula
  # A = I + R/2 - 1 <-> I = A-R/2 + 1
  ## also add outer points
  perimeter_pts + area + 1 - 0.5 * perimeter_pts
}

calc_points(end_pts, perimeter_pts)
```

    ## [1] 70253

## Part 2

``` r
end_pts <- vector("complex", nrow(input)+1)
end_pts[1] <- 1+1i
directions <- c("0" = 1+0i, "2" = -1+0i, "3" = 0-1i, "1" = 0+1i)
perimeter_pts <- 0L

for(i in seq_len(nrow(input))){
  
  size <- substr(input[i,"RGB"],2,6) |> strtoi(16)
  perimeter_pts <- perimeter_pts + size
  direction <- substr(input[i,"RGB"],7,7)
  next_pos <- cur_pos + size * directions[direction]
  end_pts[i+1] <- next_pos
  cur_pos <- next_pos
}

options(scipen = 999) 
calc_points(end_pts, perimeter_pts)
```

    ## [1] 131265059885080
