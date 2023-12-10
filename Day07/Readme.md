Day07
================
Johannes Friedrich
07.12.2023

## Read input

``` r
input <- read.table("input.txt", col.names = c("hand", "bid"))
```

## Part 1

``` r
cards <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
types <- c("11111", "1112", "122", "113", "23", "14", "5")

data <- setNames(do.call(rbind.data.frame, strsplit(input$hand, '')), 
          c("Card1", "Card2", "Card3", "Card4", "Card5"))

data$type <- data |> 
    apply(1, \(x){
      table(x) |> 
        sort() |> 
        paste0(collapse = "")
    })
data$bid <- input$bid

calculate_sum <- function(data){
  data[order(factor(data$type,  levels = types, ordered = TRUE),
             factor(data$Card1, levels = cards, ordered = TRUE),
             factor(data$Card2, levels = cards, ordered = TRUE),
             factor(data$Card3, levels = cards, ordered = TRUE),
             factor(data$Card4, levels = cards, ordered = TRUE),
             factor(data$Card5, levels = cards, ordered = TRUE)),]  |> 
    _["bid"] |> 
    (`*`)(1:nrow(input)) |> 
    sum()
}
calculate_sum(data)
```

    ## [1] 248396258

## Part 2

``` r
cards <- c("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A")

data <- setNames(do.call(rbind.data.frame, strsplit(input$hand, '')), 
          c("Card1", "Card2", "Card3", "Card4", "Card5"))

data$type <- data |> 
    apply(1, \(x){
      tab <- table(x)|> sort()
      if(!is.na(tab["J"])) {
        nr_of_jokers <- tab["J"]
        tab <- tab[-which(names(tab) == "J")]
        if(nr_of_jokers < 5) {
          tab[length(tab)] <- tab[length(tab)] + nr_of_jokers
        } else {
          tab["J"] <- 5
        }
      }
      paste0(tab, collapse = "")
    })

data$bid <- input$bid

calculate_sum(data)
```

    ## [1] 246436046
