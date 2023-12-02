##
setwd("Day01/")
input <- readLines("input.txt")

## regex
regex <- function(input){
  regmatches(input, gregexpr("[0-9]",input))
}

## gsub
gsub_split <- function(input){
  sapply(input, \(x) strsplit(gsub("\\D", "", x), ""))
}

identical(unname(res1), unname(res2))

library(microbenchmark)

result <- microbenchmark::microbenchmark(
  regex(input),
  gsub_split(input)
)

boxplot(result)
