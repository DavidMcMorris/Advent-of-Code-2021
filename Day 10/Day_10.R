# Day 10 Puzzle


input <- read.table('Day_10_Input.txt', colClasses='character')
input <- lapply(input, strsplit, '')[[1]]

point_value1 <- function(chars){
  close <- c(')', ']', '}', '>')
  points_list <- c(3, 57, 1197, 25137)
  value <- points_list[which(close == chars)]
  return(value)
}

point_value2 <- function(chars){
  open <- c('(', '[', '{', '<')
  points_list <- c(1, 2, 3, 4)
  points <- NULL
  for(i in 1:length(chars)){
    points <- c(points,points_list[which(open == chars[i])])
  }
  value <- 0
  for(i in 1:length(chars)){
    value <- 5*value + points[i]
  }
  return(value)
}

syntax_checker <- function(line,puzzle){
  open_list <- NULL
  open <- c('(', '[', '{', '<')
  close <- c(')', ']', '}', '>')
  for(i in 1:length(line)){
    if(line[i] %in% open){
      open_list <- c(open_list, line[i])
    }
    else {
      match <- open[which(close == line[i])]
      if(match == open_list[length(open_list)]){
        open_list <- open_list[1:(length(open_list)-1)]
      }
      else {
        if(puzzle == 1){
          return(point_value1(line[i]))
        }
        else {
          return(0)
        }
      }
    }
  }
  if(i == length(line)){
    if(puzzle == 1){
      return(0)
    }
    else {
      return(point_value2(rev(open_list)))
    }
  }
}

total1 <- 0
total2 <- NULL
for(i in 1:length(input)){
    total1 <- total1 + syntax_checker(input[[i]],1)
    total2 <- c(total2, syntax_checker(input[[i]],2))
}
total2 <- median(total2[which(total2!=0)])

print(total1)
print(total2)
