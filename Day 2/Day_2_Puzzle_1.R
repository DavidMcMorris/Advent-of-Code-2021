#Day 2, Puzzle 1

input <- read.table("Day_2_input.txt")
colnames(input) <- c("direction","value")

sums <- aggregate(x= input$value,
          by= list(input$direction),
          FUN=sum)
position <- c(sums$x[2],sums$x[1] - sums$x[3])

product <- prod(position)

print(product)