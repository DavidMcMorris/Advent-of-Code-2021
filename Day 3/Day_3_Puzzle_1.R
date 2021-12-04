#Day 3, Puzzle 1


input <- read.table("Day_3_input.txt",colClasses="character")

char_mat <- matrix(nrow = nrow(input),do.call('rbind', strsplit(as.character(input$V1),'',fixed=TRUE)))

num_mat <- matrix(as.numeric(char_mat),nrow=nrow(input))

col_sums <- as.numeric(colSums(num_mat))

gamma <- as.numeric(col_sums > nrow(num_mat)/2)

epsilon <- as.numeric(col_sums < nrow(num_mat)/2)

gamma_int <- strtoi(paste(gamma,collapse=""),base = 2)

epsilon_int <- strtoi(paste(epsilon,collapse=""),base = 2)

product <- gamma_int * epsilon_int

print(product)