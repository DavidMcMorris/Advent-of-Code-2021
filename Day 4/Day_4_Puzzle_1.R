#Day 4, Puzzle 1


calls <- read.table("Day_4_input.txt", colClasses="character", nrows = 1)
boards <- read.table("Day_4_input.txt", colClasses="numeric", skip = 1)

calls <- as.numeric(strsplit(calls[1,1],split=',')[[1]])

for(i in 1:length(calls)){
    inds <- which(boards==calls[i], arr.ind = TRUE)
    boards[inds] <- 100

    if(sum(rowSums(boards) == 500) > 0){
      win_board_num <- ceiling(which(rowSums(boards) == 500)/5)
      break
    }
    else for(j in 1:((nrow(boards)/5)-1)){
      if(sum(colSums(boards[(1+5*(j-1)):(5*j),]) == 500) > 0){
        win_board_num <- j
        break
      }
    }
}

winning_board <- boards[(1+5*(win_board_num-1)):(5*win_board_num),]
board_score <- sum(winning_board[winning_board != 100])
winning_call <- calls[i]
final_score <- board_score * winning_call

print(final_score)
