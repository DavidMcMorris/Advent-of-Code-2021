#Day 4, Puzzle 2


calls <- read.table("Day_4_input.txt", colClasses="character", nrows = 1)
boards <- read.table("Day_4_input.txt", colClasses="numeric", skip = 1)

calls <- as.numeric(strsplit(calls[1,1],split=',')[[1]])

old_list <- NULL
new_list <- NULL
winning_order <- NULL
for(i in 1:length(calls)){
win_board_nums <- NULL
  old_list <- new_list
    inds <- which(boards==calls[i], arr.ind = TRUE)
    boards[inds] <- 100

    if(sum(rowSums(boards) == 500) > 0){
      win_board_nums <- unique(ceiling(which(rowSums(boards) == 500)/5))
    }
    for(j in 1:((nrow(boards)/5)-1)){
      if(sum(colSums(boards[(1+5*(j-1)):(5*j),]) == 500) > 0){
        win_board_nums <- c(win_board_nums,j)
      }
    }
  winning_order <- c(winning_order,setdiff(unique(win_board_nums),old_list))
  new_list <- unique(c(win_board_nums,old_list))
}

boards <- read.table("Day_4_input.txt", colClasses="numeric", skip = 1)

winning_board_num <- winning_order[length(winning_order)]
winning_board <- boards[(1+5*(winning_board_num-1)):(5*winning_board_num),]

for(i in 1:length(calls)){
    inds <- which(winning_board==calls[i], arr.ind = TRUE)
    winning_board[inds] <- 100

    if(sum(rowSums(winning_board) == 500) > 0){
      win_board_num <- ceiling(which(rowSums(winning_board) == 500)/5)
      break
    }
    else for(j in 1:((nrow(winning_board)/5)-1)){
      if(sum(colSums(winning_board[(1+5*(j-1)):(5*j),]) == 500) > 0){
        win_board_num <- j
        break
      }
    }
}

board_score <- sum(winning_board[winning_board != 100])
winning_call <- calls[i]
final_score <- board_score * winning_call

print(final_score)
