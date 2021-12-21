# Day 16 Puzzle 2

input <- read.table("Day_16_Input.txt",colClasses = 'character')
input <- unlist(strsplit(as.character(input),split=""))

# input <- unlist(strsplit("9C0141080250320F1802104A08",split=""))

hex2bin <- function(x){
  convert <- data.frame("hex" = c(0:9,LETTERS[1:6]), "dec" = c("0000","0001","0010","0011","0100","0101","0110","0111","1000","1001","1010","1011","1100","1101","1110","1111"),stringsAsFactors = F)
  x <- convert$dec[which(convert$hex == x)]
  return(x)
}

input <- sapply(input, hex2bin)
input <-as.vector(unlist(strsplit(input,split="")))

# Replacement for base::strtoi b/c of overflow
convert <- function(x) {
    y <- as.numeric(strsplit(x, "")[[1]])
    return(sum(y * 2^rev((seq_along(y)-1))))
}

# Jumps ahead and returns literal value
literal_value <- function(i,input){
  prefix <- input[i]
  i <- i+1
  value <- input[i:(i+3)]
  i <- i+4
  while(prefix == 1){
    prefix <- input[i]
    i<-i+1
    value <- c(value,input[i:(i+3)])
    i<-i+4
  }
  value <- paste0(value,collapse='')
  value <- convert(value)
  return(c(i,value))
}

# Recursively defined packet scanner
# Returns several pieces of information:
# Only [[1]]: Version Sum, [[5]]: IDs, [[6]]: literal values are relevant outside the recursion
packet_scan <- function(input, VerSum = 0, NumPackets = NULL, LengthPackets = NULL, b=0, byNumber = F, byLength = F){
  ID_list <- NULL
  value_list <- NULL
  if(byLength == T && byNumber == T){
    stop("byLength and byPackets both true")
  } else if(byLength == T){
    counter <- 0
    stopCount <- LengthPackets
  } else if(byNumber == T){
    counter <- 0
    stopCount <- NumPackets
  } else {
    counter <- 0
    stopCount <- 3
  }
  i<-1
  while(counter < stopCount && setequal(unique(input[i:length(input)]),'0') == F){
    if(b == 1){
      value_list <- c(value_list, "CC")
      return(list(VerSum,counter,i,b,ID_list,value_list))
    }
    VerSum <- VerSum + convert(paste0(input[i:(i+2)],collapse=''))
    i<-i+3
    ID <- convert(paste0(input[i:(i+2)],collapse=''))
    value_list <- c(value_list,paste0("S",ID,collapse=""))
    ID_list <- c(ID_list,'ID',ID)
    i<-i+3
    if(ID == 4){
      outputLV <- literal_value(i,input)
      i <- outputLV[1]
      value <- outputLV[2]
      value_list <- c(value_list,"SV",value,"C")
      if(byNumber == T){
        counter <- counter + 1
      }
      else if(byLength == T){
        counter <- i - 1 - counter
      }
      if(i >= length(input) || setequal(unique(input[i:length(input)]),'0') == T){
        b<-1
        value_list <- c(value_list, "CC")
        return(list(VerSum,counter,i,b,ID_list,value_list))
      }
      else if(counter == stopCount){
        value_list <- c(value_list,"CC")
        return(list(VerSum,counter,i,b,ID_list,value_list))
      }
      else {
        if(byNumber == T){
          LengthPackets <- NULL
          NumPackets <- NumPackets - counter
        }
        else if(byLength == T){
          NumPackets <- NULL
          LengthPackets <- LengthPackets - counter
        }
        else {
          NumPackets <- NULL
          LengthPackets <- NULL
        }
        output <- packet_scan(input[i:length(input)], VerSum=VerSum, NumPackets, LengthPackets, b, byNumber, byLength)
        VerSum <- output[[1]]
        i <- i - 1 + output[[3]]
        if(byNumber == T){
          counter <- counter + 1
        }
        else if(byLength == T){
          counter <- i - counter
        }
        b <- output[[4]]
        ID_list <- c(ID_list,output[[5]])
        value_list <- c(value_list,output[[6]])
      }
    }
    else {
      type <- input[i]
      i<-i+1
      if(type == 0){
        LengthPackets = convert(paste0(input[i:(i+14)],collapse=''))
        i<-i+15
        output <- packet_scan(input[i:length(input)], VerSum=VerSum, NULL, LengthPackets, b, byNumber = F, byLength = T)
        VerSum <- output[[1]]
        i <- i - 1 + output[[3]]
        if(byNumber == T){
          counter <- counter + 1
        }
        else if(byLength == T){
          counter <- i - counter
        }
        b <- output[[4]]
        ID_list <- c(ID_list,output[[5]])
        value_list <- c(value_list,output[[6]])
      }
      else if(type == 1){
        NumPackets = convert(paste0(input[i:(i+10)],collapse=''))
        i <- i+11
        output <- packet_scan(input[i:length(input)], VerSum=VerSum, NumPackets, NULL, b, byNumber = T, byLength = F)
        VerSum <- output[[1]]
        i <- i - 1 + output[[3]]
        if(byNumber == T){
          counter <- counter + 1
        }
        else if(byLength == T){
          counter <- i - counter
        }
        b <- output[[4]]
        ID_list <- c(ID_list,output[[5]])
        value_list <- c(value_list,output[[6]])
        }
      }
    }
  value_list <- c(value_list, "CC")
  return(list(VerSum,counter,i,b,ID_list,value_list))
}

a <- packet_scan(input)
print(a[[1]])

# IDs <- rev(a[[6]][a[[6]] != "ID"])
# vals <- rev(a[[7]])

interpreter <- function(IDs,remVals,currentVals){
  currentID <- IDs[1]
  if(is.na(currentID)){return(list(currentVals, IDs, remVals))}
    if(currentID == 4){
      currentVals <- c(currentVals, remVals[1])
      print(currentVals)
      output <- interpreter(IDs[2:length(IDs)], remVals[2:length(remVals)], currentVals)
      currentVals <- output[[1]]
      IDs <- output[[2]]
      remVals <- output[[3]]
    }
    else if(currentID == 0){
      currentVals <- sum(currentVals)
      print(currentVals)
      output <- interpreter(IDs[2:length(IDs)], remVals[2:length(remVals)], currentVals)
      currentVals <- output[[1]]
      IDs <- output[[2]]
      remVals <- output[[3]]
    }
    else if(currentID == 1){
      currentVals <- prod(currentVals)
      print(currentVals)
      output <- interpreter(IDs[2:length(IDs)], remVals[2:length(remVals)], currentVals)
      currentVals <- output[[1]]
      IDs <- output[[2]]
      remVals <- output[[3]]
    }
    else if(currentID == 2){
      currentVals <- min(currentVals)
      print(currentVals)
      output <- interpreter(IDs[2:length(IDs)], remVals[2:length(remVals)], currentVals)
      currentVals <- output[[1]]
      IDs <- output[[2]]
      remVals <- output[[3]]
    }
    else if(currentID == 3){
      currentVals <- max(currentVals)
      print(currentVals)
      output <- interpreter(IDs[2:length(IDs)], remVals[2:length(remVals)], currentVals)
      currentVals <- output[[1]]
      IDs <- output[[2]]
      remVals <- output[[3]]
    }
    else if(currentID == 5){
      currentVals <- as.numeric(currentVals[1] < currentVals[2])
      print(currentVals)
      output <- interpreter(IDs[2:length(IDs)], remVals[2:length(remVals)], currentVals)
      currentVals <- output[[1]]
      IDs <- output[[2]]
      remVals <- output[[3]]
    }
    else if(currentID == 6){
      currentVals <- as.numeric(currentVals[2] < currentVals[1])
      print(currentVals)
      output <- interpreter(IDs[2:length(IDs)], remVals[2:length(remVals)], currentVals)
      currentVals <- output[[1]]
      IDs <- output[[2]]
      remVals <- output[[3]]
    }
    else if(currentID == 7){
      currentVals <- as.numeric(currentVals[2] == currentVals[1])
      print(currentVals)
      output <- interpreter(IDs[2:length(IDs)], remVals[2:length(remVals)], currentVals)
      currentVals <- output[[1]]
      IDs <- output[[2]]
      remVals <- output[[3]]
    }
  return(list(currentVals, IDs, remVals))
}

# b <- interpreter(IDs,vals,NULL)
