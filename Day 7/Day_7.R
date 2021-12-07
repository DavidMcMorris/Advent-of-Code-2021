#Day 7

input <- scan("Day_7_input.txt",sep=",")

range <- min(input):max(input)

min_fuel_v1 <- NULL
min_fuel_v2 <- NULL

for(i in 1:length(range)){
  cost<-abs(input-range[i])
  mod_cost <- 0.5*cost*(cost+1)
  min_fuel_v1 <- min(min_fuel_v1,sum(cost))
  min_fuel_v2 <- min(min_fuel_v2,sum(mod_cost))
}

print(min_fuel_v1)
print(min_fuel_v2)
