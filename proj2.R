# Define the function "insert"
# in oder to find the shorted queue and insert the new arrival car
# "station" is an array with two numerical lists
# station["qn"] is the list of number representing how many cars in that queue
# station["dt"] is the list of number representing the processing time for each queue
# "tr" and "tm" is the given constants
insert <- function(station,tr,tm){
  # find the shortest queue
  qn_min <- which.min(station["qn",]) 
  # the new arrival car joins the shortest queue
  station["qn",qn_min]<- station["qn",qn_min]+1
  # "-1" represent there isn't any car in the queue
  # if the remainging queue didn't have any car, then use random uniform distribution
  # to find it's processing time and add to the list of processing time
  if(station["dt",qn_min]==-1){
    station["dt",qn_min] <- round(runif(1,tm,tm+tr))
  }
  return (station)
}

# Define the function "finish"
# in oder to let car that have been processed leave the queue and record the processing 
# time for that car
finish <- function(station,queue,tr,tm){
  # there is one car finish processing and need to leave, then the number of cars
  # in that queue will reduce one
  station["qn",queue]<-station["qn",queue]-1
  # if there is more that one car in the queue
  # then use random uniform distribution to find the processing time and record it
  # if not, let the processing time equals to "-1" which represent there is no car
  # in the queue
  if(station["qn",queue] > 0){
    station["dt",queue] <- round(runif(1,tm,tm+tr))
  }
  else{
    station["dt",queue] <- -1
  }
  return(station)
}
# Define the function "qsim(mf,mb,a.rate,trb,trf,tmb,tmf,maxb)"
# "mf" is the number of French stations
# "mb" is the number of British stations
# "a.rate" is the probability a new car arrives at French stations in one second
# A car arrives at the French stations randomly at an average rate of one every 
# 10 seconds, so that there is a probability of 0.1 of an arrival each second
# The processing time in the French stations will be a random number uniformly distributed between (tmf,tmf+trf) 
# The processing time in the British stations will be a random number uniformly distributed between (tmb,tmb+trb) 
# The maximum number of cars in each British station will be "maxb"
qsim <- function(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20) {
  # Here are some variables we will use later
  # "start" is a list to store the time stamp when a new car arrives at the French stations 
  # "end" is a list to store the time stamp when a car finish processing and leave the 
  # Brisish station
  start <- c(0)
  end<-c(0)
  # "f" is a data.frame to store the processing time for cars in the French stations
  # Choosing to use a data.frame to store the processing time to avoid messing up each station
  # Columns in "f" stand for each French station, namely "station_1", "station_2", ..., "station_mf"
  # The first row in "f" stores the number of cars waiting in the station
  # The second row in "f" stores the processing time for each car
  column_names_f <- paste0("station_", seq_len(mf))
  f <- data.frame(matrix(c(0,-1),ncol=length(column_names_f), nrow=2))
  colnames(f) <- column_names_f
  rownames(f) <- c("qn","dt")
  # "b" is a data.frame to store the processing time for cars in the British stations
  # Same ideas to build "b"
  column_names_b <- paste0("station_", seq_len(mb))
  b <- data.frame(matrix(c(0,-1),ncol=length(column_names_b), nrow=2))
  colnames(b) <- column_names_b
  rownames(b) <- c("qn","dt")
  
  nf <- c()
  nb <- c()
  eq <- c()
  # This model will simulate for a 2-hour period, in total 7200 seconds
  for (t in 1:(2*60*60)){
    # first find the column that the processing time equals to 0 in British station
    # the reason why we need to start with British station is because it's hard
    # to leave the car in French staion if the British station is full, so we need
    # to first find the car leave British station first and then go back to cars 
    # that leave French station and go to British station directly
    for( i in which(b["dt", ] %in% 0)){
      b <- finish(b,i,trb,tmb)
      end <- append(end,t)
    }
    # find the column that the processing time equals to 0 in French station
    for (i in which(f["dt",] %in% 0)){
      # if the sum of the queue number in british station is smaller then the 
      # maximum capcity, that means there is still some vacancy in British staion
      if(sum(b["qn",])<maxb*mb){
        f <- finish(f,i,trf,tmf)
        b <- insert(b,trb,tmb)
      }
    }
    
    
    
    # Provided "a.rate" this input as the arriving probability, we use "sample()" 
    # function to randomly choose "TRUE" or "FALSE"
    # "TURE" indicates a car arrives while "False" indicating no cars arrive
    # Ignoring the situation that more than one car arriving at the same time, so 
    # "size" attribute in "sample()" function is 1
    if (t <= (90*60)){ # New cars only arrive at French stations in the first 1.5 hours
      if (sample(c(TRUE,FALSE), size = 1, prob = c(a.rate,1-a.rate))){
        # If "sample()" function generates "TURE" as the result, which means a new car 
        # arrives at the French stations, we will add a time stamp to the "start" list
        start <- append(start,t)
        #print("start a new car")
        f <- insert(f,trf,tmf)
      }
    }
    
    
    
    # One iteration in this for loop means one second has passed, so we cut one 
    # second off from the positive processing time for each car.
    f["dt", ] <- apply(f["dt", , drop = FALSE], 1, function(row) {
      ifelse(row > 0, row - 1, row)
    })
    b["dt", ] <- apply(b["dt", , drop = FALSE], 1, function(row) {
      ifelse(row > 0, row - 1, row)
    })
    
    nf <- append(nf,sum(f["qn",])/mf)
    nb <- append(nb,sum(b["qn",])/mb)
    
    eq <- append(eq,(sum(end)+(length(start)-length(end))*t-sum(start))/length(start))
  }
  

  return(list(nf,nb,eq))
}

iqsim <- qsim()
jqsim <- qsim(tmb=40)
# plot a 4 panel 2 row, 2 row plot
par(mfrow=c(2,2))

# plot of French and British queue lengths change over time
matplot(1:7200, cbind(iqsim[[1]],iqsim[[2]]), type='l', col=c("red", "blue"), 
        xlab="time", ylab="average queue length", 
        main="French and British queue lengths 
        change over time")
legend("topright", c("French", "British"), col=c("red", "blue"), lty=1:2, cex=0.6)

# plot of expected queueing time changes over time
matplot(1:7200, iqsim[[3]], type='l', xlab="time", ylab="expected queuing time", 
        main = "Expected queueing time 
        changes over time")

# plot of French and British queue lengths change over time when the minimum British
# handling time is set to 40 seconds
matplot(1:7200, cbind(jqsim[[1]],jqsim[[2]]), type='l', col=c("red", "blue"), 
        xlab="time", ylab="average queue length", 
        main="French and British queue lengths 
        change over time when minimum 
        British handling time set to be 40s")
legend("topleft", c("French", "British"), col=c("red", "blue"), lty=1:2, cex=0.6)

# plot of expected queueing time changes over time when the minimum British handling
# time is set to 40 seconds
matplot(1:7200, jqsim[[3]], type='l', xlab="time", ylab="expected queuing time", 
        main = "Expected queueing time 
        changes over time when minimum 
        British handling time set to be 40s")

# in order to find the probability of at least one car missing ferry departure,
# we need to run qsin() function 100 times
k <- 0
for (i in 1:100){
  # if the final average queue length in Frencg station isn't zero, that means 
  # there is at least one car missing ferry departure
  if (qsim()[[1]][7200] != 0){
      k <- k + 1
  }
}
k

print(paste0("The probability of at least one car missing the ferry departure is", k/100))

