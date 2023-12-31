# First, prepare for the functions to update the queues and processing time

# Define a function "finish()" to move the car that have finished processing out of the queue
# "station" is a matrix with two numerical lists
# Columns in the "station" array stand for each queue in the stations
# station["qn",] is a list consisting the number of cars in each queue
# station["dt",] is the list consisting the processing time for the processing car in each queue
# We will use "sample()" to generate random processing time in the range (tr,tr+tm)
finish <- function(station, queue, tr, tm){
  
  # If there is one car finish being processed and be able to leave the current queue, 
  # then we will reduce one from the number of queuing cars in this queue
  station["qn", queue] <- station["qn", queue] - 1
  
  # If there is more than one car in the queue, the car that has been waiting in the queue will be processed after the old car got moved out from the queue
  # then use 'sample()' to generate a processing time for this car
  if(station["qn", queue] > 0){
    station["dt", queue] <- sample(tm:(tm+tr), 1)
  }
  # If there is no car left in the queue, we give "-1" to the processing time as an indicator, just for convenience 
  else{
    station["dt", queue] <- -1
  }
  
  return(station)
}

# Define another function "insert()" to find the shortest queue and insert the newly arriving car into the queue 
insert <- function(station, tr, tm){
  # Find the shortest queue
  qn_min <- which.min(station["qn", ]) 
  # The newly arrived car will join the shortest queue
  station["qn", qn_min] <- station["qn", qn_min] + 1
  
  # According to "finish()" function we had defined above, 
  # "-1" for processing time means there's no car waiting in the queue, 
  # so the newly inserted car will be processed immediatelt.
  # Then we use "sample()" function to generate a random processing time for this car
  if(station["dt", qn_min] == -1){
    station["dt", qn_min] <- sample(tm:(tm+tr), 1)
  }
  
  return (station)
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
qsim <- function(mf = 5, mb = 5, a.rate = .1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) {
  
  # Here are some variables we will use later:
  # "f" is a matrix to store the processing time for cars in the French stations
  # Columns in "f" stand for each French station, namely "station_1", "station_2", ..., "station_mf"
  # The first row in "f" namely "qn" stores the number of cars waiting in the station
  # The second row in "f" namely "dt" stores the processing time for each car being processed
  column_names_f <- paste0("station_", seq_len(mf))
  f <- matrix(c(0,-1), nrow = 2, ncol = length(column_names_f))
  colnames(f) <- column_names_f
  rownames(f) <- c("qn","dt")
  
  # "b" is a matrix to store the processing time for cars in the British stations
  # Same ideas above to build "b"
  column_names_b <- paste0("station_", seq_len(mb))
  b <- matrix(c(0,-1), nrow = 2, ncol = length(column_names_b))
  colnames(b) <- column_names_b
  rownames(b) <- c("qn","dt")
  
  nf <- c() # nf is a list to store the average queue length in French station for each second
  nb <- c() # nb is a list to store the average queue length in British station for each second
  eq <- c() # eq is a list to store the expected waiting time for each second
  
  # This model will simulate for a 2-hour period, in total 7200 seconds
  for (t in 1:(2*60*60)){
    
    # Notice: Since the cars can't leave from the French station to the British one if there is no available queues in British station, so we start with analyzing the queues in British station
    # First, find the columns that with 0 processing time in British station
    # Then move those cars with 0 processing time out of the queue using "finish()" function to cut one off the queue and give a processing time to the next car
    for( i in which(b["dt", ] %in% 0)){
      b <- finish(b, i, trb, tmb)
    }
    
    # Find the columns with 0 processing time in the French station
    for (i in which(f["dt", ] %in% 0)){
      # If the sum of the number of cars in the queue is less than the total maximum capacity for the whole British station, 
      # we will know there are still some vacancies in the British station
      # Then we can move the cars finished processing from French station to the British station using "finish()" function we defined above
      if(sum(b["qn", ]) < maxb*mb){
        f <- finish(f, i, trf, tmf)
        b <- insert(b, trb, tmb)
      }
    }
    
    # Provided "a.rate" this input as the arriving probability, we use "sample()" function to randomly choose "TRUE" or "FALSE"
    # "TURE" indicates a car arrives while "False" indicating that there are no cars arrived
    # Ignoring the situation that more than one car arriving at the same time, so "size" attribute in "sample()" function is 1
    if (t <= 90*60){ # New cars only arrive at French stations in the first 1.5 hours in the simulation
      if (sample(c(TRUE,FALSE), size = 1, prob = c(a.rate, 1-a.rate))){
        # If "sample()" function generates "TURE" as the result, which means a new car arrives at the French stations
        # We will use "insert()" defined above to put a new car in the queue in the French station
        f <- insert(f, trf, tmf)
      }
    }
    
    # One iteration in this loop means one second has passed, so we cut one second off from the positive processing time for each car.
    f["dt", ] <- apply(f["dt", , drop = FALSE], 1, function(row) {
      ifelse(row > 0, row - 1, row)
    })
    b["dt", ] <- apply(b["dt", , drop = FALSE], 1, function(row) {
      ifelse(row > 0, row - 1, row)
    })
    
    # avg_f is the average queue length in the French station.
    # Calculated by summing up the number of cars in all queues first, then divided by the number of stations
    avg_f <- sum(f["qn", ])/mf
    # avg_b is the average queue length in the British station
    avg_b <- sum(b["qn", ])/mb 
    # Update the average queue length and expected waiting time for this second to the lists storing results across the simulation
    nf <- append(nf, avg_f)
    nb <- append(nb, avg_b)
    # Expected waiting time is the sum of the average processing time in each station multiply with average queue length in  each station
    eq <- append(eq, (avg_f*(tmf + trf/2) + avg_b*(tmb + trb/2)))
  }

  return(list(nf, nb, eq))
}


iqsim <- qsim() # Default parameters
jqsim <- qsim(tmb = 40) # Set the minimum processing time in British station as 40
# Create a panel with 2-row, 2-row plots
par(mfrow = c(2,2))

# plot of French and British queue lengths change over time
matplot(1:7200, cbind(iqsim[[1]],iqsim[[2]]), type = 'l', col = c("red", "blue"), 
        xlab = "Time", ylab = "Average queue length", 
        main = "Average queue length of the French Station and the British Station changing over time")
legend("topright", c("French", "British"), col = c("red", "blue"), lty = 1:2, cex = 0.6)

# plot of expected waiting time changes over time
matplot(1:7200, iqsim[[3]], type = 'l', xlab = "time", ylab = "expected queuing time", 
        main = "Expected waiting time changing over time")

# plot of French and British queue lengths change over time when the minimum British handling time is set as 40 seconds
matplot(1:7200, cbind(jqsim[[1]],jqsim[[2]]), type = 'l', col = c("red", "blue"), 
        xlab = "Time", ylab = "Average queue length", 
        main = "Average queue length of the French Station and the British Station changing over time when tmb = 40 ")
legend("topleft", c("French", "British"), col = c("red", "blue"), lty = 1:2, cex = 0.6)

# plot of expected waiting time changing over time when the minimum British handling time is set as seconds
matplot(1:7200, jqsim[[3]], type = 'l', xlab = "time", ylab = "expected queuing time", 
        main = "Expected waiting time changing over time when tmb = 40")

# In order to find the probability of at least one car missing ferry departure,we need to run qsim() function 100 times
k <- 0 # k is the number of times there are cars missing the ferry after 100 times simulation
for (i in 1:100){
  # If the final average queue lengths are above zero, that means there is at least one car missing the ferry departure
  kqsim <- qsim(tmb = 40)
  if (kqsim[[2]][7200] > 0 || kqsim[[1]][7200] > 0){
      k <- k + 1
  }
}

print(paste0("The probability of at least one car missing the ferry departure is ", k/100))

