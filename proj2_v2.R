# Group members: Bo Gao(s2511232), Zheyue Lin(s2519324) and Xinran Zhu(s2508695)
# Bo Gao(s2511232) and Zheyue Lin(s2519324) worked together to design the function "qsim()"
# Xinran Zhu(s2508695) worked on plots showing how average queuing lengths and expected waiting time changing over time and calculating the probability of at least one car missing the ferry



# Define a function "finish()" to move a car out of the queue after finishing being processed
# Also, "finish()" will give a processing time to the next car in the queue after the previous one got moved
# "station": French stations f or British stations b
# "station" will be a matrix with several columns and two rows "qn" and "dt", where row "qn" stores the number of queuing cars in each station
# row "dt" stores processing times for the first cars in each queue
# "queue": the queue in the station
# "tr" and "tm": the processing time will be within (tm, tr+tm)
finish <- function(station, queue, tr, tm){
  
  # Reduce one from the number of queuing cars in this queue if there is one car finished being processed
  station["qn", queue] <- station["qn", queue] - 1
  
  # If there are more cars in the queue ready for being processed,
  # use 'sample()' to generate a processing time for the first car in the queue
  if(station["qn", queue] > 0){
    station["dt", queue] <- sample(tm:(tm+tr), 1) 
  }
  # If there is no car left in the queue, we give "-1" as an indicator, just for convenience 
  else{
    station["dt", queue] <- -1
  }
  # "finish()" function will return updated "station" matrix
  return(station)
}



# Define another function "insert()" to find the shortest queue 
# and insert the newly arriving car into the queue
# If this newly arriving car will be put into a queue without any cars waiting,
# this car will also be given a processing time within (tm, tr+tm)
insert <- function(station, tr, tm){
  # Find the shortest queue
  qn_min <- which.min(station["qn", ]) 
  # The newly arrived car will join the shortest queue, so the number of queuing cars will increase 1
  station["qn", qn_min] <- station["qn", qn_min] + 1
  
  # According to "finish()" function we had defined above, 
  # "-1" for processing time means there's no car waiting in the queue, 
  # so we use "sample()" function to generate a processing time for this car
  if(station["dt", qn_min] == -1){
    station["dt", qn_min] <- sample(tm:(tm+tr), 1)
  }
  # "insert()" function will return updated "station" matrix
  return (station)
}




t_total <- 2*60*60 # Total simulation time will be 2 hours, equally 7200 seconds
t_newcar <- 1.5*60*60 # New cars will only enter French station in the first 1.5 hours




# Define function "qsim(mf,mb,a.rate,trb,trf,tmb,tmf,maxb)" to simulate the process the cars enter the French stations,
# then move to British stations from French ones after finishing being processing, 
# and finally leave British station after being processed

# "mf" is the number of French stations
# "mb" is the number of British stations
# "a.rate" is the probability a new car arrives at French stations in one second
# The processing time in the French stations will be a random number uniformly distributed between (tmf,tmf+trf) 
# The processing time in the British stations will be a random number uniformly distributed between (tmb,tmb+trb) 
# The maximum number of cars in each British station will be "maxb"

qsim <- function(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) {
  
  nf <- c() # nf is a list to store the average queuing length in French stations for each second
  nb <- c() # nb is a list to store the average queuing length in British stations for each second
  eq <- c() # eq is a list to store the expected waiting time for each second
  
  # Create a matrix "f" with "mf" columns as French stations, and two rows "dt" and "qn"
  # Where "qn" stores the queuing length in each station and "dt" have the processing time for the first car in each station
  column_names_f <- paste0("station_", seq_len(mf))
  # "-1" is the indicator showing the queue is empty
  f <- matrix(c(0,-1), nrow = 2, ncol = length(column_names_f))
  colnames(f) <- column_names_f
  rownames(f) <- c("qn","dt")
  # The same idea to create matrix "b" for British stations
  column_names_b <- paste0("station_", seq_len(mb))
  b <- matrix(c(0,-1), nrow = 2, ncol = length(column_names_b))
  colnames(b) <- column_names_b
  rownames(b) <- c("qn","dt")
  
  # Before the simulation starts, the time having passed is set to be zero
  t_passed <- 0
  
  # while loop for t_total simulation time
  while(t_passed < t_total){
    print("t")
    print(t_passed)
    print("f")
    print(f)
    print("b")
    print(b)
    
    # Sort the processing time "dt" in the station matrix and pick out the smallest one
    min_processing_t <- min(min(f["dt", ]), min(b["dt", ]))
    
    if (min_processing_t > 0){
      for (s in 1:min_processing_t){
        if(t_passed < t_newcar){
          # During this time, there might be new cars arriving in the French stations if it's in the first 1.5 hours simulation time
          # Use "sample()" to simulate whether there is new car arrived
          # "True" stands for a new car arrived and "False" means not
          # The probability for a new car arrived in one second will be "a.rate", and "1-a.rate" for there is no car arriving
          if (sample(c(TRUE,FALSE), size = 1, prob = c(a.rate, 1-a.rate))){
            # If there is a new car arrived,
            # Average queue length in French station will increase 1/mf
            # Expected processing time will increase 1/mf*(tmf + trf/2)
            f["qn", which.min(f["qn", ])] <- f["qn", which.min(f["qn", ])] + 1
            nf[[length(nf) + 1]] <- nf[[length(nf)]] + 1/mf 
            eq[[length(eq) + 1]] <- eq[[length(eq)]] + 1/mf*(tmf + trf/2) 
            t_passed <- t_passed + 1 # One second has passed
          }else{
            # The average queuing length in French stations and expected processing time won't change if there is no car arrived
            nf[[length(nf) + 1]] <- nf[[length(nf)]]
            eq[[length(eq) + 1]] <- eq[[length(eq)]]
            t_passed <- t_passed + 1
          }
        }else{
          # There won't be new cars arriving after t_passed is above 1.5 hours
          # "nf" and "eq" won't change for min_processing_t-s+1 this period time
          nf<- c(nf, rep(nf[[length(nf)]],min_processing_t - s + 1))
          eq<- c(eq, rep(eq[[length(eq)]],min_processing_t - s + 1))
          t_passed <- t_passed + min_processing_t - s + 1 # min_processing_t - s + 1 seconds have passed
          break
        }
      }
    
    # Before the shortest processing time passed, there won't be cars moving into or out from British stations,
    # which means the average queuing length in British stations will be the same during this minimum processing time
    # So we append the "nb" list with same numbers
    nb <- c(nb, rep(nb[[length(nb)]],min_processing_t))
    # Cut these passed time off from processing time
    f["dt", ] <- f["dt", ] - min_processing_t
    b["dt", ] <- b["dt", ] - min_processing_t
    
    # If the minimum processing time is zero, which means there will be a car moving
    # So all of nf, nb and eq will change
    # And we need "insert()" and "finish()" functions to help move cars out of the queue and insert new cars
    }else{
      # Notice: Since the cars can't leave from the French station to the British one if there is no available queues in British station, 
      # so we start with analyzing the queues in British station
      # Find the columns that with 0 processing time in British stations
      # Then move those cars with 0 processing time out of the queues using "finish()" function to cut them off the queue and give a processing time to the next car
      for( i in which(b["dt", ] %in% 0)){
        b <- finish(b, i, trb, tmb)
      }
      # For the cars with 0 processing time in French stations,
      # Use "finish()" to move car out of French stations and use "insert()" to move them into British stations
      for (i in which(f["dt", ] %in% 0)){
        # Cars can only move from French stations to British ones 
        # if the number of cars queuing in the British stations is less than its maximum capacity 
        if(sum(b["qn", ]) < maxb*mb){
        f <- finish(f, i, trf, tmf)
        b <- insert(b, trb, tmb)
        }
      }
      # New cars arrive at French stations in the first 1.5 hours
      if (t_passed < t_newcar){
        if (sample(c(TRUE,FALSE), size = 1, prob = c(a.rate, 1-a.rate))){
          # If "sample()" function generates "TURE" as the result, 
          # which means a new car arrives at the French stations
          # We will use "insert()" defined above to put a new car in the queue in the French station
          f <- insert(f, trf, tmf)
        }
      }
      
      # One iteration in this loop means one second has passed, so we cut one second off from the positive processing time for each car
      f["dt", ] <- apply(f["dt", , drop = FALSE], 1, function(row) {
        ifelse(row > 0, row - 1, row)
      })
      b["dt", ] <- apply(b["dt", , drop = FALSE], 1, function(row) {
        ifelse(row > 0, row - 1, row)
      })
      
      t_passed <- t_passed + 1
      
      # avg_f is the average queue length in the French stations
      avg_f <- sum(f["qn", ])/mf
      # avg_b is the average queue length in the British stations
      avg_b <- sum(b["qn", ])/mb 
      
      # Update the average queue length and expected waiting time for this second to the lists storing results across the simulation
      nf[[length(nf) + 1]] <- avg_f
      nb[[length(nb) + 1]] <- avg_b
      eq[[length(eq) + 1]] <- avg_f*(tmf + trf/2) + avg_b*(tmb + trb/2)
    }
  }
  # "qsim()" function will return a list including 3 sublists
  # nf is a list to store the average queue length in French station for each second
  # nb is a list to store the average queue length in British station for each second
  # eq is a list to store the expected waiting time for each second
  return(list(nf, nb, eq))
}

iqsim <- qsim() # Default parameters simulation result
jqsim <- qsim(tmb = 40) # Set the minimum processing time in British station as 40 simulation result

# Create a panel with 2-row, 2-row plots
par(mfrow = c(2,2))

# Plot of French and British queue lengths changing over time
matplot(1:7200, cbind(iqsim[[1]],iqsim[[2]]), type = 'l', col = c("red", "blue"), 
        xlab = "Time", ylab = "Average queuing length", 
        main = "Average queuing length of French and British Stations changing over time")
legend("topright", c("French", "British"), col = c("red", "blue"), lty = 1:2, cex = 0.6)

# Plot of expected waiting time changing over time
matplot(1:7200, iqsim[[3]], type = 'l', xlab = "Time", ylab = "Expected queuing time", 
        main = "Expected waiting time changing over time")

# Plot of French and British queuing lengths changing over time when the minimum British handling time is set as 40 seconds
matplot(1:7200, cbind(jqsim[[1]],jqsim[[2]]), type = 'l', col = c("red", "blue"), 
        xlab = "Time", ylab = "Average queuing length", 
        main = "Average queuing length of French and British Stations changing over time when tmb = 40 ")
legend("topleft", c("French", "British"), col = c("red", "blue"), lty = 1:2, cex = 0.6)

# Plot of expected waiting time changing over time when the minimum British handling time is set as seconds
matplot(1:7200, jqsim[[3]], type = 'l', xlab = "Time", ylab = "Expected queuing time", 
        main = "Expected waiting time changing over time when tmb = 40")



# Probability of at least one car missing the ferry departure = 
# the total number of times nf>0 or nb>0 / simulation times
# n_missing stores the total number of times with positive queuing lengths after 2 hours
n_missing <- 0 

for (i in 1:100){
  nqsim <- qsim(tmb = 40)
  if (nqsim[[2]][7200] > 0 || nqsim[[1]][7200] > 0){
    n_missing <- n_missing + 1
  }
}

# Print out the probability
print(paste0("The probability of at least one car missing the ferry departure is ", n_missing/100))





















