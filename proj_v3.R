# Group members: Bo Gao(s2511232), Zheyue Lin(s2519324) and Xinran Zhu(s2508695)
# Bo Gao(s2511232) and Zheyue Lin(s2519324) worked together to design the function "qsim()"
# Xinran Zhu(s2508695) worked on plots showing how average queuing lengths and expected waiting time changing over time and calculating the probability of at least one car missing the ferry


# Define a function "finish()" to move a car out of the queue after finishing being processed
# Also, "finish()" will give a processing time to the next car in the queue after the previous one got moved
# "station_qn": queue's car number of French stations f(f_qn) or British stations b(b_qn)
# "station_dt": queue's dealing time of French stations f(f_dt) or British stations b(b_dt)
# "queue": the queue in the station
# "tr" and "tm": the processing time will be within (tm, tr+tm)
finish <- function(station_qn, station_dt, queue, tr, tm){
  
  # Reduce one from the number of queuing cars in this queue if there is one car finished being processed
  station_qn[queue] <- station_qn[queue] - 1
  
  # If there are more cars in the queue ready for being processed,
  # use 'sample()' to generate a processing time for the first car in the queue
  if(station_qn[queue] > 0){
    station_dt[queue] <- sample(tm:(tm + tr), 1) 
  }
  
  # If there is no car left in the queue, we give "-1" as an indicator, just for convenience 
  else{
    station_dt[queue] <- -1
  }
  
  # "finish()" function will return updated "station_qn" and "station_dt" list
  return (list(qn = station_qn, dt = station_dt))
}

# Define another function "insert()" to find the shortest queue 
# and insert the newly arriving car into the queue
# If this newly arriving car will be put into a queue without any cars waiting,
# this car will also be given a processing time within (tm, tr+tm)
insert <- function(station_qn, station_dt, tr, tm){
  
  # Find the shortest queue
  qn_min <- which.min(station_qn) 
  # The newly arrived car will join the shortest queue, so the number of queuing cars will increase 1
  station_qn[qn_min] <- station_qn[qn_min] + 1
  
  # According to "finish()" function we had defined above, 
  # "-1" for processing time means there's no car waiting in the queue, 
  # so we use "sample()" function to generate a processing time for this car
  if(station_dt[qn_min] == -1){
    station_dt[qn_min] <- sample(tm:(tm + tr), 1)
  }
  
  # "insert()" function will return updated "station" matrix
  return (list(qn = station_qn, dt = station_dt))
}

t_total <- 2*60*60 # Total simulation time will be 2 hours, equally 7200 seconds
t_newcar <- 3*60*60/2 # New cars will only enter French station in the first 1.5 hours

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
  
  nf <- c() # nf is a list to store the average queuing length in French stations for each second
  nb <- c() # nb is a list to store the average queuing length in British stations for each second
  eq <- c() # eq is a list to store the expected waiting time for each second
  
  # Create two lists "f_qn" and "f_dt" with "mf" columns for French stations
  # Where "f_qn" stores the queuing length in each station and "f_dt" have the processing time for the first car in each station
  # And create two lists "b_qn" and "b_dt" with "mb" columns for British stations
  # Where "b_qn" stores the queuing length in each station and "b_dt" have the processing time for the first car in each station
  f_qn <- rep(0, mf)
  b_qn <- rep(0, mb)
  f_dt <- rep(-1, mf)
  b_dt <- rep(-1, mb)
  
  # This model will simulate for a 2-hour period, in total seconds
  for (t in 1:t_total){
    
    # Notice: Since the cars can't leave from the French station to the British one if there is no available queues in British station, so we start with analyzing the queues in British station
    # First, find the columns that with 0 processing time in British station
    # Then move those cars with 0 processing time out of the queue using "finish()" function to cut one off the queue and give a processing time to the next car
    # Finally, update "b_qn" and "b_dt" list
    for(i in which(b_dt %in% 0)){
      bi <- finish(b_qn, b_dt, i, trb, tmb)
      b_qn <- bi$qn
      b_dt <- bi$dt
    }
    
    # Find the columns with 0 processing time in the French station
    for (i in which(f_dt %in% 0)){
      # If the sum of the number of cars in the queue is less than the total maximum capacity for the whole British station, 
      # we will know there are still some vacancies in the British station
      # Then we can move the cars finished processing from French station to the British station using "finish()" function we defined above
      # And update "f_qn", "f_dt", "b_qn" and "b_dt"
      if(sum(b_qn) < maxb*mb){
        fi <- finish(f_qn, f_dt, i, trf, tmf)
        f_qn <- fi$qn
        f_dt <- fi$dt
        bi <- insert(b_qn, b_dt, trb, tmb)
        b_qn <- bi$qn
        b_dt <- bi$dt
      }
    }
    
    # Provided "a.rate" this input as the arriving probability, we use "sample()" function to randomly choose "TRUE" or "FALSE"
    # "TURE" indicates a car arrives while "False" indicating that there are no cars arrived
    # Ignoring the situation that more than one car arriving at the same time, so "size" attribute in "sample()" function is 1
    if (t <= t_newcar){ # New cars only arrive at French stations in the first 1.5 hours in the simulation
      if (sample(c(TRUE,FALSE), size = 1, prob = c(a.rate, 1-a.rate))){
        # If "sample()" function generates "TURE" as the result, which means a new car arrives at the French stations
        # We will use "insert()" defined above to put a new car in the queue in the French station
        # Then update "f_qn" and "f_dt"
        fi <- insert(f_qn,f_dt, trf, tmf)
        f_qn <- fi$qn
        f_dt <- fi$dt
      }
    }
    
    # One iteration in this loop means one second has passed, so we cut one second off from the positive processing time for each car
    f_dt[f_dt > 0] <- f_dt[f_dt > 0] - 1
    b_dt[b_dt > 0] <- b_dt[b_dt > 0] - 1
    
    
    # avg_f is the average queue length in the French stations
    avg_f <- sum(f_qn)/mf
    # avg_b is the average queue length in the British stations
    avg_b <- sum(b_qn)/mb 
    # Update the average queue length and expected waiting time for this second to the lists storing results across the simulation
    nf[[length(nf) + 1]] <- avg_f
    nb[[length(nb) + 1]] <- avg_b
    eq[[length(eq) + 1]] <- avg_f*(tmf + trf/2) + avg_b*(tmb + trb/2)
  }
  
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
  kqsim <- qsim(tmb = 40)
  if (kqsim[[1]][7200] > 0 || kqsim[[2]][7200] > 0){
    n_missing <- n_missing + 1
  }
}

# Print out the probability
print(paste0("The probability of at least one car missing the ferry departure is ", n_missing/100))