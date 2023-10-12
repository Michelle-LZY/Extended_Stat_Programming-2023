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
  start <- c()
  # "f" is a data.frame to store the processing time for cars in the French stations
  # Choosing to use a data.frame to store the processing time to avoid messing up each station
  # Columns in "f" stand for each French station, namely "station_1", "station_2", ..., "station_mf"
  # The first row in "f" stores the number of cars waiting in the station
  # The second row in "f" stores the processing time for each car
  column_names_f <- paste0("station_", seq_len(mf))
  f <- data.frame(matrix(0,ncol=length(column_names_f), nrow=2))
  colnames(f) <- column_names_f
  rownames(f) <- c("qn","dt")
  print(f)
  # "b" is a data.frame to store the processing time for cars in the British stations
  # Same ideas to build "b"
  column_names_b <- paste0("station_", seq_len(mb))
  b <- data.frame(matrix(0,ncol=length(column_names_b), nrow=2))
  colnames(b) <- column_names_b
  rownames(b) <- c("qn","dt")
  print(b)
  
  
  # This model will simulate for a 2-hour period, in total 7200 seconds
  for (t in 1:7200){
    # Provided "a.rate" this input as the arriving probability, we use "sample()" 
    # function to randomly choose "TRUE" or "FALSE"
    # "TURE" indicates a car arrives while "False" indicating no cars arrive
    # Ignoring the situation that more than one car arriving at the same time, so 
    # "size" attribute in "sample()" function is 1
    if (t<=5400){ # New cars only arrive at French stations in the first 1.5 hours
      if (sample(c(TRUE,FALSE), size = 1, prob = c(a.rate,1-a.rate))){
        # If "sample()" function generates "TURE" as the result, which means a new car 
        # arrives at the French stations, we will add a time stamp to the "start" list
        start.append(start,t)
        
        
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
  }
  return(c(nf,nb,eq))
}
qsim()