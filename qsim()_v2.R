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
  # so the newly inserted car will be processed immediately.
  # Then we use "sample()" function to generate a random processing time for this car
  if(station["dt", qn_min] == -1){
    station["dt", qn_min] <- sample(tm:(tm+tr), 1)
  }
  return (station)
}

t_1<-2*60*60
t_2<-1.5*60*60

qsim <- function(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20) {
  
  nf <- c() # nf is a list to store the average queue length in French station for each second
  nb <- c() # nb is a list to store the average queue length in British station for each second
  eq <- c() # eq is a list to store the expected waiting time for each second
  
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
  
  t_passed <- 0
  
  while(t_passed<t_1){
    # Sort the processing time "dt" in the station matrix and pick out the smallest one
    # 选出最短处理时间
    min_processing_t <- min(min(f["dt",]),min(b["dt",]))
    # 最短处理时间还没过之前， 不会有车移出车站，nb一直不变，所以nb直接在向量尾后延长min_processing_t个同样的数
    # nf和eq有可能在前1.5小时变大因为有新车进French station
    if (min_processing_t>0){
      for (s in 1:min_processing_t){
        if(t_passed < t_2){#只有前1.5小时有新车进来
          if (sample(c(TRUE,FALSE), size = 1, prob = c(a.rate, 1-a.rate))){
            f["qn", which.min(f["qn", ])] <- f["qn", which.min(f["qn", ])] + 1
            #GPT老师说这个写法比用append更新list快，因为只更新list新添加的元素不用复制整个list
            #进来1个新车, nf增加1/mf, eq增加1/mf*(tmf + trf/2)
            nf[[length(nf)+1]] <- nf[[length(nf)]]+1/mf 
            eq[[length(eq)+1]] <- eq[[length(eq)]]+1/mf*(tmf + trf/2) 
            t_passed <- t_passed + 1
          }else{#没有新车的时候nf,eq都不变
            nf[[length(nf)+1]] <- nf[[length(nf)]]
            eq[[length(eq)+1]] <- eq[[length(eq)]]
            t_passed <- t_passed + 1
          }
        }else{
          # t_passed超过1.5小时之后，不会有新车进入，nf,eq都不变，在向量尾后延长(min_processing_t-s+1)个同样的数
          nf<- c(nf, rep(nf[[length(nf)]],min_processing_t-s+1))
          eq<- c(eq, rep(eq[[length(eq)]],min_processing_t-s+1))
          t_passed <- t_passed + min_processing_t - s + 1
          break
        }
      }
      nb <- c(nb, rep(nb[[length(nb)]],min_processing_t))
      f["dt", ] <- f["dt", ] - min_processing_t
      b["dt", ] <- b["dt", ] - min_processing_t
    }else{ #这部分就和之前的一样
      for( i in which(b["dt", ] %in% 0)){
        b <- finish(b, i, trb, tmb)
      }
      for (i in which(f["dt", ] %in% 0)){
        if(sum(b["qn", ]) < maxb*mb){
        f <- finish(f, i, trf, tmf)
        b <- insert(b, trb, tmb)
        }
      }
      if (t_passed< t_2){
        if (sample(c(TRUE,FALSE), size = 1, prob = c(a.rate, 1-a.rate))){
          # If "sample()" function generates "TURE" as the result, which means a new car arrives at the French stations
          # We will use "insert()" defined above to put a new car in the queue in the French station
          f <- insert(f, trf, tmf)
        }
      }
      f["dt", ] <- apply(f["dt", , drop = FALSE], 1, function(row) {
        ifelse(row > 0, row - 1, row)
      })
      b["dt", ] <- apply(b["dt", , drop = FALSE], 1, function(row) {
        ifelse(row > 0, row - 1, row)
      })
      t_passed <- t_passed + 1
      
      avg_f <- sum(f["qn", ])/mf
      # avg_b is the average queue length in the British station
      avg_b <- sum(b["qn", ])/mb 
      # Update the average queue length and expected waiting time for this second to the lists storing results across the simulation
      nf[[length(nf)+1]] <- avg_f
      nb[[length(nb)+1]] <- avg_b
      eq[[length(eq) + 1]] <- avg_f*(tmf + trf/2) + avg_b*(tmb + trb/2)
    }
  }
  return(list(nf, nb, eq))
}






















