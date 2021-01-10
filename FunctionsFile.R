


#Variables for the simulation #
steps <- 100000 #total number of time steps
lever <- 0 #vector to save responses to the lever
searchmode <- 0 #type of search in every trial [1 = global or disengage; 2 = focal or visit]
Data <- data.frame() #dataframe to save the the outcome step by step
BoutFrequency <- data.frame() #dataframe to save the the relative frequency of every bout length
Irts <- data.frame() #dataframe to save the the relative frequency of IRTs



##### Global Search Mode ##### - Disengage
xDisengage <- function(q, i){
  # when disengage, lever press with probability q 
  lever[i] <<- ifelse(runif(1) <= q, 1, 0)
  searchmode[i] <<- 1
}

##### Focal Search Mode ##### - Visit
xVisit <- function (w, i) {
  # when visit, lever press with probability w
  lever[i] <<- ifelse(runif(1) <= w, 1, 0)
  searchmode[i] <<- 2
}

##### Simulation ##### 
xBehavSys <- function(d, b, w, q){
  set.seed(2020)
  for (i in 1:steps) { #first trial enters either mode with p = .5
    if (i == 1) {
      # in the very first step, enter either mode with probability = .5
      if (runif(1) > .5) { 
        xDisengage(q, i) # go to Disengage
      } else { 
        xVisit(w, i) # go to Visit
      }
    } else { #All other trials
      if (searchmode[i-1] == 1) { 
        #if the previous step was at Disengage, go to Visit with probabolity = b,
        # and stay in Disengage with probability = 1 - b
        ifelse(runif(1) <= b, xVisit(w, i), xDisengage(q, i))  
      } else {
        #if the previous step was at Visit, go to Disengage with probabolity = d,
        # and stay in Visit with probability = 1 - d
        ifelse(runif(1) <= d, xDisengage(q, i), xVisit(w, i))
      } 
    }
  }
  Data <<- cbind(searchmode, lever) #Save the data in a dataframe
  write.table(Data, "TimeStepData.txt", sep = "\t", dec = ".", row.names = F) #save the data in a .txt file
  write.csv(Data, "TimeStepData.csv") #save the data in a .csv file
  
  # Loops to count the IRTs and response bouts 
  countIrt <- 0
  countPause <- 0
  boutLength <- 0
  countRs <- 0
  countLever <- 0
  pauseLength <- 0
  addRs <- 0
  addBout <- 0
  cumRs <- 0
  csIrt <- 0
  
  for (i in 1:length(lever)) { #to calculate the IRTs survival
    if (lever[i] == 0)  { # when no lever press occurs
      countIrt <- countIrt + 1 #IRT counter 
      countPause <- countPause + 1 #pause counter
      boutLength[countPause] <- countRs #save the legth of the bout
      countRs <- 0 #reset bout counter
    } else { # when a lever press occurs 
      countLever <- countLever + 1 #lever press counter 
      pauseLength[countLever] <- countIrt #save IRT
      countIrt <- 0 # reset the IRT counter
      countRs <- countRs + 1 #bout response counter
    } 
  }
  
  #---#For the IRT Survival analysis
  Pauses <- unique(pauseLength) #IRTs
  Freq <- sapply(Pauses, function(x) length(which(pauseLength == x))) #Frequency of each IRT
  Irts <<- as.data.frame(cbind(Pauses, Freq)) #put together
  
  for (i in 1:length(Pauses)){ #calculate the cumulative sum of the frequency of each irt
    csIrt[i] <- sum(Freq[which(Pauses >= Pauses[i])])
  }
  
  Irts <<- cbind(Irts, csIrt) #put together
  Irts <<- mutate(Irts, pSurvival = csIrt/csIrt[which(Pauses == 0)]) #calculate probability of survival at every time step
  
  write.table(Data, "IRTSurvival.txt", sep = "\t", dec = ".", row.names = F) #save the data in a .txt file
  write.csv(Data, "IRTSurvival.csv") #save the data in a .csv file
  
  #---#For the response bouts  
  for (i in 2:length(lever)) { #loop to count the IRTs and response bouts
    if (searchmode[i-1] == searchmode[i])  {
      #accumulate lever presses within a mode
      addRs <- addRs + lever[i] 
    } else {
      #reset the counter when entering a mode
      addBout <- addBout + 1 #counter of bouts
      cumRs[addBout] <- addRs # total of responses in a bout
      addRs <- 0 #reset lever press counter
    }
  }
  ABout <- unique(cumRs)
  ABoutFreq <- sapply(ABout, function(x) length(which(cumRs == x))) #Frequency of each Alternative Bout length
  BoutFrequency <<- as.data.table(cbind(ABout, ABoutFreq)) # put together
  BoutFrequency <<- mutate(BoutFrequency, RelBF = ABoutFreq/sum(ABoutFreq[which(ABout > 0)]))
  write.table(Data, "BoutFrequency.txt", sep = "\t", dec = ".", row.names = F) #save the data in a .txt file
  write.csv(Data, "BoutFrequency.csv") #save the data in a .csv file
}  