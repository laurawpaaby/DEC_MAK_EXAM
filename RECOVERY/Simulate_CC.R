
# TO DO 
# - array structure 
# - when to simulate what parameters 


######## THE COMPUTER CONTRIBUTIONS OF TWO SUBJECTS - Ga
trials <- array(1:12)
s1_cont <- c(9,9,9,9,3,0,0,0,9,9,9,9)
s2_cont <- c(9,6,6,6,0,0,0,0,3,0,0,0)
matrix_con <- matrix(c(s1_cont,s2_cont), 12, 2)

real_Ga <- rowMeans(matrix_con)



#### Simulating data for parameter recovery ####
hier_cc_sim <- function(ntrials, alpha, omega, rho){
  
  # empty arrays to be filled 
  #Gb <- array(NA,c(groupSize, ntrials,ngroups))
  #Ga <- array(NA, c(groupSize, ntrials,ngroups))
  #p <- array(NA,c(groupSize, ntrials,ngroups)) #preferences
  #c <- array(NA,c(groupSize, ntrials,ngroups, 1)) #contribution
  
  ## empty array for simulated child 
  contri <- array(NA,12)
  
  ## empty array fo Gb
  Gb <- array(NA, 12)
  
  ## Gb for first trial
  Gb[1] <- rpois(alpha)
  
  # simulating behavior for subject s in each trial
  for (t in 1:12) {
    
    if (t != 1){
      
      # calculate Gb (belief about group contribution)
      Gb[t] <- ((1-omega)*(Gb[t-1]))+(omega*(Ga[t-1]))
      
      # calculate poisson parameter
      p <- rho[s]*Gb[t]
      
      # sample contribution from poisson
      contri[t] <- dpois(p)
    }
    
    else {
      
      # calculate poisson parameter - first trial
      p <- rho[s]*Gb[1]
      
      # sample contribution from poisson - first trial
      contri[t] <- dpois(p)
    }
    
  }
  return(contri)
}    