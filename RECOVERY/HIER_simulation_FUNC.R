#### Simulating data for parameter recovery ####

hier_cc_sim <- function(nsubjects, mu_alpha, mu_rho, mu_omega, sigma_alpha, sigma_rho, sigma_omega){ 
  ### mu = the group mean of the given parameter
  ### sigma = the standard deviation (variance) of that parameter 
  
  ntrials <- 12
  
  ######## THE COMPUTER CONTRIBUTIONS OF TWO SUBJECTS - Ga
  trials <- array(1:12)
  s1_cont <- c(9,9,9,9,3,0,0,0,9,9,9,9)
  s2_cont <- c(9,6,6,6,0,0,0,0,3,0,0,0)
  matrix_con <- matrix(c(s1_cont,s2_cont), 12, 2)
  real_Ga <- rowMeans(matrix_con)
  
  ##### empty arrays to fill for all subjects: 
  contri <- array(NA, c(nsubjects, ntrials))
  
  
  #### sampling the parameters based on the given mus and sigmas for each subject:
  #### these are rather narrow so the group means aren't all over the place and remain realistic
  for (s in 1:nsubjects){
    alpha <- rtruncnorm(1,2,5, mu_alpha, sigma_alpha)
    rho <- rtruncnorm(1, .1, .9, mu_rho, sigma_rho)
    omega <- rtruncnorm(1,.1,.9, mu_omega, mu_omega)
  
      ## empty array fo Gb
      Gb <- array(NA, 12)
      
      ## Gb for first trial
      Gb[1] <- rpois(1,alpha)
      
      # simulating behavior for subject s in each trial
      for (t in 1:12) {
        
        if (t != 1){
          
          # calculate Gb (belief about group contribution)
          Gb[t] <- ((1-omega)*(Gb[t-1]))+(omega*(real_Ga[t-1]))
          
          # calculate poisson parameter
          p <- rho*Gb[t]
          
          # sample contribution from poisson
          contri[s, t] <- rpois(1,p) ###  dictates the initial contribution of others (should be 0,3,6,9) => can either be manipulated somehow ?? or 0,1,2,3 and the rescaled. 
        }
        
        else {
          
          # calculate poisson parameter - first trial
          p <- rho*Gb[1]
          
          # sample contribution from poisson - first trial
          contri[s, t] <- rtpois(1,p,0,4)
        }
      }
    }
  
  return(contri)
}    