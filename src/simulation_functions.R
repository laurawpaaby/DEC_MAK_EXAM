library(extraDistr)
library(IMIFA)
library(cascsim)

compute_Ga <- function(){
  # calculate Ga: trial-wise average contribution of other players
  s1_cont <- c(9,9,9,9,3,0,0,0,9,9,9,9)
  s2_cont <- c(9,6,6,6,0,0,0,0,3,0,0,0)
  matrix_cont <- matrix(c(s1_cont,s2_cont), 12, 2)
  Ga <- rowMeans(matrix_cont)
  
  return(Ga)
}

#### SUBJECT SIMULATION

cc_sim <- function(nsub, alpha, rho, omega, Ga){
  
  # empty array for simulated subject contributions 
  c <- array(NA, c(nsub, 12))
  
  # empty array for trial-wise belief about others' contribution
  Gb <- array(NA, c(nsub, 12))
  
  
  for (s in 1:nsub){
  
  # Gb for first trial
  Gb[s,1] <- extraDistr::rtpois(1,alpha[s],-1,3)
  
  # contribution for first trial
  p <- rho*Gb[s,1]
  c[s,1] <- extraDistr::rtpois(1,p,-1,3)
  
    # simulating subject behavior in remaining trials
    for (t in 2:12) {
      
        # calculate Gb
        Gb[s,t] <- ((1-omega[s])*(Gb[s, t-1]))+(omega[s]*(Ga[t-1]))
        
        # calculate poisson parameter
        p <- rho*Gb[s,t]
        
        # sample contribution from poisson
        c[s,t] <- extraDistr::rtpois(1,p,-1,3)
    }
  }
  
  data <- list(c = c, Gb = Gb)
  
  return(data)
  
}


#### GROUP SIMULATION 

group_cc_sim <- function(nsubjects, mu_alpha, mu_rho, mu_omega, Ga){ 
  # nsubjects: number of subjects to simulate
  # mu: the group mean of the given parameter
  
  # empty arrays to fill for all subjects: 
  group_contributions <- array(NA, c(nsubjects, 12))
  
  # alpha
  tau_alpha <- 1 # mean of dgamma(0.01,0.01)
  sigma_alpha <- 1/sqrt(tau_alpha) 
  
  rate_alpha <- ( mu_alpha + sqrt( mu_alpha^2 + 4*sigma_alpha^2 ) )/
    (2*sigma_alpha^2) 
  shape_alpha <- 1 + mu_alpha * rate_alpha
  
  # rho
  sigma_rho <- 50 # from dunif(1,100)
  
  shape1_rho <- (mu_rho) * sigma_rho
  shape2_rho <- (1 - mu_rho) * sigma_rho
  
  # omega
  sigma_omega <- 50 # from dunif(1,100)
  
  shape1_omega <- (mu_omega) * sigma_omega
  shape2_omega <- (1 - mu_omega) * sigma_omega
  
  # sampling the parameters based on the given mus and sigmas for each subject:
  for (s in 1:nsubjects){
    
    # sample parameters
    alpha <- IMIFA::rltrgamma(1, shape_alpha,rate_alpha, trunc=0.001)
    rho <- cascsim::rtbeta(1, shape1_rho,shape2_rho, min=0.001, max=0.999) 
    omega <- cascsim::rtbeta(1, shape1_omega,shape2_omega, min=0.001, max=0.999)
    
    # simulating the subject's behavior based on the sampled parameters
    s_contributions <- cc_sim(alpha, rho, omega, Ga)
    
    # sample contribution from poisson
    group_contributions[s,] <- s_contributions
    }
  
  return(group_contributions)
}    