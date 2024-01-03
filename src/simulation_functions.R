library(extraDistr)
library(IMIFA)
library(cascsim)
library(truncnorm)

compute_Ga <- function(ntrials=ntrials){
  # calculate Ga: trial-wise average contribution of other players
  s1_cont <- c(9,9,9,9,3,0,0,0,9,9,9,9)
  s2_cont <- c(9,6,6,6,0,0,0,0,3,0,0,0)
  matrix_cont <- matrix(c(s1_cont,s2_cont), 12, 2)
  Ga <- rowMeans(matrix_cont)
  
  # repeat Ga according to ntrials
  if (ntrials != 12){
    rest <- round(((ntrials/12 - floor(ntrials/12))*12), 1)
    if (rest != 0){
      Ga <- c(rep(Ga, floor(ntrials/12)), Ga[1:rest])
    }
    else {
      Ga <- rep(Ga, ntrials/12)
    }
  }
  
  return(Ga)
}

#### SUBJECT SIMULATION

cc_sim <- function(nsub, alpha, rho, omega, Ga){
  
  # number of trials
  ntrials <- length(Ga)
  
  # empty array for simulated subject contributions 
  c <- array(NA, c(nsub, ntrials))
  
  # empty array for trial-wise belief about others' contribution
  Gb <- array(NA, c(nsub, ntrials))
  
  # lengthen Ga if needed
  if (ntrials != 12){
    rest <- round(((ntrials/12 - floor(ntrials/12))*12), 1)
    Ga <- c(rep(Ga, floor(ntrials/12)), Ga[1:rest])
  }
  
  for (s in 1:nsub){
  
  # Gb for first trial
  Gb[s,1] <- extraDistr::rtpois(1,alpha[s],-1,3)
  
  # contribution for first trial
  p <- rho[s]*Gb[s,1]
  c[s,1] <- extraDistr::rtpois(1,p,-1,3)
  
    # simulating subject behavior in remaining trials
    for (t in 2:ntrials) {
      
        # calculate Gb
        Gb[s,t] <- ((1-omega[s])*(Gb[s, t-1]))+(omega[s]*(Ga[t-1]))
        
        # calculate poisson parameter
        p <- rho[s]*Gb[s,t]
        
        # sample contribution from poisson
        c[s,t] <- extraDistr::rtpois(1,p,-1,3)
    }
  }
  
  data <- list(c = c, Gb = Gb)
  
  return(data)
  
}


#### GROUP SIMULATION 

group_cc_sim <- function(nsub, mu_alpha, mu_rho, mu_omega, Ga){ 
  
  # alpha
  tau_alpha <- 1 # from dgamma(0.01,0.01)
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
    
  # sample parameters
  alpha <- IMIFA::rltrgamma(nsub, shape_alpha, rate_alpha, trunc=0.001)
  rho <- cascsim::rtbeta(nsub, shape1_rho, shape2_rho, min=0.001, max=0.999) 
  omega <- cascsim::rtbeta(nsub, shape1_omega, shape2_omega, min=0.001, max=0.999)
  
  # simulating the subject's behavior based on the sampled parameters
  sim <- cc_sim(nsub, alpha, rho, omega, Ga)
  
  # sample contribution from poisson
  group_contributions <- sim$c
  
  return(group_contributions)
}    



group_cc_sim_no_reparam <- function(nsub, mu_alpha, mu_rho, mu_omega, Ga){ 
  
  # sample parameters
  alpha <- truncnorm::rtruncnorm(nsub, a=0, b=10, mean=mu_alpha, sd=1)
  rho <- truncnorm::rtruncnorm(nsub, a=0.001, b=0.999, mean=mu_rho, sd=.1)
  omega <- truncnorm::rtruncnorm(nsub, a=0.001, b=0.999, mean=mu_omega, sd=.1)
  
  # simulating the subject's behavior based on the sampled parameters
  sim <- cc_sim(nsub, alpha, rho, omega, Ga)
  
  # sample contribution from poisson
  group_contributions <- sim$c
  
  return(group_contributions)
}    