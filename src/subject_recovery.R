#### SIMULATE SUBJECTS AND RECOVER SUBJECT-LEVEL PARAMETERS ####

# load libraries
library(R2jags)
library(extraDistr)
library(tidyverse)
library(ggplot2)

source(simulation_functions.R)
source(plot_functions.R)

set.seed(626)

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

# calculate Ga: trial-wise average contribution of other players
s1_cont <- c(9,9,9,9,3,0,0,0,9,9,9,9)
s2_cont <- c(9,6,6,6,0,0,0,0,3,0,0,0)
matrix_cont <- matrix(c(s1_cont,s2_cont), 12, 2)
Ga <- rowMeans(matrix_cont)

# set number of subjects to simulate
nsub <- 100

# define parameter values for simulation
alpha <- runif(nsub,0.6,5)
rho <- runif(nsub,0.1,0.9)
omega <- runif(nsub,0.1,0.9)

# simulate subjects
sim_data <- cc_sim(nsub, alpha, rho, omega, Ga)
c <- sim_data$c
Gb <- sim_data$Gb

# visualize simulated subjects
sim_sub_plot(c)

# recover subject-level parameters
data_list <- list(
  nsub = nsub,
  Ga = Ga,
  c = c)

params <- c("omega", "rho", "alpha")

start_time = Sys.time()
samples <- jags(data = data_list,
                inits=NULL,
                parameters.to.save = params,
                model.file ="subject_model.txt", 
                n.chains = 3,
                n.iter=5000, n.burnin=1000, n.thin=1) #, n.cluster=3)

end_time = Sys.time()
duration = end_time - start_time
print(paste("[INFO]: Duration of model estimation was", round(duration, 2)))

# save samples
save(samples, file = "model_output/sub_recov_samples.RData")

# extract recovered parameters
alpha_recov <- array(NA, c(nsub))
rho_recov <- array(NA, c(nsub))
omega_recov <- array(NA, c(nsub))

for (s in 1:nsub){
  alpha_recov[s] <- MPD(samples$BUGSoutput$sims.list$alpha[s,])
  rho_recov[s] <- MPD(samples$BUGSoutput$sims.list$rho[s,])
  omega_recov[s] <- MPD(samples$BUGSoutput$sims.list$omega[s,])
}

# collect in data frame
df <- data.frame(
  parameter = rep(c("alpha", "rho", "omega"), each = nsub),
  true = c(alpha, rho, omega),
  recov = c(alpha_recov, rho_recov, omega_recov))

# visualize recovered parameters vs. true parameters
sub_recov_plot(df)



