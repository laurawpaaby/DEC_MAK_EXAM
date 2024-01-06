#### SIMULATE SUBJECTS AND RECOVER SUBJECT-LEVEL PARAMETERS ####

# load libraries
print("[INFO]: Loading packages.")
library(R2jags)
library(parallel)
library(tidyverse)

source("src/simulation_functions.R")
source("src/plot_functions.R")

set.seed(626)

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

# calculate Ga: trial-wise average contribution of other players
Ga <- compute_Ga(ntrials=12)

# set number of subjects to simulate
nsub <- 100

# define parameter values for simulation
alpha <- runif(nsub,0,3) #runif(nsub,0.6,6)
rho <- runif(nsub,0.1,0.9)
omega <- runif(nsub,0.1,0.9)

# simulate subjects
sim_data <- cc_sim(nsub, alpha, rho, omega, Ga)
c <- sim_data$c

# visualize simulated subjects
#sim_sub_plot_rtpois(c)
sim_sub_plot(c, filename="sim_sub_plot.png")

# recover subject-level parameters
data_list <- list(
  nsub = nsub,
  Ga = Ga,
  c = c)

params <- c("omega", "rho", "alpha")

print("[INFO]: Parameter estimation...")
start_time = Sys.time()
samples <- jags.parallel(data = data_list,
                        inits=NULL,
                        parameters.to.save = params,
                        model.file ="src/subject_model.txt", #remember to change ntrials in txt
                        n.chains = 3,
                        n.iter=5000, n.burnin=1000, n.thin=1,
                        jags.seed = 626)

end_time = Sys.time()
duration = end_time - start_time
print(paste("[INFO]: Duration of estimation was", round(duration, 2)))

# save samples
save(samples, file = "jags_output/sub_recov_samples.RData")

# extract recovered parameters
alpha_recov <- array(NA, c(nsub))
rho_recov <- array(NA, c(nsub))
omega_recov <- array(NA, c(nsub))

for (s in 1:nsub){
  alpha_recov[s] <- MPD(samples$BUGSoutput$sims.list$alpha[,s])
  rho_recov[s] <- MPD(samples$BUGSoutput$sims.list$rho[,s])
  omega_recov[s] <- MPD(samples$BUGSoutput$sims.list$omega[,s])
}

# collect in data frame
df <- data.frame(
  parameter = rep(c("alpha", "rho", "omega"), each = nsub),
  true = c(alpha, rho, omega),
  recov = c(alpha_recov, rho_recov, omega_recov))

# visualize recovered parameters vs. true parameters
sub_recov_plot(df, filename="sub_recov.png")

print("[INFO]: Finished.")



