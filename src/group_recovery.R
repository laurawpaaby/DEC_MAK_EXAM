#### SIMULATE GROUPS AND RECOVER GROUP MEANS ####

# load libraries
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

# Create an empty list for storing simulated data
ngroups <- 30
nsub <- 100

# define true parameter means
mu_alpha <- runif(ngroups,0,3)
mu_rho <- runif(ngroups,0.1,0.9)
mu_omega <- runif(ngroups,0.1,0.9)

# empty arrays for recovered means
mu_alpha_recov <- array(NA, c(ngroups))
mu_rho_recov <- array(NA, c(ngroups))
mu_omega_recov <- array(NA, c(ngroups))

for (i in 1:ngroups){
  
  # simulate subject contributions
  c <- group_cc_sim(nsub, mu_alpha[i], mu_rho[i], mu_omega[i], Ga)
  
  print(paste("[INFO]: Recovering  ", i, "/", ngroups, "  group means"))
  
  data_list <- list(Ga = Ga,
                    nsub = nsub,
                    c = c)
  
  params <- c("mu_alpha", "mu_rho", "mu_omega")
  
  start_time = Sys.time()
  samples <- jags.parallel(data = data_list,
                           inits=NULL,
                           parameters.to.save = params,
                           model.file ="src/group_model.txt", # remember to change ntrials in txt
                           n.chains = 3,
                           n.iter=5000, n.burnin=1000, n.thin=1,
                           jags.seed=626)
  
  end_time = Sys.time()
  duration = end_time - start_time
  print(paste("[INFO]: Duration of model estimation was", round(duration, 2)))
  
  # extract recovered parameter
  mu_alpha_recov[i] <- MPD(samples$BUGSoutput$sims.list$mu_alpha)
  mu_rho_recov[i] <- MPD(samples$BUGSoutput$sims.list$mu_rho)
  mu_omega_recov[i] <- MPD(samples$BUGSoutput$sims.list$mu_omega)
}

# plot recov versus true
recov_df <- data.frame(parameter=rep(c("alpha", "rho", "omega"), each=ngroups),
                       true=c(mu_alpha, mu_rho, mu_omega),
                       recov=c(mu_alpha_recov, mu_rho_recov, mu_omega_recov))

save(recov_df, file="jags_output/mean_recov_df.RData")

mean_recov_plot(recov_df, filename = "mean_recov.png")

print("[INFO]: Finished.")
