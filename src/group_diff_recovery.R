#### SIMULATE GROUPS AND RECOVER GROUP DIFFERENCE PARAMETERS ####

# load libraries
library(R2jags)
library(extraDistr)
library(IMIFA)
library(cascsim)
library(tidyverse)
library(ggplot2)
library(patchwork)

source(simulation_functions.R)
source(plot_functions.R)

set.seed(626)

# defining a function for calculating the maximum of the posterior density (not exactly the same as the mode)
MPD <- function(x) {
  density(x)$x[which(density(x)$y==max(density(x)$y))]
}

# calculate Ga: trial-wise average contribution of other players
Ga <- compute_Ga()

# Create an empty list for storing simulated data
ndiff <- 10
nsub <- 100
sim_data <- vector("list", ndiff)

# Initialize the list with empty vectors and arrays
for (i in 1:ndiff) {
  sim_data[[i]] <- list(
    diff = list(alpha=c(), rho=c(), omega=c()),
    mid = list(alpha=c(), rho=c(), omega=c()),
    groups_mu = list(group1 = list(alpha=c(), rho=c(), omega=c()), group2 = list(alpha=c(), rho=c(), omega=c())),
    groups_c = list(group1 = array(NA, c(nsub, 12)), group2 = array(NA, c(nsub, 12)))
  )
}


for (i in 1:ndiff) {
  
  print(paste("[INFO]: Simulating  ", i, "/", ndiff, "  group pairs"))
  
  # simulated diffs  CHANGE DISTS
  sim_data[[i]]$diff$alpha <- rnorm(1, 0, 1)
  sim_data[[i]]$diff$rho <- rnorm(1, 0, 1)
  sim_data[[i]]$diff$omega <- rnorm(1, 0, 1)
  
  # simulated mids  CHANGE DISTS
  sim_data[[i]]$mid$alpha <- rnorm(1, 0, 1)
  sim_data[[i]]$mid$rho <- rnorm(1, 0, 1)
  sim_data[[i]]$mid$omega <- rnorm(1, 0, 1)
  
  # simulate each group's subject contributions
  X <- c(-0.5, 0.5)
  for (g in 1:2){
    
    # group means
    sim_data[[i]]$groups_mu[[g]]$alpha <- sim_data[[i]]$mid$alpha + sim_data[[i]]$diff$alpha * X[g]
    sim_data[[i]]$groups_mu[[g]]$rho <- sim_data[[i]]$mid$rho + sim_data[[i]]$diff$rho * X[g]
    sim_data[[i]]$groups_mu[[g]]$omega <- sim_data[[i]]$mid$omega + sim_data[[i]]$diff$omega * X[g]
    
    # simulate group contributions
    sim_data[[i]]$groups_c[[g]] <- group_cc_sim(nsub, mu_alpha, mu_rho, mu_omega, Ga)
    
  }
}

# make data frame to store group comparison info for plotting
group1_alphas <- sapply(sim_data, function(x) x$groups_mu[[1]]$alpha)
group1_rhos <- sapply(sim_data, function(x) x$groups_mu[[1]]$rho)
group1_omegas <- sapply(sim_data, function(x) x$groups_mu[[1]]$omega)
group2_alphas <- sapply(sim_data, function(x) x$groups_mu[[2]]$alpha)
group2_rhos <- sapply(sim_data, function(x) x$groups_mu[[2]]$rho)
group2_omegas <- sapply(sim_data, function(x) x$groups_mu[[2]]$omega)

group_df_for_plot <- data.frame(comparison=factor(rep(1:ndiff, 3)),
                                parameter=rep(c("alpha", "rho", "omega"), each=ndiff),
                                first=c(group1_alphas, group1_rhos, group1_omegas),
                                second=c(group2_alphas, group2_rhos, group2_omegas)
                                )

group_df_for_plot$sign <- ifelse(group_df_for_plot$first < group_df_for_plot$second, "positive", "negative")

# plot differences for each parameter
group_diffs_plot(group_df_for_plot)

# recover group diff parameter
diff_alpha_recov <- array(NA, c(ndiff))
diff_rho_recov <- array(NA, c(ndiff))
diff_omega_recov <- array(NA, c(ndiff))

for (i in 1:ndiff) {
  
  print(paste("[INFO]: Recovering  ", i, "/", ndiff, "  group difference parameters"))
  
  data_list <- list(Ga = Ga,
                    nsub_1 = nsub,
                    nsub_2 = nsub,
                    c_1 = sim_data[[i]]$groups_c[[1]],
                    c_2 = sim_data[[i]]$groups_c[[2]])
  
  params <- c("diff_alpha", "diff_rho", "diff_omega")
  
  start_time = Sys.time()
  samples <- jags(data = data_list,
                  inits=NULL,
                  parameters.to.save = params,
                  model.file ="group_diff_model_no_JZS.R", 
                  n.chains = 3,
                  n.iter=5000, n.burnin=1000, n.thin=1) #, n.cluster=3)
  
  end_time = Sys.time()
  duration = end_time - start_time
  print(paste("[INFO]: Duration of model estimation was", round(duration, 2)))
  
  # extract recovered parameter
  diff_alpha_recov[i] <- MPD(samples$BUGSoutput$sims.list$diff_alpha)
  diff_rho_recov[i] <- MPD(samples$BUGSoutput$sims.list$diff_rho)
  diff_omega_recov[i] <- MPD(samples$BUGSoutput$sims.list$diff_omega)

}
  
# plot recov versus true
true_alpha_diff <- sapply(sim_data, function(x) x$diff$alpha)
true_rho_diff <- sapply(sim_data, function(x) x$diff$rho)
true_omega_diff <- sapply(sim_data, function(x) x$diff$omega)

true_alpha_mid <- sapply(sim_data, function(x) x$mid$alpha)
true_rho_mid <- sapply(sim_data, function(x) x$mid$rho)
true_omega_mid <- sapply(sim_data, function(x) x$mid$omega)

recov_df <- data.frame(parameter=factor(rep(c("alpha", "rho", "omega"), each=ndiff)),
                       true_diff=c(true_alpha_diff, true_rho_diff, true_omega_diff),
                       recov_diff=c(diff_alpha_recov, diff_rho_recov, diff_omega_recov),
                       true_mid=c(true_alpha_mid, true_rho_mid, true_omega_mid))

save(recov_df, file="recov_df.RData")

recov_dfs <- split(df, f = df$parameter)

diff_recov_plot(recov_dfs)

print("[INFO]: Finished.")


