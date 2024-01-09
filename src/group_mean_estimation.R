#### ESTIMATION OF GROUP MEANS ####

# load libraries
library(R2jags)
library(parallel)
library(tidyverse)

source("src/simulation_functions.R")
source("src/plot_functions.R")

set.seed(626)

# load data
data <- read.csv("pgg_bayes2023-12-06.csv")

# subtract 1 from all contributions
data <- data %>%
  mutate_at(vars(starts_with("test")), list(~ . - 1))

# split data according to MAL_status and keep only test columns
data_split <- data %>%
  select(MAL_Status, starts_with("test")) %>%
  split(.$MAL_Status)

# delete MAL_Status column from each data frame
data_split <- lapply(data_split, function(x) x[, -1])

# remove NAs
data_split$`non-malreated` <- data_split$`non-malreated`[complete.cases(data_split$`non-malreated`), ]

# convert data frames to matrices
c_1 <- data_split$`non-malreated` %>%
  select(starts_with("test")) %>%
  as.matrix()

c_2 <- data_split$maltreated %>%
  select(starts_with("test")) %>%
  as.matrix()

# number of subjects in each group
nsub_1 <- nrow(c_1)
nsub_2 <- nrow(c_2)

# Ga
Ga <- compute_Ga(ntrials=12)



print(paste("[INFO]: Estimating means of non-maltreated group"))

data_list <- list(Ga = Ga,
                  nsub = nsub_1,
                  c = c_1)

params <- c("mu_alpha", "mu_rho", "mu_omega")

start_time = Sys.time()
samples1 <- jags.parallel(data = data_list,
                         inits=NULL,
                         parameters.to.save = params,
                         model.file ="src/group_model.txt", # remember to change ntrials in txt
                         n.chains = 3,
                         n.iter=5000, n.burnin=1000, n.thin=1,
                         jags.seed=626)

end_time = Sys.time()
duration = end_time - start_time
print(paste("[INFO]: Duration of model estimation was", round(duration, 2)))

save(samples1, file = "jags_output/nmt_estimation_samples.RData")

print(paste("[INFO]: Estimating means of maltreated group"))

data_list <- list(Ga = Ga,
                  nsub = nsub_2,
                  c = c_2)

params <- c("mu_alpha", "mu_rho", "mu_omega")

start_time = Sys.time()
samples2 <- jags.parallel(data = data_list,
                         inits=NULL,
                         parameters.to.save = params,
                         model.file ="src/group_model.txt", # remember to change ntrials in txt
                         n.chains = 3,
                         n.iter=5000, n.burnin=1000, n.thin=1,
                         jags.seed=626)

end_time = Sys.time()
duration = end_time - start_time
print(paste("[INFO]: Duration of model estimation was", round(duration, 2)))

save(samples2, file = "jags_output/mt_estimation_samples.RData")

# plot posterior density of means
df <- data.frame(parameter = rep(c("alpha", "rho", "omega"), each=24000),
                 samples = c(samples1$BUGSoutput$sims.list$mu_alpha,
                             samples2$BUGSoutput$sims.list$mu_alpha,
                             samples1$BUGSoutput$sims.list$mu_rho,
                             samples2$BUGSoutput$sims.list$mu_rho,
                             samples1$BUGSoutput$sims.list$mu_omega,
                             samples2$BUGSoutput$sims.list$mu_omega),
                 group = rep( rep(c("non-maltreated", "maltreated"), each=12000) , 3)
                 )

save(df, file="jags_output/group_mean_estimation_df.RData")

post_mean_plot(df)

print("[INFO]: Finished.")

