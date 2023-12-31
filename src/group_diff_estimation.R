#### ESTIMATION OF PARAMETER DIFFERENCES BETWEEN GROUPS ####

# load libraries
library(R2jags)
library(parallel)
library(extraDistr)
library(tidyverse)

source("src/simulation_functions.R")
source("src/plot_functions.R")

set.seed(626)

# load data
data <- read.csv("pgg_bayes2023-12-06.csv")

# presumably, the three entries with empty MAL status are non-maltreated if has to add up to 230
data$MAL_Status[data$MAL_Status == ""] <- "non-malreated"

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
c_1 <- data_split$maltreated %>%
  select(starts_with("test")) %>%
  as.matrix()

c_2 <- data_split$`non-malreated` %>%
  select(starts_with("test")) %>%
  as.matrix()

# number of subjects in each group
nsub_1 <- nrow(c_1)
nsub_2 <- nrow(c_2)

# Ga
Ga <- compute_Ga(ntrials=12)

# define data for JAGS
data_list <- list(Ga = Ga,
                  nsub_1 = nsub_1,
                  nsub_2 = nsub_2,
                  c_1 = c_1,
                  c_2 = c_2)

# set parameters for JAGS
params <- c("diff_alpha", "diff_rho", "diff_omega")

# run JAGS
print("[INFO]: Parameter estimation...")

start_time = Sys.time()
samples <- jags.parallel(data = data_list,
                         inits=NULL,
                         parameters.to.save = params,
                         model.file ="src/group_diff_model.txt", 
                         n.chains = 3,
                         n.iter=5000, n.burnin=1000, n.thin=1,
                         jags.seed = 626)

end_time = Sys.time()
duration = end_time - start_time
print(paste("[INFO]: Duration of model estimation was", round(duration, 2)))

# save samples
save(samples, file = "jags_output/group_diff_estimation.RData")
print("[INFO]: Saved jags_output/group_diff_estimation.RData.")

# plot posterior density of differences
df <- data.frame(parameter=rep(c("alpha", "rho", "omega"), each=12000),
                  samples = c(as.vector(samples$BUGSoutput$sims.list$diff_alpha), 
                              as.vector(samples$BUGSoutput$sims.list$diff_rho), 
                              as.vector(samples$BUGSoutput$sims.list$diff_omega)))

post_diff_plot(df)

print("[INFO]: Finished.")

