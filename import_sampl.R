# Importance sampling #########################################################
## This script illustrates the Importance-Sampling algorithm
## Author: Vladimir N. Minin
## last update: 07/17/2022

# setup ####

set.seed(1234)

## define a threshold value and number of Monte Carlo samples
my_const = 4.5
sim_size = 10000

## true probability of interest
(true_prob = pnorm(my_const,lower.tail=FALSE))

## Your task: create a naive and an importance sampling
## estimate of the normal tail probability.
## To generate realizations from the standard normal
## distribution use `rnorm()` function.
## To generate realizations from the shifted exponential
## use `rexp()` to generate regular exponentials and
## then add my_const to them. Also, remember that you
## don't have to code the formula for the normal
## density, because it is available via `dnorm()'.
## If you finish early, get Monte Carlo errors for
## naive and important sampling schemes.

# naive Monte Carlo estimate ####
naive_sample <- rnorm(sim_size, 0, 1)
naive_hx <- naive_sample > my_const
(naive_mcmc_est <- mean(naive_hx))
(naive_mcmc_err <- 1.96 * sqrt(var(naive_hx) / sim_size))
(c(naive_mcmc_est - naive_mcmc_err, naive_mcmc_est + naive_mcmc_err))

# Importance sampling estimate ####
y_sample <- rexp(sim_size, 1) + my_const
phi_y <- dnorm(y_sample, 0, 1)
g_y <- exp(-(y_sample - my_const))
y <- phi_y / g_y
(import_mcmc_est <- mean(y))
(import_mcmc_err <- 1.96 * sqrt(var(y) / sim_size))
(c(import_mcmc_est - import_mcmc_err, import_mcmc_est + import_mcmc_err))
