## This script illustrates the Metropolis-Hastings algorithm for
## approximating the standard normal distribution
## Author: Vladimir N. Minin
## last update: 07/17/2022

## Your task: Add code the following function.

#' Generate the next state the Metropolis-Hastings (M-H) chain targeting N(0,1)
#' distribution, using a uniform random walk proposal
#'
#' @param cur_value Current state of M-H Markov chain
#' @param tuning_par Tuning parameter of the uniform random walk, where a random
#'   increment ~ Unif[-tuning_par, tuning_par] is added to the current state of
#'   the Markov chain (delta in the notes)
#'
#' @return Numeric vector of length 2. The first element of the vector contains
#'   the next state of the M-H Markov chain. The second element contains 0 if
#'   the proposed values was rejected and 1 otherwise.
#'
#' @examples
#' unif_rw_next(0.4, 5.0)
unif_rw_next = function(cur_value, tuning_par) {
	
	return_value = c(cur_value, 0)
	
	U1 <- runif(1, -tuning_par, tuning_par)
	U2 <- runif(1, 0, 1)
	
	chk <- U2 <= min(exp( ( cur_value^2 - (cur_value + U1)^2 ) / 2 ), 1)
	
	return_value <- c(
		ifelse(
			chk,
			cur_value + U1,
			cur_value
		),
		as.numeric(chk)
	)
	
	return(return_value)
}

mcmc_size = 10000
start_value = 3.0

mcmc_out = matrix(0, nrow=(mcmc_size), ncol=2)
colnames(mcmc_out) = c("state", "acc_status")

cur_value = c(start_value,1)
mcmc_out[1,] = cur_value

## Your task: add code to the below for loop to
## fill in the mcmc.out matrix defined above with
## the first column recording the state of the random
## walk and the second column recording the acceptance status
## of each Metropolis-Hastings move. Don't forget to play
## with the tuning parameter \delta.

set.seed(1234)
for (i in 2:mcmc_size){
	# using row mcmc_out[i-1,] and unif_rw_next
	mcmc_out[i,] = unif_rw_next(mcmc_out[i-1, "state"], 5.0)
}

## acceptance probability
mean(mcmc_out[,"acc_status"])

## mean of the target distribution
mean(mcmc_out[,"state"])

## trace plot
plot(1:mcmc_size, mcmc_out[,"state"], type="l", xlab="Iteration", ylab="MCMC State")

## histogram
hist(mcmc_out[,"state"], xlab="MCMC State", main="Target Distribution Histogram")


# Testing different delta
delta_test <- seq(0.1, 10, 0.1)

res <- vector(mode = "list", length = length(delta_test))

for (i in 1:length(delta_test)) {
	this_delta <- delta_test[[i]]
	
	this_out <- matrix(0, nrow=(mcmc_size), ncol=2)
	colnames(this_out) = c("state", "acc_status")
	this_out[1,] = c(3, 1)
		
	for (j in 2:mcmc_size){
		this_out[j,] = unif_rw_next(mcmc_out[j-1, "state"], this_delta)
	}
	
	res[[i]] <- this_out
	
}

ap <- purrr::map_dbl(res, ~mean(.x[,"acc_status"]))
tm <- purrr::map_dbl(res, ~mean(.x[,"state"]))

par(mfrow = c(1, 2), oma = c(0, 0, 0, 0))
plot(delta_test, ap, xlab = "delta", ylab = "acceptance probability", type = "b")
abline(h = 0.3, lty = 2)
abline(h = 0.4, lty = 2)
plot(delta_test, tm, xlab = "delta", ylab = "target distribution mean", type = "b")
abline(h = 0, lty = 2)
