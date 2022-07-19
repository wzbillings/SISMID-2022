## This script illustrates the ergodic theorem using the Ehrenfest model of
## diffusion
## Author: Vladimir N. Minin
## last update: 07/17/2022
## Updated by Zane Billings 2022-07-19

#' Generate the next state of the Ehrenfest diffusion model
#'
#' @param cur_state Current state of the Ehrenfest diffusion model
#' @param num_mols Total number of molecules in the Ehrenfest diffusion model
#'
#' @return a numeric of length 1 that is either (cur_state+1) or (cur_state-1)
#'
#' @examples
#' next_state(32, 1000)
next_state = function(cur_state, num_mols){
	X_n <- sample(
		x = c(cur_state - 1, cur_state + 1),
		size = 1,
		prob = c(cur_state / num_mols, 1 - cur_state / num_mols)
	)
	
	# Another way to do it
	# X_n <- ifelse(
	# 	runif(1) < (cur_state / num_mols),
	# 	cur_state - 1,
	# 	cur_state + 1
	# )

	## this function randomly draws a new state of the Ehrenfest model
	## One of your tasks is to finish writing this function
	return(X_n)
}
















## set the number of molecules and the number of iterations
my_num_mol = 100
sim_size = 100000

set.seed(1234)
## initialize the chain by drawing the initial state uniformly at random from
# all possible states. R function `sample()' will be handy.
my_draws = numeric(sim_size)
my_draws[1] = sample(x = 1:my_num_mol, size = 1L)

## run the Markov chain
for (i in 2:sim_size){
	## use next.state function to evolve the Markov chain one step at a time
	my_draws[i] = next_state(my_draws[i - 1], my_num_mol)
}

## plot the chain

plot(1:sim_size, my_draws, type="l", ylab="Ehrenfest State", xlab="Time Step")

## get the "time averages"

mean(my_draws)
var(my_draws)

## get the "space averages," theoretical binomial
## mean and variance
my_num_mol/2
my_num_mol/4
