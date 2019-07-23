
#' Generates a probability distribution over possible data outcomes
#'
#' NOTE: This needs to be checked for whether it is taking account of strategy probabilities properly
#'
#' @param model A causal model as created by \code{make_model}
#' @param given A data frame with observations
#' @param subset data strategy
#' @export
#' @return A dataset
#' @examples
#'
#' library(dplyr)
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#'
#' model <- set_parameters(model, type = "flat")
#' possible_data <- make_possible_data(model, N= 2)
#' make_data_probabilities(model, pars = model$parameters, possible_data)
#'
#' given <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1)) %>%
#'   collapse_data(model)
#' possible_data <- make_possible_data(model, given = given, condition = "X==1 & Y==1", vars = "M", within = TRUE )
#' make_data_probabilities(model, pars = model$parameters, possible_data)
#'
make_data_probabilities <- function(model, pars,  possible_data) {

	A_w <- (get_likelihood_helpers(model)$A_w)[possible_data$event, ]
	w   <-  draw_event_prob(model, parameters = pars, using = "parameters")
	w_full = A_w %*% w

	strategy    <- possible_data$strategy
	strat_set   <- unique(strategy)

	# Probability of outcomes within each strategy set
	x <- apply(possible_data[,-(1:2)], 2, function(d)
		sapply(strat_set, function(j) dmultinom(d[strategy==j],
																						prob = w_full[strategy==j])
		))
	if(!is.null(nrow(x))) x <- apply(x, 2, prod)

	# Normalization
	x/sum(x)
}
