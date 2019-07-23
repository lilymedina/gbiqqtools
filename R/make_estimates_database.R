



#' Generates a database of results using gbiqq over possible data
#'
#' This function runs many models and can take a long time depending on the size of possible data.
#'
#' @param model A causal model as created by \code{make_model}
#' @param possible_data A data frame with possible data
#' @param queries Queries
#' @export
#' @return A dataframe
#' @examples
#' #' library(dplyr)
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#'
#' given = data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#'
#' possible_data <- make_possible_data(model, given)
#'
#' estimates_database <- make_estimates_database(
#'       model,
#'       given = given,
#'       possible_data,
#'       queries = "Y[X=1]>Y[X=0]")
#'

make_estimates_database <- function(model,
																		given,
																		possible_data = NULL,
																		queries = "Y[X=1]>Y[X=0]") {

	if(!exists("fit")) fit  <- fitted_model()
	if(is.null(possible_data)) possible_data <- make_possible_data(model, given, ...)

	## HACK: 3 here only because of particular shape of possible data
	out <- sapply(3:ncol(possible_data), function(j) {

		data_events <- possible_data[, c(1, j)]

		data <- simulate_data(model, data_events = data_events)

		updated <- gbiqq::gbiqq(model = model, data = data, stan_model = fit)

		gbiqq::query_model(updated,
											 queries = queries,
											 using = "posteriors",
											 subset = TRUE)

	})
	## NEED BETTER OUTPUT FORMAT? NAD INCLUDE SUMMARY OF DATA IN TEH OUTPUT DATABASE TO IMPROVE LEGIBILITY
	t(out)
}
## NOTE NEED TO ADD SUBSET ARGUMENT
##  gbiqq::gbiqq necessary for me for reasons I don't understand!
