

#' Diagnosis a single data strategy
#'
diagnose_strategies_single <- function(reference_model,
															analysis_model,
															given,
															queries,
															estimates_database = NULL,
															possible_data = NULL,
															possible_data_args = list(N = list(1),
																												within = FALSE,
																												condition = list(TRUE),
																												vars = list(NULL)),
															sims = 1000) {
	# Houskeeping

	if(is.null(possible_data)){
		possible_data_args$model <- reference_model
		possible_data_args$given <- given
		possible_data            <- do.call(make_possible_data, possible_data_args)

	} else{
		possible_data_args       <- attr(possible_data, "possible_data_args")

  }

	if(is.null(estimates_database)) {
		estimates_database <- make_estimates_database(analysis_model,
																									given = given,
																									possible_data = possible_data,
																									queries = queries)
		}

	# The magic: Draw multiple parameters and get mean squared error over possible data observations as well as posterior variance.

	out <- replicate(sims,
		{
		# draw parameters: use posteriors if available
		using <- ifelse(is.null(reference_model$posterior_distribution), "priors", "posteriors")
		pars   <- draw_parameters(reference_model, using = using)

		# implied data probabilities
		probs <- make_data_probabilities(reference_model, pars = pars, possible_data = possible_data)

		# implied estimand
		estimand <- gbiqq::query_model(reference_model,
																		 parameters = pars,
																		 queries = queries,
																		 using = "parameters",
																		 subset = TRUE)$mean
		# evaluation
		estimates     <- unlist(estimates_database[,"mean"])
		squared_error <- (estimates - estimand)^2
		post_var      <- (unlist(estimates_database[,"sd"]))^2

		# Return
		c(estimand = estimand,
			estimate = estimates%*%probs,
			MSE      = squared_error%*%probs,
			post_var = post_var%*%probs)
		})

	if(length(possible_data_args$N)==1){

		vars_arg   <- ifelse(is.null(possible_data_args$vars[[1]]), "ALL", possible_data_args$vars[[1]])
		condition  <- ifelse(is.logical(possible_data_args$condition[[1]]), "-", possible_data_args$condition[[1]])
		diagnosis           <- data.frame(query   = queries,
																			condition = condition,
																			variables = vars_arg,
																			N         = possible_data_args$N[[1]],
																			steps     =  1,

																			t(as.matrix(c(apply(out, 1, mean), sims = sims), nrow = 1)),
																			stringsAsFactors = FALSE)

	} else{
    n_steps     <- length(possible_data_args$N)
		conditions_ <- paste0(possible_data_args$condition, collapse = " ")
		vars_       <- paste0("c(", paste0(possible_data_args$vars, collapse = ", "), ")")
		N_          <- paste0("c(", paste0(possible_data_args$N,    collapse = ", "), ")")
		diagnosis   <- data.frame(query     = queries,
														 condition = conditions_,
														 variables = vars_,
														 N         = N_,
														 steps     = length(possible_data_args$N),
														 t(as.matrix(c(apply(out, 1, mean), sims = sims), nrow = 1)),
														 stringsAsFactors = FALSE)
	}

	return_list <- list()
	return_list$possible_data       <- possible_data
	return_list$estimates_database  <- estimates_database
  return_list$replicates          <- out
	return_list$diagnoses_df        <- diagnosis


	class(return_list) <- c("strategy_diagnosis_single")
	return(return_list)

}




#' Diagnosis a data strategy
#'
#' @param reference_model A causal model as created by \code{make_model}
#' @param analysis_model A causal model as created by \code{make_model}
#' @param given A data frame with exisitng data
#' @param queries queries
#' @export
#' @return A dataframe
#' @examples
#'
#' library(dplyr)
#' reference_model <- analysis_model <-
#'    make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#'
#' df    <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#' given <- collapse_data(df, reference_model)
#'
#' reference_model <- gbiqq(reference_model, df)
#'
#' possible_data <- make_possible_data(model = reference_model,
#'                                     given = given,
#'                                     within = TRUE,
#'                                     condition = "X==1 & Y==1",
#'                                     N = 1,
#'                                     vars = "M")
#'
#' queries <- "Y[X=1]>Y[X=0]"
#'
#' estimates_database <- make_estimates_database(analysis_model,
#'                                  given,
#'                                  possible_data,
#'                                  queries = queries)
#'
#' diagnose_strategies(reference_model = reference_model,
#'                   analysis_model = analysis_model,
#'                   given = given,
#'                   queries = queries,
#'                   estimates_database = estimates_database,
#'                   possible_data = possible_data,
#'                   sims = 10)
diagnose_strategies <- function(reference_model,
																analysis_model,
																given,
																queries,
																estimates_database = NULL,
																possible_data = NULL,
																N = 1,
																within = FALSE,
																vars   = NULL,
																conditions = list(TRUE),
																sims = 1000){

	if(!is.null(possible_data)){
		return_list <- diagnose_strategies_single(reference_model, analysis_model, given, queries, estimates_database, possible_data, sims = sims)
	} else{
		if(length(conditions) == 1){
			return_list <- diagnose_strategies_single(reference_model, analysis_model, given, queries, estimates_database, possible_data, possible_data_args = list(N = N, within = within,vars = vars, condition = conditions), sims = sims)
		} else{
			## option 1
			return_list <- lapply(conditions, function(k) {
				diagnose_strategies_single(reference_model, analysis_model, given, queries, possible_data_args = list(N = N, within = within,vars = vars, condition = k), sims = sims)
			})
			names(return_list) <- conditions
			diagnoses_df <- lapply(return_list, function(x) x$diagnoses_df)
			return_list$diagnoses_df <- do.call(rbind, diagnoses_df)
			rownames(return_list$diagnoses_df) <- NULL
			class(return_list) <- c("strategy_diagnoses")
		}



	}



	return(return_list)
}

###



#' @export
print.strategy_diagnoses <- function(x, ...) {
	print(summary(x))
	invisible(x)
}


#' @export
summary.strategy_diagnoses <- function(object, ...) {
	structure(object, class = c("summary.strategy_diagnoses", "data.frame"))

}

#' @export
print.summary.strategy_diagnoses <- function(x, ...){
	print(x$diagnoses_df)
}



#' @export
print.strategy_diagnosis_single <- function(x, ...) {
	print(summary(x))
	invisible(x)
}


#' @export
summary.strategy_diagnosis_single <- function(object, ...) {
	structure(object, class = c("summary.strategy_diagnosis_single", "data.frame"))

}

#' @export
print.summary.strategy_diagnosis_single <- function(x, ...){
	print(x$diagnoses_df)

}


