#' Get data probabilities
#'
#' Takes in a matrix of possible (single case) observations and returns the probability of each.
#'
#' @param model A  model
#' @param data Data in long format
#' @param parameters A numeric vector. Values of parameters may be specified. By default, parameters is drawn from priors.
#' @export
#' @examples
#' model <- make_model("X->Y")
#' data <- simulate_data(model, n = 4)
#' get_data_probs(model, data)
get_data_probs <- function(model, data, parameters = NULL){

	if(is.null(parameters)) {
		if(is.null(model$parameters)) stop("parameters not provided")
		parameters <- model$parameters }

	events  <- get_data_events(data = data, model = model)$data_events
	A_w     <- get_likelihood_helpers(model)$A_w
	probs   <- A_w %*% draw_event_prob(model, parameters = parameters)
	np      <- rownames(probs)
	unlist(sapply(encode_data(model, data), function(j) probs[np==j]))
}


