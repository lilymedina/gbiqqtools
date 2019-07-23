#' helper for ways to allocate N units into n data types: tidies partition::composition output
#'
#' @param N Number of observations to be distributed
#' @param n Number of possible values observations could take
#' @examples
#' allocations(4,2)
allocations <- function(N, n) {
	x <- partitions::compositions(N,n)
	x <- data.frame(as.matrix(x))
	colnames(x) <- 1:ncol(x)
	x
}



#' helper to fill buckets dataframe
#' @param buckets dataframe with columns event, count and capacity vars plus strategy allocation var
#' @param vars vars to be observed
#' @export
#' @examples
#' model <- make_model("X->M->Y")
#' buckets = data.frame(event = "X0Y0", count = 3, capacity = 3, strategy = 2)
#' # Find different data that might result from looking at "M" in 2 out of 3 X0Y0 data types
#' fill_bucket(model, buckets, vars = "M")
fill_bucket <- function(model, buckets, vars, row = 1, column = 4){

	if(!(all(vars %in% model$variables))) stop("Vars not in model$variables")

	# Figure out set of possible finer units
	df <- simulate_data(model,
											data_events = data.frame(
												event = buckets$event[row], count = 1))
	possible_findings <- perm(rep(1, length(vars)))
	df <- df %>% slice(rep(1:n(), each = nrow(possible_findings)))
	df[vars] <- possible_findings
	df <- collapse_data(df, model)
	# Assign n across new possible finer events
	new_events <- cbind(event = df[df$count ==1, "event"],
											gbiqq:::allocations(buckets[row, column], sum(df$count)))

	# tidy up
	remaining  <- data.frame(event = buckets[row, 1], matrix(buckets$count[row] - buckets[row, column], ncol = ncol(new_events)-1, nrow = 1))
	names(remaining) <- names(new_events)
	rbind(new_events,remaining)
}


#' helper for getting all data on variables with N observed
#'
#' @examples
#' model <- make_model("X->M->Y")
#' gbiqq:::all_possible(model, N=2, vars = c("X", "M"))
#' gbiqq:::all_possible(model, N=2, vars = c("X", "Y"), condition = "Y==0")
all_possible <- function(model, N, vars = NULL, condition = TRUE){

	if(is.null(vars)) vars <- model$variables
	possible               <- get_max_possible_data(model)
	if(!all(is.na(vars))) possible[, !names(possible) %in% vars] <- NA
	possible <- possible[with(possible, eval(parse(text = condition))),]

	d.frame   <- summarize_data(model, possible)
	df        <- summarize_data(model, possible)[d.frame$count >0 ,1:2]

	possible_data <- gbiqq:::allocations(N, length(df$event))
	out <- matrix(0, nrow(d.frame), ncol(possible_data))
	out[d.frame$count >0,] <- as.matrix(possible_data)
	cbind(d.frame[,1:2], out)
}
#' Produces the possible permutations of a set of variables
#'
#' @param max A vector of integers indicating the maximum value of an integer value starting at 0. Defaults to 1. The number of permutation is defined by \code{max}'s length
#' @export
#' @return A matrix of permutations
#' @importFrom rlang exprs
#' @examples
#'
#' \dontrun{
#' perm(3)
#' }
# perm <- function(v) {
# 	sapply(1:length(v), function(x) {
# 		rep(rep(1:v[x], each = prod(v[x:length(v)])/v[x]), length.out = prod(v))
# 	}) - 1
# }
perm <- function(max = rep(1, 2)){
	grid <- sapply(max, function(m) exprs(0:!!m))
	perm <- do.call(expand.grid, grid)
	colnames(perm) <- NULL
	perm
}

