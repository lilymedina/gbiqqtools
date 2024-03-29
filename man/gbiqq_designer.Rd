% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbiqq_designer.R
\name{gbiqq_designer}
\alias{gbiqq_designer}
\title{Integrate gbiqq with DeclareDesign}
\usage{
gbiqq_designer(model = make_model("X -> Y"), restrictions = NULL,
  priors = "uniform", parameters = NULL, inquiry = list(ATE =
  "Y[X=1] - Y[X=0]"), data_strat = NULL, answer_strat = NULL)
}
\arguments{
\item{model}{A model object generated by \code{make_model()}.}

\item{restrictions}{A list of character vectors specifying nodal types to be kept or removed from the model. Use \code{get_nodal_types} to see syntax.}

\item{priors}{A numeric vector. Dirichlet hyperparameters.}

\item{parameters}{A numeric vector. True parameters.}

\item{inquiry}{A named list of strings. Names are estimand labels and values are strings containing estimand definitions. See `Examples` for correct format.}

\item{data_strat}{should be of the form "list(n_obs = n_obs, vars = list("X", "Y"), probs = list(...), n = NULL, subsets = list(...))}

\item{answer_strat}{priors used at the analysis stage, if different}
}
\description{
Define a model, estimand, and answer strategy.
}
\examples{
require("DeclareDesign")
my_design <- gbiqq_designer(
  model = make_model("X -> Y"),
  parameters = c(.5, .5, .1, .7, .1, .1),  # Truth
  data_strat = list(n_obs = 5, vars = list(NULL), probs = list(NULL),
                    ns = NULL,
     subsets = list(NULL)),
  inquiry = list(ATE = "Y[X=1] - Y[X=0]"))

\dontrun{draw_data(my_design)}
\dontrun{draw_estimands(my_design)}
\dontrun{draw_estimates(my_design)}
\dontrun{diagnose_design(my_design, sims = 2)}

my_design <- gbiqq_designer(
model = make_model("X -> M -> Y"),
inquiry = list(ATE = "Y[X=1] - Y[X=0]"),
data_strat = list(n_obs = 5,
vars = list(c("X", "Y"), "M"),
probs = list(1, .5),
n = NULL,
subsets = list(NULL, "X==1 & Y==0")),
answer_strat  = NULL
)

df <- draw_data(my_design)
draw_estimands(my_design)
\dontrun{
get_estimates(my_design, df)
}
}
