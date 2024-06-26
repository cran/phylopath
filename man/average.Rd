% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phylopath.R
\name{average}
\alias{average}
\title{Extract and average the best supported models from a phylogenetic path
analysis.}
\usage{
average(phylopath, cut_off = 2, avg_method = "conditional", ...)
}
\arguments{
\item{phylopath}{An object of class \code{phylopath}.}

\item{cut_off}{The CICc cut-off used to select the best models. Use
\code{Inf} to average over all models. Use the \code{\link[=best]{best()}} function to
only use the top model, or \code{\link[=choice]{choice()}} to select any single model.}

\item{avg_method}{Either \code{"full"} or \code{"conditional"}. The methods
differ in how they deal with averaging a path coefficient where the path is
absent in some of the models. The full method sets the coefficient (and the
variance) for the missing paths to zero, meaning paths that are missing in
some models will shrink towards zero. The conditional method only averages
over models where the path appears, making it more sensitive to small
effects. Following von Hardenberg & Gonzalez-Voyer 2013, conditional
averaging is set as the default.}

\item{...}{Arguments to pass to \link[phylolm:phylolm]{phylolm::phylolm} and \link[phylolm:phyloglm]{phylolm::phyloglm}. Provide \code{boot = K}
parameter to enable bootstrapping, where \code{K} is the number of bootstrap replicates. If you
specified other options in the original \link{phylo_path} call you don't need to specify them again.}
}
\value{
An object of class \code{fitted_DAG}.
}
\description{
Extract and average the best supported models from a phylogenetic path
analysis.
}
\examples{
  candidates <- define_model_set(
    A = NL ~ RS,
    B = RS ~ NL + BM,
    .common = c(LS ~ BM, DD ~ NL, NL ~ BM)
  )
  p <- phylo_path(candidates, rhino, rhino_tree)
  summary(p)

  # Models A and B have similar support, so we may decide to take
  # their average.

  avg_model <- average(p)
  # Print the average model to see coefficients, se and ci:
  avg_model

  \dontrun{
  # Plot to show the weighted graph:
  plot(avg_model)

  # One can see that an averaged model is not necessarily a DAG itself.
  # This model actually has a path in two directions.

  # Note that coefficients that only occur in one of the models become much
  # smaller when we use full averaging:

  coef_plot(avg_model)
  coef_plot(average(p, method = 'full'))
  }

}
