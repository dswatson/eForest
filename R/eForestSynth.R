#' Synthesize data from autoencoder forest
#' 
#' Takes uniform draws from hyperrectangles computed by an autoencoder forest.
#' 
#' @param rf Random forest model object of class \code{ranger}.
#' @param x Input data. May be the training data used for \code{rf}, or else
#'   an independent test set. 
#' @param z Autoencoded data as computed by \code{eForest}.
#' @param nSynth Number of synthetic samples to draw.
#'
#' @details
#' This function creates synthetic samples from an autoencoder forest. 
#' 
#' @return
#' A dataset of synthetic samples.
#' 
#' @references 
#' Feng, J. and Zhou, Z. (2017). AutoEncoder by Forest. \emph{arXiv} preprint, 
#' 1709.09018. 
#' 
#' @examples
#' # Encode model
#' library(ranger)
#' rf <- ranger(Species ~ ., data = iris)
#' z <- eForest(rf, iris)
#' 
#' # Synthesize
#' synth <- eForestSynth(rf, iris, z, 100)
#' 
#' @export
#' @import data.table
#' @import ranger
#'

eForestSynth <- function(rf, x, z, nSynth) {
  
  # Prelimz
  num_trees <- rf$num.trees
  x <- suppressWarnings(prep_x(x))
  n <- nrow(x)
  d <- ncol(x)
  factor_cols <- sapply(x, is.factor)
  factor_names <- colnames(x)[factor_cols]
  if (any(factor_cols)) {
    lvls <- rbindlist(lapply(factor_names, function(j) {
      data.table(
        variable = j, value = rf$forest$covariate.levels[[j]]
      )[, number := .I]
    }))
  }
  
  # Synthesize
  keep <- data.table(obs = sample(n, nSynth, replace = TRUE))
  synth <- merge(keep, z, by = 'obs', allow.cartesian = TRUE, sort = FALSE)
  synth[, idx := rep(seq_len(nSynth), each = d)]
  synth[, fctr := rep(factor_cols, nSynth)]
  synth[, new := runif(.N, min, max)]
  synth[fctr == TRUE, new := round(new)]
  synth <- dcast(synth, idx ~ variable, value.var = 'new')[, idx := NULL]
  
  # Fix factor columns
  if (any(factor_cols)) {
    for (j in factor_names) {
      tmp <- lvls[variable == j, .(value, number)]
      setnames(tmp, 'number', j)
      synth <- merge(synth, tmp, by = j, sort = FALSE)[, c(j) := NULL]
      setnames(synth, 'value', j)
    }
  }
  setcolorder(synth, colnames(x))
  return(synth)
  
}