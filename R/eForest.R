#' Autoencoder forest
#' 
#' Implements an autoencoder forest, a function that maps inputs to a 
#' hyperrectangle defined by the intersection of leaves in a random forest.
#' 
#' @param rf Random forest model object of class \code{ranger}.
#' @param x Input data. May be the training data used for \code{rf}, or else
#'   an independent test set. 
#' @param parallel Compute in parallel? Must register backend beforehand.
#'
#' @details
#' This function implements the autoencoder forest algorithm proposed by Feng & 
#' Zhou (2017). 
#' 
#' @return
#' An encoded dataset of maximum compatible rules for each sample.
#' 
#' @references 
#' Feng, J. and Zhou, Z. (2017). AutoEncoder by Forest. \emph{arXiv} preprint, 
#' 1709.09018. 
#' 
#' @export
#' @import data.table
#' @import ranger
#' @import foreach
#'

# Autoencoder forest function
eForest <- function(
    rf,
    x,
    parallel = TRUE) {
  
  # Prelimz
  num_trees <- rf$num.trees
  x <- prep_x(x)
  n <- nrow(x)
  d <- ncol(x)
  factor_cols <- sapply(x, is.factor)
  
  # Compute bounds
  bnd_fn <- function(tree) {
    num_nodes <- length(rf$forest$split.varIDs[[tree]])
    lb <- matrix(-Inf, nrow = num_nodes, ncol = d)
    ub <- matrix(Inf, nrow = num_nodes, ncol = d)
    for (j in seq_len(d)) {
      if (j %in% which(!factor_cols)) {
        lb[, j] <- min(x[[j]])
        ub[, j] <- max(x[[j]])
      } else {
        lb[, j] <- 0.5
        ub[, j] <- length(unique(x[[j]])) + 0.5
      }
    }
    for (i in 1:num_nodes) {
      left_child <- rf$forest$child.nodeIDs[[tree]][[1]][i] + 1L
      right_child <- rf$forest$child.nodeIDs[[tree]][[2]][i] + 1L
      splitvarID <- rf$forest$split.varIDs[[tree]][i] + 1L
      splitval <- rf$forest$split.value[[tree]][i]
      if (left_child > 1L) {
        ub[left_child, ] <- ub[right_child, ] <- ub[i, ]
        lb[left_child, ] <- lb[right_child, ] <- lb[i, ]
        if (left_child != right_child) {
          # If no pruned node, split changes bounds
          ub[left_child, splitvarID] <- lb[right_child, splitvarID] <- splitval
        }
      }
    }
    leaves <- which(rf$forest$child.nodeIDs[[tree]][[1]] == 0L) 
    colnames(lb) <- colnames(ub) <- colnames(x)
    merge(melt(data.table(tree = tree, leaf = leaves, lb[leaves, ]), 
               id.vars = c('tree', 'leaf'), value.name = 'min'), 
          melt(data.table(tree = tree, leaf = leaves, ub[leaves, ]), 
               id.vars = c('tree', 'leaf'), value.name = 'max'), 
          by = c('tree', 'leaf', 'variable'), sort = FALSE)
  }
  if (isTRUE(parallel)) {
    bnds <- foreach(tree = seq_len(num_trees), .combine = rbind) %dopar% bnd_fn(tree)
  } else {
    bnds <- foreach(tree = seq_len(num_trees), .combine = rbind) %do% bnd_fn(tree)
  }
  
  # Only keep the leaves we care about
  pred <- data.table(
    'tree' = rep(seq_len(num_trees), each = n), 
    'leaf' = as.vector(stats::predict(rf, x, type = 'terminalNodes')$predictions + 1L),
    'obs' = rep(seq_len(n), times = num_trees)
  )
  bnds <- merge(bnds, unique(pred[, .(tree, leaf)]), by = c('tree', 'leaf'), 
                sort = FALSE)
  
  # Compute encoding for each sample
  df <- merge(bnds, pred, by = c('tree', 'leaf'), allow.cartesian = TRUE, 
              sort = FALSE)
  lo <- df[, max(min), by = .(obs, variable)]
  hi <- df[, min(max), by = .(obs, variable)]
  setnames(lo, 'V1', 'min')
  setnames(hi, 'V1', 'max')
  out <- merge(lo, hi, by = c('obs', 'variable'))
  return(out)
  
}


