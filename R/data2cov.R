#' @title Perform PCA on data and return list of candidate covariance
#' matrices
#'
#' @param data a mash data object
#'
#' @param npc the number of PCs to use
#'
#' @param subset indices of the subset of data to use (set to NULL for
#' all data)
#'
#' @return Returns a list of covariance matrices: the npc rank-one
#'   covariance matrices based on the first npc PCs, and the rank npc
#'   covariance matrix. If flashier did not identify any factors,
#'   \code{NULL} is returned.
#'
#' @examples
#' data = mash_set_data(Bhat = cbind(c(1,2),c(3,4)), Shat = cbind(c(1,1),c(1,1)))
#' cov_pca(data,2)
#'
#' @importFrom assertthat assert_that
#'
#' @export
#'
cov_pca = function(data,npc,subset = NULL) {
  assert_that(npc > 1)
  assert_that(npc <= n_conditions(data))
  if (is.null(subset))
    subset = 1:n_effects(data)
  res.svd = svd(data$Bhat[subset,],nv = npc,nu = npc)

  # FIXME: we need to think of for the EE case what to use for svd
  # input: Bhat or Bhat/Shat
  f     = res.svd$v
  Ulist = cov_from_factors(t(f),"PCA")
  d     = diag(res.svd$d[1:npc])
  Ulist = c(Ulist,list("tPCA" = f %*% d^2 %*% t(f)/length(subset)))
  for (i in 1:length(Ulist)) {
    rownames(Ulist[[i]]) <- colnames(data$Bhat)
    colnames(Ulist[[i]]) <- colnames(data$Bhat)
  }
  return(Ulist)
}

#' @title Perform Empirical Bayes Matrix Factorization using flashier, and
#'   return a list of candidate covariance matrices
#'
#' @param data A \dQuote{mash} data object.
#'
#' @param factors If \code{factors = "default"}, the factors and
#'   loadings are both unconstrained. If \code{factors = "nonneg"}, the
#'   factors are constrained to be non-negative, and the loadings are
#'   unconstrained.
#'
#' @param subset Data samples (rows) used to estimate the
#'   covariances. Sset to \code{NULL} to use all the data.
#'
#' @param remove_singleton If \code{remove_singleton = TRUE}, factors
#'   corresponding to singleton matrices will be removed from the output.
#'
#' @param tag How to name the covariance matrices.
#'
#' @param output_model The fitted flash model will be saved to this file
#'   (using \code{\link{saveRDS}}).
#'
#' @param greedy_args List containing additional parameters passed to
#'    \code{flashier::flash_greedy}.
#'
#' @param backfit_args List containing additional parameters passed
#'   to \code{flashier::flash_backfit}.
#'
#' @return A list of covariance matrices.
#'
#' @examples
#' # See https://stephenslab.github.io/mashr/articles/flash_mash.html
#' # for an example
#'
#' @importFrom utils hasName
#' @importFrom stats fitted
#'
#' @export
#'
cov_flash = function (data, factors = c("default","nonneg"),
                      subset = NULL, remove_singleton = FALSE,
                      tag = NULL, output_model = NULL,
                      greedy_args = list(),
                      backfit_args = list()) {
  if (!requireNamespace("flashier",quietly = TRUE))
    stop("cov_flash requires package flashier")

  # Only keep factors with at least two values greater than 1/sqrt(n)
  find_nonunique_effects <- function (fl) {
    thresh          <- 1/sqrt(nrow(fl$F_pm))
    vals_above_avg  <- colSums(abs(fl$F_pm) > thresh)
    nonuniq_effects <- which(vals_above_avg > 1)
    message(paste("Removing",
                  length(vals_above_avg) - length(nonuniq_effects),
                  "singleton effect vectors"))
    return(fl$F_pm[,nonuniq_effects,drop = FALSE])
  }

  # Function to initialize non-negative factors.
  nonneg_init <- function (fl) {
    ret     <- flashier::flash_greedy_init_softImpute(fl)
    pos_sum <- sum(ret[[2]][ret[[2]] > 0])
    neg_sum <- -sum(ret[[2]][ret[[2]] < 0])
    if (neg_sum > pos_sum)
      return(list(-ret[[1]],-ret[[2]]))
    else
      return(ret)
  }

  # Extract a subset of the data.
  if (is.null(subset))
    subset <- 1:n_effects(data)

  # Initialize the flash object.
  f       <- flashier::flash_init(as.matrix(data$Bhat[subset,]),var_type = 2)
  factors <- match.arg(factors)

  # Set defaults.
  greedy_args$flash <- f
  greedy_args$Kmax  <- 100
  if (!hasName(greedy_args,"init_fn")) {
    if (factors == "default")
      greedy_args$init_fn <- flashier::flash_greedy_init_softImpute
    else if (factors == "nonneg")
      greedy_args$init_fn <- nonneg_init
  }
  if (factors == "default")
    greedy_args$ebnm_fn <- ebnm::ebnm_point_normal
  else if (factors == "nonneg")
    greedy_args$ebnm_fn <- c(ebnm::ebnm_point_normal,
                             ebnm::ebnm_point_exponential)

  # Backfit the flash model.
  backfit_args$flash <- do.call(flashier::flash_greedy,greedy_args)
  f <- do.call(flashier::flash_backfit,backfit_args)
  if (f$n_factors == 0) {
    warning("Flashier did not find any factors; returning NULL")
    return(NULL)
  }
  if (remove_singleton)
    flash_factors <- find_nonunique_effects(f)
  else
   flash_factors <- flashier::ldf(f)$F
  if (!is.null(output_model))
   saveRDS(list(model = f,factors = flash_factors),output_model)
  if (missing(tag))
    tag <- factors
  U.flash <- list()
  U.flash[[paste0("tFLASH_",tag)]] <-
    t(fitted(f)) %*% fitted(f) / nrow(fitted(f))
  if (ncol(flash_factors) > 0)
    U.flash <- c(U.flash,
                 c(cov_from_factors(t(flash_factors),
                                    paste0("FLASH_",tag))))
  for (i in 1:length(U.flash)) {
    rownames(U.flash[[i]]) <- colnames(data$Bhat[subset,])
    colnames(U.flash[[i]]) <- colnames(data$Bhat[subset,])
  }
  return(U.flash)
}

#' @title Perform "extreme deconvolution" (Bovy et al) on a subset of
#' the data
#'
#' @param data a mash data object
#'
#' @param Ulist_init a named list of covariance matrices to use to
#' initialize ED; default is to use matrices from PCs
#'
#' @param subset a subset of data to be used when ED is run (set to
#' NULL for all the data)
#'
#' @param algorithm algorithm to run ED
#'
#' @param ... other arguments to be passed to ED algorith, see
#' \code{\link{extreme_deconvolution}} for algorithm 'bovy', or
#' \code{\link{teem_wrapper}} for algorithm 'teem'
#'
#' @details Runs the extreme deconvolution algorithm from Bovy et al
#' (Annals of Applied Statistics) to estimate data-driven covariance
#' matrices. It can be initialized with, for example running \code{cov_pca} with,
#' say, 5 PCs.
#' @examples
#' \dontrun{
#' data = mash_set_data(Bhat = cbind(c(1,2),c(3,4)), Shat = cbind(c(1,1),c(1,1)))
#' U_pca = cov_pca(data,2)
#' U_x = apply(data$Bhat, 2, function(x) x - mean(x))
#' U_xx = t(U_x) %*% U_x / nrow(U_x)
#' cov_ed(data,c(U_pca, list(xx = U_xx)))
#' }
#'
#' @export
#'
cov_ed = function(data, Ulist_init, subset = NULL,
                  algorithm=c('bovy', 'teem'), ...) {
  algorithm = match.arg(algorithm)
  if (algorithm=='bovy') {
    Ulist_ed = bovy_wrapper(data, Ulist_init, subset, ...)$Ulist
  } else {
    Ulist_ed = teem_wrapper(data, Ulist_init, subset, ...)$U
  }
  names(Ulist_ed) = make_names("ED", if(is.null(names(Ulist_init))) 1:length(Ulist_ed) else names(Ulist_init))
  Ulist_ed
}

# For a vector x, return the rank one matrix xx'.
r1cov=function(x){x %*% t(x)}

#' produce list of rank-1 covariance matrices corresponding to rows of f
#'
#' @param f a matrix of factors (each row is a factor)
#'
#' @param name a string indicating the name to use
#'
#' @return a list of rank one matrices whose kth element is f[k,]
#' f[k,]' and named name_k
#'
#' @keywords internal
#'
cov_from_factors = function(f, name){
  Ulist = list()
  for(i in 1:nrow(f)){
    Ulist = c(Ulist,list(r1cov(f[i,])))
  }
  names(Ulist) = paste0(name,"_",(1:nrow(f)))
  return(Ulist)
}
