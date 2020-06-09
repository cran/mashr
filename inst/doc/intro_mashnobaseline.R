## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment = "#",fig.width = 5,
                      fig.height = 4,fig.align = "center",
                      eval = TRUE)

## -----------------------------------------------------------------------------
library(MASS)
library(kableExtra)

## -----------------------------------------------------------------------------
generate_data = function(n, p, V, Utrue, err_sd=0.01, pi=NULL){
  if (is.null(pi)) {
    pi = rep(1, length(Utrue)) # default to uniform distribution
  }
  assertthat::are_equal(length(pi), length(Utrue))

  for (j in 1:length(Utrue)) {
    assertthat::are_equal(dim(Utrue[j]), c(p, p))
  }

  pi <- pi / sum(pi) # normalize pi to sum to one
  which_U <- sample(1:length(pi), n, replace=TRUE, prob=pi)

  Beta = matrix(0, nrow=n, ncol=p)
  for(i in 1:n){
    Beta[i,] = mvrnorm(1, rep(0, p), Utrue[[which_U[i]]])
  }
  Shat = matrix(err_sd, nrow=n, ncol=p, byrow = TRUE)
  E = mvrnorm(n, rep(0, p), Shat[1,]^2 * V)
  Bhat = Beta + E
  return(list(B = Beta, Bhat=Bhat, Shat = Shat, whichU = which_U))
}

## -----------------------------------------------------------------------------
set.seed(1)
n = 2000
R = 5
V = diag(R)
U0 = matrix(0, R, R)
U1 = matrix(1, R, R)
U2 = U0; U2[1:2,1:2] = 4
U3 = U0; U3[5,5] = 4
simdata = generate_data(n, R, V, list(U0=U0, U1=U1, U2=U2, U3 = U3), err_sd = 1)

## -----------------------------------------------------------------------------
library(mashr)
data = mash_set_data(simdata$Bhat, simdata$Shat)

data.L = mash_update_data(data, ref = 'mean')

## -----------------------------------------------------------------------------
U.c = cov_canonical(data.L)

## -----------------------------------------------------------------------------
m.1by1 = mash_1by1(data.L)
strong = get_significant_results(m.1by1)
U.pca = cov_pca(data.L,2,subset=strong)
U.ed = cov_ed(data.L, U.pca, subset=strong)

## -----------------------------------------------------------------------------
m = mash(data.L, c(U.c,U.ed), algorithm.version = 'R')

## -----------------------------------------------------------------------------
print(get_loglik(m),digits=10)

## -----------------------------------------------------------------------------
length(get_significant_results(m))

## ---- echo=FALSE--------------------------------------------------------------
null.ind = which(simdata$whichU %in% c(1,2))

