## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment = "#",fig.width = 5,
                      fig.height = 4,fig.align = "center",
                      eval = TRUE)

## -----------------------------------------------------------------------------
library(ashr)
library(mashr)
set.seed(1)
simdata = simple_sims(10000,5,1) # simulates data on 40k tests

# identify a subset of strong tests
m.1by1 = mash_1by1(mash_set_data(simdata$Bhat,simdata$Shat))
strong.subset = get_significant_results(m.1by1,0.05)

# identify a random subset of 5000 tests
random.subset = sample(1:nrow(simdata$Bhat),5000)


## -----------------------------------------------------------------------------
data.temp = mash_set_data(simdata$Bhat[random.subset,],simdata$Shat[random.subset,])
Vhat = estimate_null_correlation_simple(data.temp)
rm(data.temp)

## -----------------------------------------------------------------------------
data.random = mash_set_data(simdata$Bhat[random.subset,],simdata$Shat[random.subset,],V=Vhat)
data.strong = mash_set_data(simdata$Bhat[strong.subset,],simdata$Shat[strong.subset,], V=Vhat)

## -----------------------------------------------------------------------------
U.pca = cov_pca(data.strong,5)
U.ed = cov_ed(data.strong, U.pca)

## -----------------------------------------------------------------------------
U.c = cov_canonical(data.random)
m = mash(data.random, Ulist = c(U.ed,U.c), outputlevel = 1)

## -----------------------------------------------------------------------------
m2 = mash(data.strong, g=get_fitted_g(m), fixg=TRUE)
head(get_lfsr(m2))

