## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment = "#",fig.width = 5,
                      fig.height = 4,fig.align = "center",
                      eval = TRUE)

## -----------------------------------------------------------------------------
library(mashr)
set.seed(1)
simdata = sim_contrast2(nsamp = 12000, ncond = 8)

## -----------------------------------------------------------------------------
data = mash_set_data(simdata$Chat, simdata$Shat)

data.L = mash_update_data(data, ref = 8)

## -----------------------------------------------------------------------------
U.c = cov_canonical(data.L)
mashcontrast.model = mash(data.L, U.c, algorithm.version = 'R')

## -----------------------------------------------------------------------------
print(get_loglik(mashcontrast.model),digits=10)

## -----------------------------------------------------------------------------
length(get_significant_results(mashcontrast.model))

## -----------------------------------------------------------------------------
L = contrast_matrix(8, ref=8)
data.wrong = mash_set_data(Bhat = simdata$Chat %*% t(L), Shat = 1)
m = mash(data.wrong, U.c)

## -----------------------------------------------------------------------------
print(get_loglik(m),digits = 10)

