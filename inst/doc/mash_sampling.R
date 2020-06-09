## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment = "#",fig.width = 7,
                      fig.height = 7,fig.align = "center")

## -----------------------------------------------------------------------------
library(mashr)
set.seed(1)
simdata = simple_sims(500,5,1)

## -----------------------------------------------------------------------------
data = mash_set_data(simdata$Bhat, simdata$Shat)
U.c = cov_canonical(data)

## -----------------------------------------------------------------------------
m = mash(data, U.c, algorithm.version = 'R', posterior_samples = 100)

## -----------------------------------------------------------------------------
m$result = mash_compute_posterior_matrices(m, data, algorithm.version = 'R',
                                           posterior_samples = 100)

## -----------------------------------------------------------------------------
library(corrplot)
x = get_pairwise_sharing_from_samples(m, factor=0.5, lfsr_thresh = 1)
corrplot(x, method='color', cl.lim=c(0,1), type='upper', addCoef.col = "black", tl.col="black", tl.srt=45, title = 'Pairwise Sharing by Magnitude', mar = c(4,0,4,0))

## -----------------------------------------------------------------------------
x = get_pairwise_sharing_from_samples(m, factor=0.5, lfsr_thresh = 0.05)
corrplot(x, method='color', cl.lim=c(0,1), type='upper', addCoef.col = "black", tl.col="black", tl.srt=45, title = 'Pairwise Sharing by Magnitude', mar = c(4,0,4,0))

## -----------------------------------------------------------------------------
x = get_pairwise_sharing(m, factor=0.5)
corrplot(x, method='color', cl.lim=c(0,1), type='upper', addCoef.col = "black", tl.col="black", tl.srt=45, title = 'Pairwise Sharing by Magnitude', mar = c(4,0,4,0))

