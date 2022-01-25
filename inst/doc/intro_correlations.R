## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,comment = "#",fig.width = 5,
                      fig.height = 4,fig.align = "center",
                      eval = TRUE)

## -----------------------------------------------------------------------------
library(mashr)
set.seed(1)
simdata = simple_sims(500,5,1)
V = matrix(0.5,5,5)
diag(V) = 1
simdata$Bhat = simdata$B + mvtnorm::rmvnorm(2000, sigma = V)

## -----------------------------------------------------------------------------
data   = mash_set_data(simdata$Bhat, simdata$Shat)
V.simple = estimate_null_correlation_simple(data)
data.Vsimple = mash_update_data(data, V=V.simple)

## -----------------------------------------------------------------------------
U.c = cov_canonical(data.Vsimple) 
m.Vsimple = mash(data.Vsimple, U.c) # fits with correlations because data.V includes correlation information 
print(get_loglik(m.Vsimple),digits=10) # log-likelihood of the fit with correlations set to V

## -----------------------------------------------------------------------------
m.orig = mash(data, U.c) # fits without correlations because data object was set up without correlations
print(get_loglik(m.orig),digits=10)

## -----------------------------------------------------------------------------
loglik = c(get_loglik(m.orig), get_loglik(m.Vsimple))
significant = c(length(get_significant_results(m.orig)), length(get_significant_results(m.Vsimple)))
false_positive = c(sum(get_significant_results(m.orig) < 501), 
                   sum(get_significant_results(m.Vsimple) < 501))
tb = rbind(loglik, significant, false_positive)
colnames(tb) = c('without cor', 'V simple')
row.names(tb) = c('log likelihood', '# significance', '# False positive')
tb

## -----------------------------------------------------------------------------
V.em = mash_estimate_corr_em(data, U.c, details = TRUE)
m.Vem = V.em$mash.model
print(get_loglik(m.Vem),digits=10) # log-likelihood of the fit

## -----------------------------------------------------------------------------
loglik = c(get_loglik(m.orig), get_loglik(m.Vsimple), get_loglik(m.Vem))
significant = c(length(get_significant_results(m.orig)), length(get_significant_results(m.Vsimple)),
                length(get_significant_results(m.Vem)))
false_positive = c(sum(get_significant_results(m.orig) < 501), 
                   sum(get_significant_results(m.Vsimple) < 501),
                   sum(get_significant_results(m.Vem) < 501))
tb = rbind(loglik, significant, false_positive)
colnames(tb) = c('without cor', 'V simple', 'V EM')
row.names(tb) = c('log likelihood', '# significance', '# False positive')
tb

