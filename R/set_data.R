#' @title Create a data object for mash analysis.
#'
#' @param Bhat An N by R matrix of observed estimates.
#'
#' @param Shat An N by R matrix of corresponding standard errors. Shat
#'   can be a scalar if all standard errors are equal. This is most
#'   useful if Bhat is a matrix of Z scores, so elements of Shat are all
#'   1. Default is 1.
#'
#' @param alpha Numeric value of alpha parameter in the model. alpha =
#'   0 for Exchangeable Effects (EE), alpha = 1 for Exchangeable
#'   Z-scores (EZ). Default is 0. Please refer to equation (3.2) of
#'   M. Stephens 2016, Biostatistics for a discussion on alpha.
#'
#' @param df An N by R matrix of corresponding degrees of freedom of
#'   the t-statistic Bhat/Shat. Can be a scalar if all degrees of
#'   freedom are equal. Default is inf (for large samples).
#'
#' @param pval An N by R matrix of p-values of t-statistic
#'   Bhat/Shat. Shat and df should not be specified when pval is
#'   provided.
#'
#' @param V an R by R matrix / [R x R x N] array of effect specific
#'   correlation matrix of error correlations; must be positive
#'   definite. [So Bhat_j distributed as N(B_j,diag(Shat_j) V[,,j]
#'   diag(Shat_j)) where _j denotes the jth row of a matrix]. Defaults
#'   to identity.
#'
#' @param zero_check_tol a small positive number as threshold for Shat
#'   to be considered zero if any Shat is smaller or equal to this
#'   number.
#'
#' @param zero_Bhat_Shat_reset Replace zeros in Shat matrix to given
#'   value if the corresponding Bhat are also zeros.
#'
#' @param zero_Shat_reset Replace zeros in Shat matrix to given value.
#'
#' @return A data object for passing into mash functions.
#'
#' @examples
#' simdata = simple_sims(50,5,1)
#' data = mash_set_data(simdata$Bhat, simdata$Shat)
#'
#' @importFrom stats pt
#'
#' @export
#'
mash_set_data = function (Bhat, Shat = NULL, alpha = 0, df = Inf,
                          pval = NULL, V = diag(ncol(Bhat)),
                          zero_check_tol = .Machine$double.eps,
                          zero_Bhat_Shat_reset = 0, zero_Shat_reset = 0) {
  if (is.null(Shat) && is.null(pval)) {
    Shat = 1
  }
  if (!is.null(pval) && !is.null(Shat)) {
    stop("Either Shat or pval can be specified but not both.")
  }
  if (!is.null(pval) && !is.infinite(df)) {
    stop("Either df or pval can be specified but not both.")
  }
  if (!is.null(pval)) {
    ## Shat and df have to be NULL
    if (length(which(pval == 0))>0) {
      stop("p-values cannot contain zero values (implying infinite z-scores)")
    }
    Shat = Bhat / p2z(pval, Bhat)
  }
  if(length(Shat)==1) {
    Shat = matrix(Shat,nrow=nrow(Bhat),ncol=ncol(Bhat))
  }
  if(!identical(dim(Bhat),dim(Shat))){
    stop("dimensions of Bhat and Shat must match")
  }
  if (length(which(is.nan(Bhat) | is.infinite(Bhat)))>0) {
    stop("Bhat cannot contain NaN/Inf values")
  }
  if (length(which(is.nan(Shat) | is.infinite(Shat)))>0) {
    stop("Shat cannot contain NaN/Inf values")
  }
  if (length(which(Shat <= zero_check_tol))>0) {
    msg = paste0("If it is expected please set Shat to a positive number to avoid numerical issues; or lower the threshold to call zeros using zero_check_tol (currently set to ", zero_check_tol, ").")
    if (length(which(Shat <= zero_check_tol & Bhat <= zero_check_tol)) > 0) {
      if (zero_Bhat_Shat_reset>0) {
        Shat[which(Shat <= zero_check_tol & Bhat <= zero_check_tol)] = zero_Bhat_Shat_reset
      } else {
        stop(paste("Both Bhat and Shat are zero (or near zero) for some input data. Please check your input.", msg, "To replace zero elements you can use `zero_Bhat_Shat_reset`."))
      }
    } else {
      if (zero_Shat_reset>0) {
        Shat[which(Shat <= zero_check_tol)] = zero_Shat_reset
      } else {
        stop(paste("Shat contains zero (or near zero) values.", msg, "To replace zero elements you can use `zero_Shat_reset`."))
      }
    }
  }
  commonV = TRUE
  if(length(dim(V)) == 3){
    commonV = FALSE
  }

  if(commonV){
    check_positive_definite(V)
  } else {
    if(dim(V)[3] != nrow(Bhat)) {
      stop('The number of correlation matrices does not match the number of effects')
    }
    for(i in 1:dim(V)[3]) {
      check_positive_definite(V[,,i])
    }
  }

  if(dim(V)[1] != ncol(Bhat)) {
    stop('dimension of correlation matrix does not match the number of conditions')
  }

  if(any(!is.infinite(df))) {
    if (length(df)==1) {
      df = matrix(df,nrow=nrow(Bhat),ncol=ncol(Bhat))
    }
    ## Shat = Bhat/Z where Z is the Z score corresponding to a p value from a t test done on (Bhat,Shat_orig,df)
    Shat = Bhat / p2z(2 * pt(-abs(Bhat/Shat), df), Bhat)
  }
  na_idx = which(is.na(Bhat))
  sbhat_not_null = !all(Shat == 1)
  if (!isTRUE(all.equal(na_idx, which(is.na(Shat)), check.attributes=FALSE)) && sbhat_not_null) {
    stop("Missing data pattern is inconsistent between Bhat and Shat")
  }
  # transform data according to alpha
  if (alpha != 0 && sbhat_not_null) {
    ## alpha models dependence of effect size on standard error
    ## alpha > 0 implies larger effects has large standard error
    ## a special case when alpha = 1 is the EZ model
    Shat_alpha = Shat^alpha
    Bhat = Bhat / Shat_alpha
    Shat = Shat^(1-alpha)
  } else {
    Shat_alpha = matrix(1, nrow(Shat), ncol(Shat))
  }
  Bhat[na_idx] = 0
  Shat[na_idx] = 1E6
  Shat_alpha[na_idx] = 1
  data = list(Bhat=Bhat, Shat=Shat, Shat_alpha=Shat_alpha, V=V, commonV = commonV, alpha=alpha)
  class(data) = 'mash'
  return(data)
}

#' @title Update the data object for mash analysis.
#'
#' @description This function can update two parts of the mash
#' data. The first one is setting the reference group, so the mash
#' data can be used for commonbaseline analysis. The other one is
#' updating the null correlation matrix.
#'
#' @param mashdata mash data object ontaining the Bhat matrix,
#' standard errors, V; created using \code{mash_set_data}
#'
#' @param ref the reference group. It could be a number between 1,...,
#' R, R is number of conditions, or the name of reference group. If
#' there is no reference group, it can be the string 'mean'.
#'
#' @param V an R by R matrix / [R x R x N] array of correlation matrix
#' of error correlations
#'
#' @return a updated mash data object
#
#' @examples
#' simdata = simple_sims(50,5,1)
#' data = mash_set_data(simdata$Bhat, simdata$Shat)
#' mash_update_data(data, 'mean')
#'
#' @export
#'
mash_update_data = function(mashdata, ref= NULL, V = NULL){
  if (!inherits(mashdata,"mash"))
    stop('data is not a "mash" object')

  R = n_conditions(mashdata)

  if(!is.null(V)){
    if(length(dim(V)) == 3){
      for(i in 1:dim(V)[3]){
        check_positive_definite(V[,,i])
        if(R != nrow(V[,,i])){
          stop('The dimension of correlation matrix does not match the data.')
        }
      }
    }else{
      check_positive_definite(V)
    }
    mashdata$V = V
    mashdata$commonV = length(dim(V)) != 3
  }

  if(!is.null(mashdata$L)){
    mashdata = mash_set_data_contrast(mashdata, mashdata$L)
  }

  if(!is.null(ref)){
    if(!is.null(mashdata$L)){
      stop('The data is ready for the contrast analysis.')
    }
    R = n_conditions(mashdata)
    name = colnames(mashdata$Bhat)
    if(is.null(name)){
      name = 1:R
    }
    L = contrast_matrix(R, ref, name)
    mashdata = mash_set_data_contrast(mashdata, L)
  }

  return(mashdata)
}

#' @title Create contrast matrix
#'
#' @param R the number of column for the contrast matrix
#'
#' @param ref the reference group. It could be a number between 1,...,
#' R, R is number of conditions, or the name of reference group. If
#' there is no reference group, it can be the string 'mean'.
#'
#' @param name a length R vector contains the name for conditions
#'
#' @examples
#' contrast_matrix(5, 'mean')
#'
#' @export
#'
contrast_matrix = function(R, ref, name=1:R){
  if(ref == 'mean'){
    L = matrix(-1/R, R, R)
    diag(L) = (R-1)/R
    L = L[1:(R-1),]
    row.names(L) = paste0(name[1:(R-1)],'-','mean')
  }else if(ref %in% 1:R){
    L = diag(R)
    L[,ref] = -1
    L = L[-ref,]
    row.names(L) = paste0(name[-ref],'-', name[ref])
  }else if (ref %in% name){
    ind = which(name %in% ref)
    if (length(ind) != 1){
      stop('There are more than one groups in the data have the same name as the reference group.')
    }
    L = diag(R)
    L[,ind] = -1
    L = L[-ind,]
    row.names(L) = paste0(name[-ind],'-', ref)
  }else{
    stop('The ref group is not in the given conditions.')
  }
  colnames(L) = name
  attr(L, "reference") = ref
  return(L)
}

#' @title Create a data object for mash contrast analysis
#'
#' @description This is an internal (non-exported) function. This help
#'   page provides additional documentation mainly intended for
#'   developers and expert users.
#'
#' @param mashdata a mash data object containing the Bhat matrix,
#' standard errors, V; created using \code{mash_set_data}
#'
#' @param L the contrast matrix
#'
#' @return a data object after the contrast transfermation
#'
#' @keywords internal
#'
mash_set_data_contrast = function(mashdata, L){
  # check data
  if (!inherits(mashdata,"mash"))
    stop('data is not a "mash" object')

  # check contrast
  R = ncol(mashdata$Bhat)
  if(ncol(L) != R){
    stop('The contrast is not correct')
  }

  # transfer Bhat
  Bhat = mashdata$Bhat %*% t(L)
  mashdata$Shat_orig = mashdata$Shat
  mashdata$L = L

  # get standard error for delta
  if(mashdata$commonV && is_common_cov_Shat(mashdata)){
    V = get_cov(mashdata,1) # all covariances are same
    Shat = matrix(rep(sqrt(diag(V)), each=nrow(Bhat)), nrow = nrow(Bhat))
    V = cov2cor(V)
  } else{
    V = array(0,c(ncol(Bhat), ncol(Bhat), nrow(Bhat)))
    Shat = matrix(0, nrow(Bhat), ncol(Bhat))
    for(j in 1:nrow(Bhat)){
      tmp = get_cov(mashdata,j)
      Shat[j,] = sqrt(diag(tmp))
      V[,,j] = cov2cor(tmp)
    }
  }

  data = list(Bhat = Bhat, Shat=Shat,
              Shat_orig = mashdata$Shat_orig,
              Shat_alpha = matrix(1, nrow(Shat), ncol(Shat)),
              V = mashdata$V, commonV = mashdata$commonV, alpha = 0, L = L,
              LSVSLt = V)
  class(data) = 'mash'
  return(data)
}

# Return the covariance matrix for jth data point from mash data object.
get_cov = function(data,j){
  if(data$commonV){
    if(is.null(data$L)){
      data$Shat[j,] * t(data$V * data$Shat[j,]) # quicker than diag(Shat[j,]) %*% V %*% diag(Shat[j,])
    } else{
      Sigma = data$Shat_orig[j,] * t(data$V * data$Shat_orig[j,])
      data$L %*% (Sigma %*% t(data$L))
    }
  }else{
    if(is.null(data$L)){
      data$Shat[j,] * t(data$V[,,j] * data$Shat[j,]) # quicker than diag(Shat[j,]) %*% V %*% diag(Shat[j,])
    } else{
      Sigma = data$Shat_orig[j,] * t(data$V[,,j] * data$Shat_orig[j,])
      data$L %*% (Sigma %*% t(data$L))
    }
  }
}

# @title Check that all covariances are equal (Shat).
# @description checks if all rows of Shat are the same - if so
#   covariances are equal
# @param data A mash data object.
is_common_cov_Shat = function(data){
  if(is.null(data$L)){
    sum(1-duplicated(data$Shat, MARGIN=1)) == 1
  } else{
    sum(1-duplicated(data$Shat_orig, MARGIN=1)) == 1
  }
}

# @title Check that all rows of Shat_alpha are the same.
# @description checks if all rows of Shat_alpha are the same
# @param data A mash data object.
is_common_cov_Shat_alpha = function(data){
  sum(1-duplicated(data$Shat_alpha, MARGIN=1)) == 1
}

n_conditions = function(data){ncol(data$Bhat)}

n_effects = function(data){nrow(data$Bhat)}

#' @importFrom stats qnorm
p2z = function(pval, Bhat) {
  z = abs(qnorm(pval / 2))
  z[which(Bhat < 0)] = -1 * z[which(Bhat < 0)]
  return(z)
}
