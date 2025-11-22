df_prepare <- function(df, ry = "AUC", rind = "SUBJ", rmet = "MET"){
  df <- df |> dplyr::rename(y = all_of(ry),
                            i = all_of(rind),
                            k = all_of(rmet))
  return(df)
}


opt_minH0 <- function(mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1, rho = 0.5, eta = 0.5){
  
  df <- data.frame(y,i,k)
  
  df_list <- df |> dplyr::select(y,i,k) |> dplyr::group_split(i, .keep = F)
  
  df_list <- lapply(df_list, function(dd) dd |> dplyr::arrange(k))
  
  do.call("sum",lapply(df_list, function(dd) 
    x_minH0(x = dd[[1]], k = dd[[2]], mu1 = mu1, mu2 = mu2,
            sigma1 = sigma1, sigma2 = sigma2, rho = rho, eta = eta)))
}




x_minH0 <- function(x, k, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1, rho = 0.5, eta = 0.5){
  
  # Fixed params
  p1 <- table(k)[[1]]
  p2 <- table(k)[[2]]
  
  p <- p1 + p2
  
  if ((p1>0) & (p2>0)){
    # var-cor matrix
    vc_mat <- matrix(0,nrow = p, ncol = p)
    
    ## Sigma_p1
    vc_mat[1:p1,1:p1] <- (sigma1^2)*((1-rho)*diag(p1) + rho)
    
    ## Sigma_p2
    vc_mat[(p1+1):p,(p1+1):p] <- (sigma2^2)*((1-rho)*diag(p2) + rho)
    
    ## Sigma_p1p2
    vc_mat[1:p1,(p1+1):p] <- eta*sigma1*sigma2*matrix(1,p1,p2)
    
    ## Sigma_p2p1
    vc_mat[(p1+1):p,1:p1] <- eta*sigma1*sigma2*matrix(1,p2,p1)
    
    # Mean vector
    mu_vec <- c(rep(mu1,p1),rep(mu2,p2))
    
    return(-sum(mvtnorm::dmvnorm(x, mean = mu_vec, sigma = vc_mat, log = TRUE)))
  }else{
    return(0)
  }
}

opt_minH1 <- function(mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1, rho1 = 0.5, rho2 = 0.5,  eta = 0.5){
  
  df <- data.frame(y,i,k)
  
  df_list <- df |> dplyr::select(y,i,k) |> dplyr::group_split(i, .keep = F)
  
  df_list <- lapply(df_list, function(dd) dd |> dplyr::arrange(k))
  
  do.call("sum",lapply(df_list, function(dd) 
    x_minH1(x = dd[[1]], k = dd[[2]], mu1 = mu1, mu2 = mu2,
            sigma1 = sigma1, sigma2 = sigma2, 
            rho1 = rho1, rho2 = rho2, eta = eta)))
}


x_minH1 <- function(x, k, mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1, rho1 = 0.5, rho2 = 0.5, eta = 0.5){
  
  # Fixed params
  p1 <- table(k)[[1]]
  p2 <- table(k)[[2]]
  
  p <- p1 + p2
  if ((p1>0) & (p2>0)){
  # var-cor matrix
  vc_mat <- matrix(0,nrow = p, ncol = p)
  
  ## Sigma_p1
  vc_mat[1:p1,1:p1] <- (sigma1^2)*((1-rho1)*diag(p1) + rho1)
  
  ## Sigma_p2
  vc_mat[(p1+1):p,(p1+1):p] <- (sigma2^2)*((1-rho2)*diag(p2) + rho2)
  
  ## Sigma_p1p2
  vc_mat[1:p1,(p1+1):p] <- eta*sigma1*sigma2*matrix(1,p1,p2)
  
  ## Sigma_p2p1
  vc_mat[(p1+1):p,1:p1] <- eta*sigma1*sigma2*matrix(1,p2,p1)
  
  # Mean vector
  mu_vec <- c(rep(mu1,p1),rep(mu2,p2))
  
  return(-sum(mvtnorm::dmvnorm(x, mean = mu_vec, sigma = vc_mat, log = TRUE)))
  }else{
    return(0)
  }
}


ICC_LR_test <- function(ry = "y",rind = "i",rmet = "k",data,optimizer = "nlminb"){
  
  DNAME <- deparse(substitute(data))
  
  aux <- df_prepare(data,ry,rind,rmet)
  
  aux_H0 <- bbmle::mle2(minuslogl = opt_minH0,optimizer = optimizer, 
                        data = aux, skip.hessian = T)
  
  aux_H1 <- bbmle::mle2(minuslogl = opt_minH1,optimizer = optimizer, 
                        data = aux, skip.hessian = T)
  
  null.val <- rep(bbmle::coef(aux_H0)[5],2)
  H1.val <- bbmle::coef(aux_H1)[5:6]
  names(null.val) <- c("rho1","rho2")
  
  param <- 1
  names(param) <- "df"
  
  LR <- 2*(bbmle::logLik(aux_H1)[[1]] - bbmle::logLik(aux_H0)[[1]])
  names(LR) <- "X-squared"
  
  pval <- pchisq(LR,param, lower.tail = F)
  
  structure(list(null.value = null.val, alternative = "two-sided", 
                 method = "Likelihood Ratio test", estimate = H1.val,
                 data.name = DNAME, statistic = LR, p.value = pval),
            class = "htest")
}
