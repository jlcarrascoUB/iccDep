

# Wald Test ------------------------------------------------------------

## Diferència -----

# Wald test amb la diferència de ICCs
# Standard error --> Delta method

W_test<-function(r,S){
  if ((sum(is.na(r))==0) & (sum(is.na(S))==0)){
    dif<-sum(c(1,-1)*diag(r))
    dif_var<-S[1,1]+S[2,2]-2*S[1,2]
    cval<-(dif^2)/dif_var
    p_Wald<-1-pchisq(cval,1)
  } else p_Wald<-NA
  
  return(p_Wald)
}


## Fisher's Z transformation ------------------------------------------------------------


# Wald test using Z.transformation
WZ_test<-function(r,S,k1,k2){
  if ((sum(is.na(r))==0) & (sum(is.na(S))==0)){
    # Z transformation delta
    Z1<-Z_trans(r[1,1],k1)
    Z2<-Z_trans(r[2,2],k2)
    var_Z1<-(dZ(r[1,1],k1)^2)*S[1,1]
    var_Z2<-(dZ(r[2,2],k2)^2)*S[2,2]
    cov_Z1Z2<-dZ(r[1,1],k1)*dZ(r[2,2],k2)*S[1,2]
    dif_Z<-Z1-Z2
    dif_var_Z<-var_Z1+var_Z2-2*cov_Z1Z2
    cval_Z<-(dif_Z^2)/dif_var_Z
    p_Wald_Z<-1-pchisq(cval_Z,1)
  } else p_Wald_Z<-NA
  return(p_Wald_Z)
}


## K transformation ------------------------------------------------------------


# Wald test using K.transformation
WK_test<-function(r,S,k1,k2){
  if ((sum(is.na(r))==0) & (sum(is.na(S))==0)){
    # Z transformation delta
    K1<-K_trans(r[1,1],k1)
    K2<-K_trans(r[2,2],k2)
    var_K1<-(dK(r[1,1],k1)^2)*S[1,1]
    var_K2<-(dK(r[2,2],k2)^2)*S[2,2]
    cov_K1K2<-dK(r[1,1],k1)*dK(r[2,2],k2)*S[1,2]
    dif_K<-K1-K2
    dif_var_K<-var_K1+var_K2-2*cov_K1K2
    cval_K<-(dif_K^2)/dif_var_K
    p_Wald_K<-1-pchisq(cval_K,1)
  } else p_Wald_K<-NA
  return(p_Wald_K)
}

## Fisher's Z transformation Donner ------------------------------------------------------------



# Wald test. Z transformation Donner

WZ_Donner<-function(r,S,k1,k2,ns,rg){
  if ((sum(is.na(r))==0) & (is.na(rg)==FALSE)){
    # Z transformation
    Z1<-Z_trans(r[1,1],k1)
    Z2<-Z_trans(r[2,2],k2)
    var_Z1<-var_Z_D(ns,k1)
    var_Z2<-var_Z_D(ns,k2)
    cov_Z1Z2<-cov_Z_D(ns,k1,k2,rg,rg,r[1,2])
    theta<-0.5*log((1+(k1-1)*rg)/(1+(k2-1)*rg))
    dif_Z<-Z1-Z2-theta
    dif_var_Z <- var_Z1+var_Z2-2*cov_Z1Z2
    cval_Z<-(dif_Z^2)/dif_var_Z
    p_Wald_ZD<-1-pchisq(cval_Z,1)
  } else p_Wald_ZD<-NA
  return(p_Wald_ZD)
  
}

## K transformation Donner ------------------------------------------------------------



WK_Donner<-function(r,S,k1,k2,ns,rg){
  if ((sum(is.na(r))==0) & (is.na(rg)==FALSE)){
    # K transformation Donner
    K1<-K_trans(r[1,1],k1)
    K2<-K_trans(r[2,2],k2)
    theta_M <- (1/ns)*((7-5*k1)/sqrt(18*k1*(k1-1))-(7-5*k2)/sqrt(18*k2*(k2-1)))
    theta_K<-K_trans(rg,k1)-K_trans(rg,k2) - theta_M
    cov_K1K2 <- cov_K_D(ns,k1,k2,rg,rg,r[1,2])
    dif_var_K <- 2*((1/ns)-cov_K1K2)
    dif_K<-K1-K2-theta_K
    cval_K<-(dif_K^2)/dif_var_K
    p_Wald_KD<-1-pchisq(cval_K,1)
  } else p_Wald_KD<-NA
  return(p_Wald_KD)
}

