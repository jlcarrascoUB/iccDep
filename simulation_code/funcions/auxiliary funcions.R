library(Deriv)

sa<-function(tau)  exp(2*(tau))
icc<-function(tau,log_s) sa(tau)/(sa(tau)+sa(log_s))
d1<-Deriv(icc,"tau")
d2<-Deriv(icc,"log_s")
se2<- function(ltheta,log_s) ifelse(ltheta == 0,exp(2*log_s),exp(2*ltheta+2*log_s))
icc2<-function(tau,ltheta,log_s) sa(tau)/(sa(tau)+se2(ltheta,log_s))
e1<-Deriv(icc2,"tau")
e2<-Deriv(icc2,"ltheta")
e3<-Deriv(icc2,"log_s")

Z_trans<-function(icc,k) 0.5*log((1+(k-1)*icc)/(1-icc))
dZ<-Deriv(Z_trans,"icc")

K_trans<-function(icc,k) sqrt((k-1)/(2*k))*log((1+(k-1)*icc)/(1-icc))
dK<-Deriv(K_trans,"icc")
Z_inv_KG <- function(z,m,N){
  s <- sqrt((2*m)/(m-1))
  r <- (exp(s*z)-1)/(exp(s*z)+m-1)
  return(r)
}


var_Z_D <-function(n,k){
  #if (k==2) var_z<-(n-1.5) else var_z <- k/(2*(k-1)*(n-2))
  var_z <- k/(2*(k-1)*(n-2))
  var_z
}

cov_Z_D<-function(n,k1,k2,r1,r2,r12) {
  (k1*k2*r12^2)/(2*n*(1+(k1-1)*r1)*(1+(k2-1)*r2))
}

cov_K_D<-function(n,k1,k2,r1,r2,r12) {
  sqrt(k1*k2*(k1-1)*(k2-1))*(r12^2)/(n*(1+(k1-1)*r1)*(1+(k2-1)*r2))
}


# Nombre repliques per grup
nrep<-function(data){
  
  xx <- data %>% group_by(ind,type) %>% dplyr::summarise(k=n(),.groups = 'drop') %>% ungroup()
  xx <- xx %>% mutate(k2=k^2) %>% group_by(type) %>% dplyr::summarise(sk=sum(k),sk2=sum(k2), N=n(), .groups = 'drop') %>% 
    mutate(k0=(sk-sk2/sk)/(N-1)) %>% ungroup()
  
  k01<-xx$k0[1]
  k02<-xx$k0[2]
  return(c(k01,k02))
}

# Nombre d'individus
nsub<-function(data){
  zz<-data %>% group_by(ind) %>% dplyr::summarize(N=n(), .groups = 'drop')
  return(nrow(zz))
  
}


bca<-function (theta, t0, cl = 0.95)
{
  
  theta<-theta[!is.na(theta)]
  cl_low <- (1 - cl)/2
  cl_hi <- 1 - cl_low
  nsims <- length(theta)
  z.inv <- length(theta[theta < t0])/nsims
  z <- qnorm(z.inv)
  U <- (nsims - 1) * (t0 - theta)
  A1 <- sum(U^3)
  A2 <- 6 * (sum(U^2))^{
    3/2
  }
  a <- A1/A2
  ll.inv <- pnorm(z + (z + qnorm(cl_low))/(1 - a * (z + qnorm(cl_low))))
  ll <- quantile(theta, ll.inv, names = FALSE)
  ul.inv <- pnorm(z + (z + qnorm(cl_hi))/(1 - a * (z + qnorm(cl_hi))))
  ul <- quantile(theta, ul.inv, names = FALSE)
  return(c(ll, ul))
}


remove.NA<-function(x)  x[is.na(x)==FALSE]



# Log-normal

# Expectation
mu_log<-function(m,s) {
  exp(m+(s^2)/2)
}
# Variance
var_log<-function(m,s) {
  (exp(s^2)-1)*(exp(2*m+s^2))
}

sk_log<-function(s) {
  (exp(s^2)+2)*sqrt(exp(s^2)-1)
}

# mu3
mu3_log<-function(m,s) sk_log(s)*var_log(m,s)^1.5



dif_rho<-function(ICC) {
  apply(ICC,1,diff)
}

dif_Z<-function(ICC,k,rg){
  Z1<-Z_trans(ICC[,1],k[1])
  Z2<-Z_trans(ICC[,2],k[2])
  theta<-0.5*log((1+(k[1]-1)*rg)/(1+(k[2]-1)*rg))
  Z1-Z2-theta
}

dif_K<-function(ICC,k,ns,rg){
  
  K1<-K_trans(ICC[,1],k[1])
  K2<-K_trans(ICC[,2],k[2])
  theta_M <- (1/ns)*((7-5*k[1])/sqrt(18*k[1]*(k[1]-1))-(7-5*k[2])/sqrt(18*k[2]*(k[2]-1)))
  theta_K<-K_trans(rg,k[1])-K_trans(rg,k[2]) - theta_M
  dif_K<-K1-K2-theta_K
  dif_K
}
