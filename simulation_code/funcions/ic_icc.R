ic_icc <-function(iccmat,Smat,alpha=0.05, m = c(2,2), N){
  

    # Fisher Z transform
    z1 <- Z_trans(iccmat[1,1],m[1])
    z2 <- Z_trans(iccmat[2,2],m[2])
    
    se.z1 <- dZ(iccmat[1,1],m[1])*sqrt(Smat[1,1])
    se.z2 <- dZ(iccmat[2,2],m[2])*sqrt(Smat[2,2])
    
    ic.z1 <- z1+c(-1,1)*qnorm(1-alpha/2)*se.z1
    ic.z2 <- z2+c(-1,1)*qnorm(1-alpha/2)*se.z2
    ic.icc_Z1 <- (exp(2*ic.z1)-1)/(exp(2*ic.z1)+m[1]-1)
    ic.icc_Z2 <- (exp(2*ic.z2)-1)/(exp(2*ic.z2)+m[2]-1)

    # Konishi Gupta Z transform
    z1 <- K_trans(iccmat[1,1],m[1])
    z2 <- K_trans(iccmat[2,2],m[2])
    bias1 <-  (7-5*m[1])/(N*sqrt(18*m[1]*(m[1]-1)))
    bias2 <-  (7-5*m[2])/(N*sqrt(18*m[2]*(m[2]-1)))
    
    se.z1 <- dK(iccmat[1,1],m[1])*sqrt(Smat[1,1])
    se.z2 <- dK(iccmat[2,2],m[2])*sqrt(Smat[2,2])
    
    ic.z1 <- z1-bias1+c(-1,1)*qnorm(1-alpha/2)*se.z1
    ic.z2 <- z2-bias2+c(-1,1)*qnorm(1-alpha/2)*se.z2
    
    ic.icc_K1 <- Z_inv_KG(ic.z1,m[1],N)
    ic.icc_K2 <- Z_inv_KG(ic.z2,m[2],N)

    ic.icc1 <- iccmat[1,1] + c(-1,1)*qnorm(1-alpha/2)*sqrt(Smat[1,1])
    ic.icc2 <- iccmat[2,2] + c(-1,1)*qnorm(1-alpha/2)*sqrt(Smat[2,2])

  result <- matrix(c(ic.icc1,ic.icc2,ic.icc_Z1,ic.icc_Z2,ic.icc_K1,ic.icc_K2),
                   ncol=4,byrow=TRUE)
  
  colnames(result)<-c("r1_LL95%","r1_UL95%","r2_LL95%","r2_UL95%")
  rownames(result)<-c("As.","F","KG")
  return(result)
}
