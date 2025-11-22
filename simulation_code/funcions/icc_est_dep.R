icc_est_dep<-function(model_dif){
  
  if(class(model_dif)[1]!='try-error'){
    Gmat<-getVarCov(model_dif)
    
    Smat<-diag(c(1,exp(as.numeric(model_dif$modelStruct$varStruct)))*model_dif$sigma)^2
    
    Vmat<-Gmat+Smat
    D<-diag(sqrt(diag(Vmat)))
    Di<-solve(D)
    r.est<-Di%*%Gmat%*%Di
    
    vars<-attr(model_dif$apVar,"Pars")
    
    if( (class(model_dif$apVar)[1]!="character") & (sum(is.na(model_dif$apVar))==0) )
    {
      S<-model_dif$apVar[-3,-3]
      
      # Var icc1
      
      
      dev1<-matrix(c(e1(vars[1],0,vars[5]),0,0,e3(vars[1],0,vars[5])),nrow=4)
      var_icc1<-t(dev1)%*%S%*%dev1
      
      # Var icc2
      dev2<-matrix(c(0,e1(vars[2],vars[4],vars[5]),e2(vars[2],vars[4],vars[5]),
                     e3(vars[2],vars[4],vars[5])),nrow=4)
      var_icc2<-t(dev2)%*%S%*%dev2
      
      # Covariance icc1, icc2
      cov_icc12<-t(dev1)%*%S%*%dev2
      cov_mat_icc<-matrix(c(var_icc1,cov_icc12,cov_icc12,var_icc2),ncol=2,byrow = T)
      
    }
    else cov_mat_icc<-NA 
    
  } else {r.est<-NA;cov_mat_icc<-NA}
  
  return(list(ICC=r.est,S=cov_mat_icc))
} 
