
est_icc<-function(data,nboot=500, nperm=100,parallel=TRUE,
                  workers=15,boot=TRUE,perm=TRUE){
  
  
  
  # Replicates by group
  nr<-nrep(data)
  
  # Number of subjects
  ns<-nsub(data)
  
  # Application of methods
  
  # Model
  fmod<-fit_model_dep_icc(data,"y","ind","type")
  
  if (!is.null(fmod)){
    # ICCs dependents
    out_icc<-icc_est_dep(fmod)
    
    if (boot){
    # Bootstrap
    boot_icc<-np_boot(data,"y", "ind","type",nboot=nboot,parallel=parallel,
                      workers=workers)
    
    }
    
    if (perm){
    # Permutations
    perm_icc<-perm_test(data,ry="y", rind="ind",rtype="type",nperm=nperm,
                        parallel=parallel,workers=workers)
    #Permutacions
    p_Perm<-perm_icc$pval
    }else{
      p_Perm<-NA 
    }
    
    # Confidence intervals
    ci.icc<-ic_icc(out_icc$ICC,out_icc$S,m=nr,N=ns)
    
    if(boot){
    ci.bca_1<-bca(boot_icc$theta$r1,out_icc$ICC[1,1])
    ci.bca_2<-bca(boot_icc$theta$r2,out_icc$ICC[2,2])
    ci.icc<-rbind(ci.icc,c(ci.bca_1,ci.bca_2))
    rownames(ci.icc)[4]<-"BCa"
    }
    # Tests
    
    rg=sum(nr*diag(out_icc$ICC))/sum(nr)
    # Wald test
    p_W_As<-W_test(out_icc$ICC,out_icc$S)
    p_WZ_As<-WZ_test(out_icc$ICC,out_icc$S,nr[1],nr[2])
    p_WK_As<-WK_test(out_icc$ICC,out_icc$S,nr[1],nr[2])
    p_WZ_Don<-WZ_Donner(out_icc$ICC,out_icc$S,nr[1],nr[2],ns,rg)
    p_WK_Don<-WK_Donner(out_icc$ICC,out_icc$S,nr[1],nr[2],ns,rg)
    
    if(boot){
    # Wald test amb S obtinguda amb bootstrap
    p_W_boot<-W_test(out_icc$ICC,boot_icc$S)
    p_WZ_boot<-WZ_test(out_icc$ICC,boot_icc$S,nr[1],nr[2])
    p_WK_boot<-WK_test(out_icc$ICC,boot_icc$S,nr[1],nr[2])
    }else{
      p_W_boot<-NA
      p_WZ_boot<-NA
      p_WK_boot<-NA
      
      
    }
   
    
    # Bootstrap
    if (boot){
    boot_Dif_r<-remove.NA(dif_rho(boot_icc$theta))
    boot_Dif_Z<-remove.NA(dif_Z(boot_icc$theta,nr,rg))
    boot_Dif_K<-remove.NA(dif_K(boot_icc$theta,nr,ns,rg))
    
    # Original differences
    theta_rho<-diff(diag(out_icc$ICC))
    theta_Z<-Z_trans(out_icc$ICC[2,2],nr[2])-Z_trans(out_icc$ICC[1,1],nr[1])
    theta_K<-K_trans(out_icc$ICC[2,2],nr[2])-K_trans(out_icc$ICC[1,1],nr[1])
    
    
    p_rho_boot<-(sum( abs(boot_Dif_r-mean(boot_Dif_r)) > abs(theta_rho) ) + 1) / (length(boot_Dif_r)+1)
    p_Z_boot<-(sum( abs(boot_Dif_Z-mean(boot_Dif_Z)) > abs(theta_Z) ) + 1) / (length(boot_Dif_Z)+1)
    p_K_boot<-(sum( abs(boot_Dif_K-mean(boot_Dif_K)) > abs(theta_K) ) + 1) / (length(boot_Dif_K)+1)
    
    
    # Chi-square bootstrap
    p_rho_chisq_boot<-1-pchisq((theta_rho^2)/var(boot_Dif_r),1)
    p_Z_chisq_boot<-1-pchisq((theta_Z^2)/var(boot_Dif_Z),1)
    p_K_chisq_boot<-1-pchisq((theta_K^2)/var(boot_Dif_K),1)
    
    # Interval BCa
    ci_dif<-bca(boot_Dif_r,theta_rho)
    in_ci_Bca<-1-between(0,ci_dif[1],ci_dif[2])
    
    }else{
      p_rho_boot<-NA
      p_Z_boot<-NA
      p_K_boot<-NA
      p_rho_chisq_boot<-NA
      p_Z_chisq_boot<-NA
      p_K_chisq_boot<-NA
      ci_dif<-NA
      in_ci_Bca<-NA
      
    }
    
   
    # LRT
    
    out_LRT<-try(ICC_LR_test(ry = "y",rind = "ind",rmet = "type",
                             data = data))
    
    if(inherits(out_LRT,"try-error")) { 
      out_LRT<-list()
      out_LRT$p.value<-NA
    }
    
    p_vals<-c(p_W_As,p_WZ_As,p_WK_As,p_WZ_Don,p_WK_Don,p_W_boot,
              p_WZ_boot,p_WK_boot,p_Perm,p_rho_boot,p_Z_boot,p_K_boot,
              p_rho_chisq_boot,p_Z_chisq_boot,p_K_chisq_boot,in_ci_Bca,
              p_LRT=out_LRT$p.value)
    names(p_vals)<-c("W_As","WZ_As","WK_As","WZ_Don","WK_Don","W_boot",
                     "WZ_boot","WK_boot","Perm","rho_boot","Z_boot","K_boot",
                     "rho_chisq_boot","Z_chisq_boot","K_chisq_boot",
                     "ci_BCa","LRT")
    
    out<-list(ICC=out_icc$ICC,S=out_icc$S,ci=ci.icc,ci_dif=ci_dif,pvals=p_vals,
              model=fmod)
  }else{
    out<-NULL
  }
  
  
  return(out)
  
  
}  

