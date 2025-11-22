

sim_icc<-function(n,k1,k2,m,G,R,opt.rep,nboot=500, nperm=100,parallel=TRUE,
                     workers=15,type="N"){
  
  # Data simulation step
  if (type=="N"){    
    # Data simulation step Normal

      data_sim<-sim.data(n=n,k1=k1,k2=k2,m=m,G=G,R=R,opt.rep=opt.rep)
      res_sk<-NULL
    
  }

  
  if (type=="LN"){      
    # Data simulation step Log_Normal
    data_sim<-sim.data_ln(n=n,k1=k1,k2=k2,m=m,G=G,R=R,opt.rep=opt.rep) |> mutate(yt=log(y))
    res_sk <- data_sim %>% group_by(type) %>% dplyr::summarize(sk=moments::skewness(y),.groups = 'drop') %>% ungroup()
    
  }
  
  # Replicates by group
  nr<-nrep(data_sim)
  
  # Number of subjects
  ns<-nsub(data_sim)
  
  # Application of methods
  
  # Model
  
  fmod<-fit_model_dep_icc(data_sim,"y","ind","type")
  

if (!is.null(fmod)){
    # ICCs dependents
  out_icc<-icc_est_dep(fmod)

  # Bootstrap
  boot_icc<-np_boot(data_sim,"y", "ind","type",nboot=nboot,parallel=parallel,
                    workers=workers,progress=FALSE)
  

  # Permutations
  perm_icc<-perm_test(data_sim,ry="y", rind="ind",rtype="type",nperm=nperm,
                      parallel=parallel,workers=workers,progress=FALSE)
  
  
  # Confidence intervals
  ci.icc<-ic_icc(out_icc$ICC,out_icc$S,m=nr,N=ns)
  
  ci.bca_1<-bca(boot_icc$theta$r1,out_icc$ICC[1,1])
  ci.bca_2<-bca(boot_icc$theta$r2,out_icc$ICC[2,2])
  ci.icc<-rbind(ci.icc,c(ci.bca_1,ci.bca_2))
  rownames(ci.icc)[4]<-"BCa"
  
  # Tests

  rg=sum(nr*diag(out_icc$ICC))/sum(nr)
  # Wald test
  p_W_As<-W_test(out_icc$ICC,out_icc$S)
  p_WZ_As<-WZ_test(out_icc$ICC,out_icc$S,nr[1],nr[2])
  p_WK_As<-WK_test(out_icc$ICC,out_icc$S,nr[1],nr[2])
  p_WZ_Don<-WZ_Donner(out_icc$ICC,out_icc$S,nr[1],nr[2],ns,rg)
  p_WK_Don<-WK_Donner(out_icc$ICC,out_icc$S,nr[1],nr[2],ns,rg)
  # Wald test amb S obtinguda amb bootstrap
  p_W_boot<-W_test(out_icc$ICC,boot_icc$S)
  p_WZ_boot<-WZ_test(out_icc$ICC,boot_icc$S,nr[1],nr[2])
  p_WK_boot<-WK_test(out_icc$ICC,boot_icc$S,nr[1],nr[2])
  
  #Permutacions
  p_Perm<-perm_icc$pval

  
  # Bootstrap
  
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
 
  # LRT
  
  out_LRT<-try(ICC_LR_test(ry = "y",rind = "ind",rmet = "type",
                           data = data_sim))
  
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
            res_sk=res_sk)
  }else{
    out<-NULL
    }

  #Dades transformades
  if (type=="LN"){
    
    fmod_log<-fit_model_dep_icc(data_sim,"yt","ind","type")
    
    if (!is.null(fmod_log)){
      # ICCs dependents
     out_icc_log<-icc_est_dep(fmod_log)
     # Confidence intervals
     ci.icc_log<-ic_icc(out_icc_log$ICC,out_icc_log$S,m=nr,N=ns)
       # Tests
     
     rg=sum(nr*diag(out_icc_log$ICC))/sum(nr)
     # Wald test
     p_W_As_log<-W_test(out_icc_log$ICC,out_icc_log$S)
     p_WZ_As_log<-WZ_test(out_icc_log$ICC,out_icc_log$S,nr[1],nr[2])
     p_WK_As_log<-WK_test(out_icc_log$ICC,out_icc_log$S,nr[1],nr[2])
     p_WZ_Don_log<-WZ_Donner(out_icc_log$ICC,out_icc_log$S,nr[1],nr[2],ns,rg)
     p_WK_Don_log<-WK_Donner(out_icc_log$ICC,out_icc_log$S,nr[1],nr[2],ns,rg)
     
      
    }
    
    
    
    out_LRT_log<-try(ICC_LR_test(ry = "yt",rind = "ind",rmet = "type",
                             data = data_sim |> dplyr::select(yt,ind,type)))
    
    if(inherits(out_LRT_log,"try-error")) { 
      out_LRT_log<-list()
      out_LRT_log$p.value<-NA
    }
    
    
    p_vals_log<-c(p_W_As_log,p_WZ_As_log,p_WK_As_log,p_WZ_Don_log,p_WK_Don_log,
              p_LRT_log=out_LRT_log$p.value)
    names(p_vals_log)<-c("W_As_log","WZ_As_log","WK_As_log","WZ_Don_log","WK_Don_log"
                         ,"LRT_log")
    
    out_log<-list(ICC=out_icc_log$ICC,S=out_icc_log$S,ci=ci.icc_log,pvals=p_vals_log)
  }else{
    out_log<-NULL
  }
    

return(list(res_N=out,res_LN=out_log))

  
}  




sim_icc_LRT<-function(n,k1,k2,m,G,R,opt.rep,type="N"){
  
  # Data simulation step
  if (type=="N"){    
    # Data simulation step Normal
    
    data_sim<-sim.data(n=n,k1=k1,k2=k2,m=m,G=G,R=R,opt.rep=opt.rep)
    res_sk<-NULL
    
  }
  
  
  if (type=="LN"){      
    # Data simulation step Log_Normal
    data_sim<-sim.data_ln(n=n,k1=k1,k2=k2,m=m,G=G,R=R,opt.rep=opt.rep) |> mutate(yt=log(y))
    res_sk <- data_sim %>% group_by(type) %>% dplyr::summarize(sk=moments::skewness(y),.groups = 'drop') %>% ungroup()
    
  }
  
  # Replicates by group
  nr<-nrep(data_sim)
  
  # Number of subjects
  ns<-nsub(data_sim)
  
  # Application of methods
  
  # LRT
    
    out_LRT<-try(ICC_LR_test(ry = "y",rind = "ind",rmet = "type",
                             data = data_sim))
    
    if(inherits(out_LRT,"try-error")) { 
      out_LRT<-list()
      out_LRT$p.value<-NA
    }
    
    
    
 
  return(out_LRT$p.value)
  
  
}  
