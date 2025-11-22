

sim_res<-function(out,dist="N",nrep){
  
  out<-compact(out)
  
  nsim<-length(out)
  
  res_sim_icc<-1:nsim %>% map_dfr(~{
    data.frame(r1=out[[.x]]$ICC[1,1],r2=out[[.x]]$ICC[2,2],
               r12=out[[.x]]$ICC[1,2])
    
  }
  )
  est_rho<-apply(res_sim_icc,2,mean)  
  
  
  res_sim_cov<-1:nsim %>% map_dfr(~{
    data.frame(
      cov_As_r1=between(rho.true[1,1],out[[.x]]$ci[1,1],out[[.x]]$ci[1,2]),
      cov_As_r2=between(rho.true[2,2],out[[.x]]$ci[1,3],out[[.x]]$ci[1,4]),
      cov_F_r1=between(rho.true[1,1],out[[.x]]$ci[2,1],out[[.x]]$ci[2,2]),
      cov_F_r2=between(rho.true[2,2],out[[.x]]$ci[2,3],out[[.x]]$ci[2,4]),
      cov_KG_r1=between(rho.true[1,1],out[[.x]]$ci[3,1],out[[.x]]$ci[3,2]),
      cov_KG_r2=between(rho.true[2,2],out[[.x]]$ci[3,3],out[[.x]]$ci[3,4]),
      cov_BCa_r1=between(rho.true[1,1],out[[.x]]$ci[4,1],out[[.x]]$ci[4,2]),
      cov_BCa_r2=between(rho.true[2,2],out[[.x]]$ci[4,3],out[[.x]]$ci[4,4])
      
      
      
    )
  }
  )
  
  cov_rho<-colMeans(res_sim_cov,na.rm = TRUE)
  
  
  if (dist=="LN"){
    res_sim_sk<-1:nsim %>% map_dfr(~{
      out[[.x]]$res_sk
    }
    )
    
    sk<-res_sim_sk %>% group_by(type) %>% summarise(m_sk=mean(sk))
  }
  else {
    sk<-NULL
  }
  

  res_sim_pval<-1:nsim %>% map_dfr(~{
    
     data.frame(
      matrix(
        c(out[[.x]]$pvals[-16]<0.05,out[[.x]]$pvals[16]),nrow=1))
  }
  )
  

  names(res_sim_pval)<-c("W_As","WZ_As","WK_As","WZ_Don","WK_Don","W_boot",
                         "WZ_boot","WK_boot","Perm","rho_boot","Z_boot","K_boot",
                         "rho_chisq_boot","Z_chisq_boot","K_chisq_boot",
                         "LRT","ci_BCa")
  
  res_p<-colMeans(res_sim_pval,na.rm = TRUE)
  
  res_sim_pchisq<-1:nsim %>% map_dfr(~{
    
    data.frame(
      p_chisq_as(out[[.x]])<0.05
    )
  }
    
  )
  
 
  res_p[18]<-colMeans(res_sim_pchisq,na.rm = TRUE)
  names(res_p)[18]<-"Chisq.Test" 
  
  
  return(list(nsim=nsim,r=est_rho,cov=cov_rho,sk=sk,p=res_p))
  
  
}


sim_res2<-function(out){
  
  out<-compact(out)
  
  nsim<-length(out)
  
  res_sim_icc<-1:nsim %>% map_dfr(~{
    data.frame(r1=out[[.x]]$ICC[1,1],r2=out[[.x]]$ICC[2,2],
               r12=out[[.x]]$ICC[1,2])
    
  }
  )
  est_rho<-apply(res_sim_icc,2,mean)  
  

  res_sim_cov<-1:nsim %>% map_dfr(~{
    data.frame(
      cov_As_r1=between(rho.true[1,1],out[[.x]]$ci[1,1],out[[.x]]$ci[1,2]),
      cov_As_r2=between(rho.true[2,2],out[[.x]]$ci[1,3],out[[.x]]$ci[1,4]),
      cov_F_r1=between(rho.true[1,1],out[[.x]]$ci[2,1],out[[.x]]$ci[2,2]),
      cov_F_r2=between(rho.true[2,2],out[[.x]]$ci[2,3],out[[.x]]$ci[2,4]),
      cov_KG_r1=between(rho.true[1,1],out[[.x]]$ci[3,1],out[[.x]]$ci[3,2]),
      cov_KG_r2=between(rho.true[2,2],out[[.x]]$ci[3,3],out[[.x]]$ci[3,4])

    )
  }
  )
  
  cov_rho<-colMeans(res_sim_cov)
  
  
  res_sim_pval<-1:nsim %>% map_dfr(~{
      data.frame(
      matrix(
        c(out[[.x]]$pvals<0.05),nrow=1))
  }
  )
  
  
  
  names(res_sim_pval)<-c("W_As_log","WZ_As_log","WK_As_log","WZ_Don_log","WK_Don_log",
                         "LRT_log")
  
  res_p<-colMeans(res_sim_pval)
  
  
  
  res_sim_pchisq<-1:nsim %>% map_dfr(~{
    
    data.frame(
      p_chisq_as(out[[.x]])<0.05
    )
  }
  
  )
  
  
  res_p[7]<-colMeans(res_sim_pchisq,na.rm = TRUE)
  names(res_p)[7]<-"Chisq.Test_log" 
  
  return(list(nsim=nsim,r=est_rho,cov=cov_rho,p=res_p))
  
  
}


p_chisq_as<-function(data){
  
  icc<-matrix(diag(data$ICC),ncol=1)
  
  k <- length(icc) # vector length
  comb <- combn(k, 2)  # all pairs (i < j)
  
  L <- matrix(0, nrow = ncol(comb), ncol = k)
  for (i in 1:ncol(comb)) {
    L[i, comb[2, i]] <- 1   # positive
    L[i, comb[1, i]] <- -1  # negative
  }
  
  dif<-L %*% icc

  dev_icc<-matrix(rep(0,k*k),nrow=k)

  
  # Asymptotic
  
  TT<-t(dif)%*%ginv(L%*%data$S%*%t(L))%*%dif
  p_chisq_As<-1-pchisq(TT,k-1)
  
  colnames(p_chisq_As)<-"Asym. Chi-Square"
  
  

  
  return(p_chisq_As)
  
}

