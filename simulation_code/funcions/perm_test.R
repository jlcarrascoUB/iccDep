
perm_fun<-function(data){

  per_sam<-data %>% arrange(ind)
  
  k_ind<-per_sam |> group_by(ind,type) |> summarise(n=n(),.groups="drop")
  
  k_ind <- k_ind |> group_by(ind) |> mutate(type_perm=sample(type))
  
  per_sam$type<-rep(k_ind$type_perm,times=k_ind$n)
  
  mod_perm<-fit_model_dep_icc(per_sam,"y","ind","type")
  
  if(inherits(mod_perm,"lme")){
    iccest_perm<-icc_est_dep(mod_perm)
    dif_perm<-diff(diag(iccest_perm$ICC))
    
  }
  
  if((inherits(mod_perm,"try-error"))|(is.null(mod_perm))){
    dif_perm<-NA
  }

  
  return(dif_perm)
}  

perm_test<-function(dataset,ry, rind,rtype,nperm=100,parallel=TRUE,
                    workers=15,
                    future_seed=123,progress=TRUE){
  
  dades<- data.frame(dataset) |> dplyr::select(any_of(c(ry,rind,rtype)))
  
  dades <- dades |> dplyr::rename(y = all_of(ry),
                                  ind = all_of(rind),
                                  type = all_of(rtype))
  
  
  mod_orig<-fit_model_dep_icc(dades,"y","ind","type")
  iccest_orig<-icc_est_dep(mod_orig)
  dif_r<-diff(diag(iccest_orig$ICC))
  

  if(parallel){
  ncores <- parallelly::availableCores(omit = 1)
  if(workers >= ncores){
    workers <- ncores
  }
  
  
  oplan <- future::plan("multisession", workers = workers)
  on.exit(future::plan(oplan))
  }
  
  
  if(progress){
  with_progress({
    
   p <- progressr::progressor(steps = nperm)  
    
    perm_icc<-furrr::future_map(as.integer(1:nperm), ~ {
      p()
      Sys.sleep(.2)
      perm_fun(dades)
    }
    , .options = furrr::furrr_options(seed = future_seed),
    p = p
    )
    

  },enable=TRUE)

  }else{
    perm_icc<-furrr::future_map(as.integer(1:nperm), ~ {
      perm_fun(dades)
    }
    , .options = furrr::furrr_options(seed = future_seed)
    )
  }
  
  dif_r_perm<-unlist(perm_icc)
  n_r<-length(remove.NA(dif_r_perm))
  pval_r<-(sum(ifelse(abs(dif_r_perm)>=abs(dif_r),1,0),na.rm=TRUE)+1)/(n_r+1)
  
  
  return(list(pval=pval_r,perm_dif=dif_r_perm,dif_r=dif_r))
  
}



