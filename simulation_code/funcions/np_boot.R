

resamp_model<-function(i,Bmat,dataset,g_rows,ns,ry,rtype){
  cls <- Bmat[,i]
  xx<-lapply(g_rows[cls],length)
  new_id <- rep(1:ns,times=unlist(xx))
  idx <- unlist(g_rows[cls], recursive = FALSE)
  resamp_data <- dataset[idx, ]
  # Change ind tags
  resamp_data[,"ind"] <- new_id
  resamp_data<-as.data.frame(resamp_data)
  
  
  new_model <- fit_model_dep_icc(resamp_data,ry,"ind",rtype)
  
  if(inherits(new_model,"lme")){
    out<-icc_est_dep(new_model)$ICC
  } else {
    out<-NA
  }
  return(out)
  #if((inherits(new_model,"try-error"))|(is.null(new_model))){
   #   return(NA)
    #}
  }  



np_boot<- function(dataset,ry, rind,rtype,nboot=100,parallel=TRUE,
                   workers=15,
                   future_seed=123,progress=TRUE){

# Resample matrix
ns<-nsub(dataset)
dots <- as.name(rind)
grouped <- dplyr::group_by(dataset, !!dots)
g_rows <- dplyr::group_rows(grouped)
id <- seq_along(g_rows)
Bmat <- matrix(sample(rep(id,nboot)),nrow=length(id),byrow=FALSE)
if(anyNA(Bmat)){
  warning("NA values in resample matrix.")
}


if (parallel){
    ncores <- parallelly::availableCores(omit = 1)
    if(workers >= ncores){
      workers <- ncores
    }
    
    
      oplan <- future::plan("multisession", workers = workers)
      on.exit(future::plan(oplan))
}

if(progress){  
    with_progress({
      
      p <- progressr::progressor(steps = nboot)  
      
      icc_resamples<-furrr::future_map(as.integer(1:nboot), ~ {
        p()
        Sys.sleep(.2)
        resamp_model(.x,Bmat,dataset,g_rows,ns,ry,rtype)
      },.options = furrr::furrr_options(seed = future_seed),
      p = p
      )
     
       },enable=TRUE)

}else{
  
  icc_resamples<-furrr::future_map(as.integer(1:nboot), ~ {
    resamp_model(.x,Bmat,dataset,g_rows,ns,ry,rtype)
  },.options = furrr::furrr_options(seed = future_seed)
  )
  

}
# Extreu les diagonals de cada matriu
icc_resamples<-icc_resamples[!is.na(icc_resamples)]
diagonals <- lapply(icc_resamples, diag)

# Converteix la llista de diagonals en un data frame
df <- do.call(rbind, lapply(diagonals, function(x) data.frame(r1 = x[1], r2 = x[2])))

m_boot<-apply(df,2,mean)
S<-diag(apply(df,2,var))
S[1,2]<-S[2,1]<-var(df$r1,df$r2)


return(list(theta=df,m_boot=m_boot,S_boot=S))

}

