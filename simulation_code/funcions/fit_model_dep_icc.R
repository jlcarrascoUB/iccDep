

fit_model_dep_icc<-function(dataset,ry,rind,rtype){
  
  dades<- data.frame(dataset) |> dplyr::select(any_of(c(ry,rind,rtype)))
  
  dades <- dades |> dplyr::rename(y = all_of(ry),
                                  ind = all_of(rind),
                                  type = all_of(rtype))
  
  # Model with different ICCs
  model_dif<-try(lme(y~type,random = ~-1+type|ind,
                     weights=varIdent(form=~1|type),
                     data=dades,na.action="na.omit"),silent=T)
  
  # Change optimizer in case of no convergence
  if(class(model_dif)[1]=='try-error'){
    
    model_dif<-try(lme(y~type,random = ~-1+type|ind,
                       weights=varIdent(form=~1|type),data=dades,
                       control=list(opt="optim"),na.action="na.omit"))
  }
  
  
  # Checking and fixing error in variances
  
  if(class(model_dif)[1]!='try-error'){
    if(class(model_dif$apVar)[1]=="character"){
      cat("Error on apVar")
      model_dif<-tryCatch(lme(y~type,random = ~-1+type|ind,
                              weights=varIdent(form=~1|type),
                              data=dades,
                              control=list(opt="optim",minAbsParApVar=0.1)),
                          error=function(e){})
      
      if(class(model_dif)[1]=='try-error'){
        model_dif<-NULL
        message("Model did not converge")
    }
    }
  }
  
  return(model_dif)
}


