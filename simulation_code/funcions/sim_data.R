# Simula dades ------------------------------------------------------------


sim.data<-function(n,k1,k2,m,G,R,opt.rep=1){
  # n number of subjects 
  # k mean number of replicates
  # m vector of means
  # G between subjects covariance matrix
  # R within subjects covariance matrix
  
  # Subject effect

  b<-MASS::mvrnorm(n = n, mu=m, Sigma=G)
  
  
  # Type 1 data
  # Number of replicates
  #nr<-sample(1:k,n,replace=T)
  
  if (opt.rep==1) nr<-k1
  if (opt.rep==2) nr<-rpois(n,k1)+1
  
  br<-rep(b[,1],times=nr)
  
  y.1<-br+rnorm(length(br),0,sqrt(R[1,1]))
  id<-rep(1:n,times=nr)
  data_1<-data.frame(y=y.1,ind=id,type=1)
  
 
  # Type 2 data
  # Number of replicates
  if (opt.rep==1) nr<-k2
  if (opt.rep==2) nr<-rpois(n,k2)+1
  
  br<-rep(b[,2],times=nr)
  y.2<-br+rnorm(length(br),0,sqrt(R[2,2]))
  id<-rep(1:n,times=nr)
  data_2<-data.frame(y=y.2,ind=id,type=2)
  
  dades<-rbind(data_1,data_2)
  dades$type<-as.factor(dades$type)
  dades$type_ef<-as.factor(dades$type)
  dades <- dades %>% ungroup()
  return(dades)
}

# Subjects log-normal
# Error normal
sim.data_ln<-function(n,k1,k2,m=c(0,0),G,R,opt.rep=1,positive=TRUE){
  # n1 number of subjects in sample 1
  # n2 number of subjects in sample 2
  # k mean number of replicates
  # m vector of means
  # G between subjects covariance matrix
  # R within subjects covariance matrix
  
  # Subject effect
  b<-exp(MASS::mvrnorm(n = n, mu=m, Sigma=G))
  
  
  # Type 1 data
  # Number of replicates
  #nr<-sample(1:k,n,replace=T)
  
  if (opt.rep==1) nr<-k1
  if (opt.rep==2) nr<-rpois(n,k1)+1
  
  br<-rep(b[,1],times=nr)
  
  y.1<-br+rnorm(length(br),0,sqrt(R[1,1]))
  
  
  id<-rep(1:n,times=nr)
  data_1<-data.frame(y=y.1,ind=id,type=1)
  
  # Type 2 data
  # Number of replicates
  if (opt.rep==1) nr<-k2
  if (opt.rep==2) nr<-rpois(n,k2)+1
  
  br<-rep(b[,2],times=nr)
  y.2<-br+rnorm(length(br),0,sqrt(R[2,2]))
  id<-rep(1:n,times=nr)
  data_2<-data.frame(y=y.2,ind=id,type=2)
  
 dades<-rbind(data_1,data_2)
  if(positive){
    dades<-dades |> mutate(y=abs(y))
  }
  
  dades$type<-as.factor(dades$type)
  dades$type_ef<-as.factor(dades$type)
  dades <- dades %>% ungroup()
  dades<-dades %>% arrange(ind,type)
  return(dades)
}
