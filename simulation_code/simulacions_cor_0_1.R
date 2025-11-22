library(nlme)
library(dplyr)
library(furrr)
library(progressr)
library(MASS)
library(bbmle)


# Load functions ----

options(dplyr.summarise.inform = FALSE)



file_list <- list.files(path="./funcions")
for (i in 1:length(file_list)){
  print(i)
  source(paste("./funcions/",file_list[[i]],sep=""))
}



plan(list(
  tweak(multisession, workers = availableCores() %/% 8),
  tweak(multisession, workers = I(8))
))

on.exit(future::plan(oplan))

plan(list(
  tweak(multisession, workers = availableCores() %/% 2),
  tweak(multisession, workers = I(2))
))


on.exit(future::plan(oplan))



# dir.create("./simulacions/temp2")
# Type-I error rho=0.8 r12=0.1-------




s1<-1
s2<-1
r1=0.8
r2=0.8

sb1<-r1*s1
sb2<-r2*s2
sr1<-s1-sb1
sr2<-s2-sb2
r12<-0.1
s12<-sqrt(s1)*sqrt(s2)*r12

Gmat<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)
m1=0
m2=0
G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true



### Normal constant -----

#### n=25, k=2, opt.rep=1, type "N" --------------




t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="N")
                 
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)

#save.image("./simulacions/sim_r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)

#save.image("./simulacions/sim_r_0_8_cor_0.1_n25_k5_5.RData")


#### n=25, k=20, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)



#### n=100, k=2, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)

#save.image("./simulacions/sim_r_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)


#save.image("./simulacions/sim_r_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)


### Normal var -----
#### n=25, k=2, opt.rep=1, type "N" --------------




t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("./simulacions/sim_r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("./simulacions/sim_r_0_8_cor_0.1_n25_k5_5.RData")


#### n=25, k=20, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


#### n=100, k=2, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("./simulacions/sim_r_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("./simulacions/sim_r_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_8_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


### Log-Normal Heavy ------

#dir.create("./simulacions/temp_LN")

m1<-0
m2<-0
r1=0.8
r2=0.8
sb1<-0.7
sb2<-0.7
r12<-0.2



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true

# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den



# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN



#### n=25, k=2, opt.rep=1, type "LN" --------------
t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
                  ~{
                    p()
                    Sys.sleep(.2)
                    out<-tryCatch({
                      sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="LN")
                      
                    },
                    error = function(e) {
                      message(paste("Error en la iteració", i, ":", e$message))
                      return(NULL)
                    }
                    )
                    name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
                    saveRDS(out, file = name_file)
                    
                  },
                  .options = furrr_options(seed = TRUE),
                  .progress = TRUE,
                  p=p
                  
                  
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#save.image("./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_r_0_8_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()


#### n=100, k=2, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_r_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_r_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()


### Log-Normal Heavy var -------


#### n=25, k=2, opt.rep=2, type "LN" --------------
t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#save.image("./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_r_0_8_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


#### n=100, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_r_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_r_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_8_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()



### Log-Normal Light ------



m1<-0
m2<-0
r1=0.8
r2=0.8
sb1<-0.2
sb2<-0.2
r12<-0.15



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den


# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN


#### n=25, k=2, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n25_k5_5.RData")


#### n=25, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n25_k5_5.RData")



#### n=100, k=2, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                              nboot=500, nperm=100,parallel=FALSE,
                              type="LN")
                 
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1


# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1


# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n100_k5_5.RData")



### Log-Normal Light var ------






#### n=25, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n25_k5_5.RData")


#### n=25, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n25_k5_5.RData")





#### n=100, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1


# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_r_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1


# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_8_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()



# Power rho1=0.6 rho2=0.8 r12=0.1 -------



s1<-1
s2<-1
r1=0.6
r2=0.8

sb1<-r1*s1
sb2<-r2*s2
sr1<-s1-sb1
sr2<-s2-sb2
r12<-0.1
s12<-sqrt(s1)*sqrt(s2)*r12

Gmat<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)
m1=0
m2=0
G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


### Normal -----

#### n=25, k=2, opt.rep=1, type "N" --------------



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.RData")

#### n=25, k=20, opt.rep=1, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=2, opt.rep=1, type "N" --------------

# # S'ha tallat i completem les combinacions que falten
# file_list <- list.files(path="./simulacions/temp")
# numeros <- gsub("sim_(\\d+).rds", "\\1", file_list)
# numeros<-as.numeric(numeros)
# # Crear un vector amb els nombres de 1 a 1000
# totals <- 1:1000
# # Trobar els nombres que no estan en el vector 'numeros'
# no_present <- setdiff(totals, numeros)



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=1, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.RData")

#### n=100, k=20, opt.rep=1, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()



### Normal var -----

#### n=25, k=2, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=2, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.RData")

#### n=25, k=20, opt.rep=2, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=2, opt.rep=2, type "N" --------------

# # S'ha tallat i completem les combinacions que falten
# file_list <- list.files(path="./simulacions/temp")
# numeros <- gsub("sim_(\\d+).rds", "\\1", file_list)
# numeros<-as.numeric(numeros)
# # Crear un vector amb els nombres de 1 a 1000
# totals <- 1:1000
# # Trobar els nombres que no estan en el vector 'numeros'
# no_present <- setdiff(totals, numeros)



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.RData")

#### n=100, k=20, opt.rep=2, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_6_r2_0_8_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




### Log-Normal Heavy ------



m1<-0
m2<-0
r1=0.6
r2=0.8
sb1<-1
sb2<-0.7
r12<-0.2



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true

# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den



# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN



#### n=25, k=2, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()





#### n=100, k=2, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()

### Log-Normal Heavy var ------



#### n=25, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




#### n=100, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_6_r2_0_8_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




### Log-Normal Light ------


m1<-0
m2<-0
r1=0.6
r2=0.8
sb1<-0.2
sb2<-0.1
r12<-0.15




# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den


# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN




#### n=25, k=2, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()





#### n=100, k=2, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.RData")




#### n=100, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()




### Log-Normal Light var ------





#### n=25, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




#### n=100, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_Power_r1_0_6_r2_0_8_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




# Type-I error rho=0.4 r12=0.1 -------



s1<-1
s2<-1
r1=0.4
r2=0.4

sb1<-r1*s1
sb2<-r2*s2
sr1<-s1-sb1
sr2<-s2-sb2
r12<-0.1
s12<-sqrt(s1)*sqrt(s2)*r12

Gmat<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)
m1=0
m2=0
G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


### Normal -----



#### n=25, k=2, opt.rep=1, type "N" --------------


# save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n25_k2_2.RData")


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#### n=25, k=5, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k5_5.RData")


#### n=100, k=2, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k5_5.RData")




### Normal var -----

#### n=25, k=2, opt.rep=2, type "N" --------------


# save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k2_2.RData")


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#### n=25, k=5, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k5_5.RData")







#### n=100, k=2, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_4_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k5_5.RData")






### Log-Normal Heavy ------



m1<-0
m2<-0
r1=0.4
r2=0.4
sb1<-1.45
sb2<-1.45
r12<-0.4



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true

# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den



# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN


#### n=25, k=2, opt.rep=1, type "LN" --------------
t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#save.image("./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")




#### n=25, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()








#### n=100, k=2, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()



### Log-Normal Heavy var ------




#### n=25, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")



#### n=25, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")




#### n=25, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()






#### n=100, k=2, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_4_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


### Log-Normal Light ------


m1<-0
m2<-0
r1=0.4
r2=0.4
sb1<-0.15
sb2<-0.1
r12<-0.275



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den


# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN







#### n=25, k=2, opt.rep=1, type "LN" --------------
t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()



#save.image("./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")




#### n=25, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()






#### n=100, k=2, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()





## Log-Normal Light var ------



#### n=25, k=2, opt.rep=2, type "LN" --------------
t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#save.image("./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")




#### n=25, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()










#### n=100, k=2, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_4_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


# Power rho1=0.4 rho2=0.2 r12=0.1 -------


s1<-1
s2<-1
r1=0.4
r2=0.2

sb1<-r1*s1
sb2<-r2*s2
sr1<-s1-sb1
sr2<-s2-sb2
r12<-0.1
s12<-sqrt(s1)*sqrt(s2)*r12

Gmat<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)
m1=0
m2=0
G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


### Normal -----

#### n=25, k=2, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()







#### n=25, k=5, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k5_5.RData")


#### n=100, k=2, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k2_2.RData")



### Extra LRT


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 10)
  
  future_map(1:10,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc_LRT(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,type="N")
                 
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1


# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:10) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n100_k2_2_LRT.rds")
remove(resultats_totals)
gc()
gc()


#### n=100, k=5, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()



### Normal var -----




#### n=25, k=2, opt.rep=2, type "N" --------------



t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#### n=25, k=5, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n25_k5_5.RData")






#### n=100, k=2, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_4_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_power_r1_0_4_r2_0_2_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




### Log-Normal Heavy ------



m1<-0
m2<-0
r1=0.4
r2=0.2
sb1<-1.45
sb2<-2
r12<-0.55



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true

# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den



# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN



#### n=25, k=2, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#### n=25, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()







#### n=25, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()





#### n=100, k=2, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#### n=100, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()






#### n=100, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()




### Log-Normal Heavy var ------



#### n=25, k=2, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#### n=25, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()







#### n=25, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




#### n=100, k=2, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#### n=100, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()








#### n=100, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_power_r1_0_4_r2_0_2_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




## Log-Normal Light ------



m1<-0
m2<-0
r1=0.4
r2=0.2
sb1<-0.1
sb2<-0.2
r12<-0.4



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


# Skewness expected


# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den



# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN





#### n=25, k=2, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#### n=25, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()






#### n=25, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()






#### n=100, k=2, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#### n=100, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()






#### n=100, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1
# Combinar tots els resultats en un únic fitxer
i<-1


resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  if (file.exists(fitxer_temp)){
    
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
  }
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()




## Log-Normal Light var ------





#### n=25, k=2, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#### n=25, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()






#### n=25, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=2, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#### n=100, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()






#### n=100, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_power_r1_0_4_r2_0_2_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


# Type-I error rho=0.2 r12=0.1 -------



s1<-1
s2<-1
r1=0.2
r2=0.2

sb1<-r1*s1
sb2<-r2*s2
sr1<-s1-sb1
sr2<-s2-sb2
r12<-0.1
s12<-sqrt(s1)*sqrt(s2)*r12

Gmat<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)
m1=0
m2=0
G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


### Normal -----



#### n=25, k=2, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()




#### n=25, k=5, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n25_k5_5.RData")


### Ordinador Despatx 2 -----

#### n=100, k=2, opt.rep=1, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=1, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp3/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp3/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()

#### Fins aquí -----



### Normal var -----


#### n=25, k=2, opt.rep=2, type "N" --------------


# save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n25_k2_2.RData")


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#### n=25, k=5, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n25_k5_5.RData")



#### n=25, k=20, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n25_k5_5.RData")





#### n=100, k=2, opt.rep=2, type "N" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()

#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n100_k2_2.RData")


#### n=100, k=5, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()



#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_r_0_2_cor_0.1_n100_k5_5.RData")



#### n=100, k=20, opt.rep=2, type "N" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="N")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_r_0_2_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()






### Log-Normal Heavy ------



m1<-0
m2<-0
r1=0.2
r2=0.2
sb1<-1.8
sb2<-1.8
r12<-0.7



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true

# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den



# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN






#### n=25, k=2, opt.rep=1, type "LN" --------------
t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()


#save.image("./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")




#### n=25, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()




## Fins aquí ----




#### n=100, k=2, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()


# Fins aquí ------




### Log-Normal Heavy var ------




#### n=25, k=2, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")



#### n=25, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")




#### n=25, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=2, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_r_0_2_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()




### Log-Normal Light ------


m1<-0
m2<-0
r1=0.2
r2=0.2
sb1<-0.15
sb2<-0.1
r12<-0.4



# G matrix 
sb1_log<-var_log(m1,sqrt(sb1))
sb2_log<-var_log(m2,sqrt(sb2))
s12_log<-mu_log(m1,sqrt(sb1))*mu_log(m2,sqrt(sb2))*(exp(r12*sqrt(sb1*sb2))-1)
Gmat<-matrix(c(sb1_log,s12_log,s12_log,sb2_log),nrow=2)


#R matrix
sr1<-(1-r1)*sb1_log/r1
sr2<-(1-r2)*sb2_log/r2
Smat<-matrix(c(sr1,0,0,sr2),nrow=2)

G<-Gmat
R<-Smat

V<-G+R

D<-diag(sqrt(diag(V)))
Di<-solve(D)
rho.true<-Di%*%G%*%Di
rho.true


# Skewness expected

# Type 1
num<-mu3_log(m1,sqrt(sb1))
den<-(var_log(m1,sqrt(sb1))+sr1)^1.5
num/den

# Type 2
num<-mu3_log(m2,sqrt(sb2))
den<-(var_log(m2,sqrt(sb2))+sr2)^1.5
num/den


# G matrix Normal scale
s12<-sqrt(sb1)*sqrt(sb2)*r12
G_LN<-matrix(c(sb1,s12,s12,sb2),nrow=2)
Gmat<-G_LN



# Ordinador despatx Sara ----


#### n=25, k=2, opt.rep=1, type "LN" --------------
t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n25_k2_2.rds")
remove(resultats_totals)
gc()
gc()



#save.image("./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n25_k5_5.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")




#### n=25, k=20, opt.rep=1, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n25_k20_20.rds")
remove(resultats_totals)
gc()
gc()


# Fins aquí ------


# Ordinador seminari ------



#### n=100, k=2, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n100_k2_2.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=5, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n100_k5_5.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=20, opt.rep=1, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=1,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp2/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp2/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n100_k20_20.rds")
remove(resultats_totals)
gc()
gc()


# Fins aquí -----



## Log-Normal Light var ------


# Ordinador Passadís ----


#### n=25, k=2, opt.rep=2, type "LN" --------------
t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n25_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()


#save.image("./simulacions/sim_LN__r_0_8_cor_0.1_n25_k2_2.RData")


#### n=25, k=5, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n25_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()
#save.image("C:/Users/jlcarrasco/OneDrive - Universitat de Barcelona/recerca/ICC dependent/simulacio/sim_LN__r_0_4_cor_0.1_n25_k2_2.RData")




#### n=25, k=20, opt.rep=2, type "LN" --------------

t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=25,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp4/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp4/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n25_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


# Ordinador despatx  -----


#### n=100, k=2, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=2,k2=2,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n100_k2_2_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=5, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=5,k2=5,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n100_k5_5_var.rds")
remove(resultats_totals)
gc()
gc()



#### n=100, k=20, opt.rep=2, type "LN" --------------


t1<-date()
set.seed(2023)
with_progress({
  p <- progressor(steps = 1000)
  
  future_map(1:1000,
             ~{
               p()
               Sys.sleep(.2)
               out<-tryCatch({
                 sim_icc(n=100,k1=20,k2=20,m=c(0,0),G=Gmat,R=Smat,opt.rep=2,
                         nboot=500, nperm=100,parallel=FALSE,
                         type="LN")
                 
               },
               error = function(e) {
                 message(paste("Error en la iteració", i, ":", e$message))
                 return(NULL)
               }
               )
               name_file<- paste0("./simulacions/temp_LN/sim_", .x, ".rds")
               saveRDS(out, file = name_file)
               
             },
             .options = furrr_options(seed = TRUE),
             .progress = TRUE,
             p=p
             
             
  )
}
)

t2<-date()

t2
t1

# Combinar tots els resultats en un únic fitxer

resultats_totals <- list()
for (i in 1:1000) {
  fitxer_temp <- paste0("./simulacions/temp_LN/sim_", i, ".rds")
  resultats_totals[[i]] <- readRDS(fitxer_temp)
  file.remove(fitxer_temp)  # Eliminar el fitxer temporal després de llegir-lo
}
saveRDS(resultats_totals, file = "./simulacions/sim_LN_Light_r_0_2_cor_0.1_n100_k20_20_var.rds")
remove(resultats_totals)
gc()
gc()


