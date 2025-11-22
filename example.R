library(iccCompare)
library(MASS)
library(dplyr)

load("sin_dat.RData")

dades_sin <- dades_sin |> mutate(Section=factor(Section,levels=c("Day","Night")))

sin<-dades_sin %>% group_by(Section) %>% summarise(Skewness=round(moments::skewness(Sinuosity),2))

formatted_sin <- as.data.frame(sin)
formatted_sin <- format(formatted_sin, digits = 2)
ggplot(dades_sin) + 
  geom_density(aes(x = Sinuosity, color = Section), adjust = 2, linewidth = 2) + 
  ggtitle("Sinuosity values") + 
  annotate("table", x = 50, y = 0.12, label = list(formatted_sin), cex = 5) + 
  ylab("Density") + 
  xlab("Sinuosity %") + 
  theme_bw()

set.seed(2025)
res_icc<-icc_dep_test(dades_sin,ry="Sinuosity",rind="id",rtype="Section",
  Wald = TRUE,  Boot = TRUE, nboot = 500,  Perm = TRUE, nperm = 100,
  future_seed = TRUE
)

res_LRT<-ICC_LR_test(dades_sin,ry="Sinuosity",rind="id",rtype="Section")
