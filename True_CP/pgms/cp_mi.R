source("init.R")
source("funs/ni.d.R")
source("funs/add.X.R")
source("funs/wald.ci.R")
source("funs/miss.fun.R")
source("funs/mice.run.R")
source("funs/mi.comb.R")
source("funs/miss.param.assign.R")

library(dplyr)

scenario <- 1
sc <- data.frame(expand.grid(p_C = seq(0.65, 0.95, 0.1),
                             M2 = c(0.025, 0.1, 0.2, 0.5),
                             n = c(100,500)))%>%
  dplyr::mutate(scenario.id = seq(1, length(n),1))

sc <- sc%>%
  dplyr::filter(scenario.id == scenario)
# library(parallel)
# cl <- makeCluster(Sys.getenv()["SLURM_NTASKS"], type = "MPI")
# 
# system.time({
#   
#   
#   parallel::clusterExport(cl, varlist = ls())
#   
#   x1 <- 
#     parallel::clusterApply(cl,
#                            x = 1:5000, 
#                            fun=function(x){
                             
                             
 library(tidyr, warn.conflicts = F, quietly = T)
 library(dplyr, warn.conflicts = F, quietly = T)
 library(purrr, warn.conflicts = F, quietly = T)
 library(reshape2, warn.conflicts = F, quietly = T)
 library(mice, warn.conflicts = F, quietly = T)
 
x<-1
 set.seed(100000*scenario + x)                                                   
 #generate full data with desired correlation structure
 dt <- ni.d(N_T = sc$n,
            N_C = sc$n,
            p_T = sc$p_C - sc$M2,
            p_C = sc$p_C)
 
 dt1<-dt%>%
   dplyr::mutate(x.str = case_when(y==0 ~ rbinom(1,1,0.6),
                                   y==1 ~ rbinom(1,1,0.2)))

 #define missingness parameters and do rates
 m.param <- miss.param.assign(do = do.val)
 
 #impose missing values and perform analysis
 m.param%>%
   group_split(missing)%>%
   purrr::set_names(sort(m.param$missing))%>%
   purrr::map_df(.f=function(xx,df){
     miss.fun(df = dt.H0, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, do = do.val,
              M2 = ss$M2,
              ci.method = wald.ci,
              mice.anal = TRUE)
   },df = y, .id = 'missing')%>%
   dplyr::mutate(scenario.id = ss$scenario.id,
                 p_C = ss$p_C,
                 p_T = ss$p_C,
                 M2 = ss$M2,
                 type = 't.H0',
                 do = do.val,
                 sim.id = x)
                           })
  
})
saveRDS(x1, sprintf("results/outH0_%s_%s%d_%d_set%d.rds",rho.val, method, scenario, round(100*do.val,0), setn))





