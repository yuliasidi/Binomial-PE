source("init_m5.R")
source("funs/ni.d.R")
source("funs/add.X.R")
source("funs/wn.ci.R")
source("funs/miss.fun.wnsimp.R")
source("funs/mice.run.wncomb.simp.R")
source("funs/miss.param.assign.R")

library(dplyr)

ss.bounds <- readRDS("ss.bounds.rds")

method <- 'wnsimp'
scenario <- 17

ss <- ss.bounds%>%
  dplyr::filter(method == "wn", scenario.id == scenario)

do.val <- 0.2

rho.val <- 'p30'

setn <- 1

library(parallel)
cl <- makeCluster(Sys.getenv()["SLURM_NTASKS"], type = "MPI")

system.time({
  

parallel::clusterExport(cl, varlist = ls())

x1 <- 
  parallel::clusterApply(cl,
                         x = 1:5000, 
                         fun=function(x){
                           
                           
 library(tidyr, warn.conflicts = F, quietly = T)
 library(dplyr, warn.conflicts = F, quietly = T)
 library(purrr, warn.conflicts = F, quietly = T)
 library(reshape2, warn.conflicts = F, quietly = T)
 library(mice, warn.conflicts = F, quietly = T)
  
 set.seed(10000*scenario + x)                                                   
 #generate full data with desired correlation structure
 dt.H0 <- ni.d(N_T = ss$n.arm,
               N_C = ss$n.arm,
               p_T = ss$p_C - ss$M2,
               p_C = ss$p_C)%>%
   add.X(rho=0.3, ss$ub)
 
 #define missingness parameters and do rates
m.param <- miss.param.assign(do = do.val)

 #impose missing values and perform analysis
  m.param%>%
   group_split(missing)%>%
   purrr::set_names(sort(m.param$missing))%>%
   purrr::map_df(.f=function(xx,df){
     miss.fun.wnsimp(df = dt.H0, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, do = do.val,
              M2 = ss$M2,
              ci.method = wn.ci,
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
saveRDS(x1, sprintf("results/outH0_%s_%s%d_%d_set%d_mi5.rds",rho.val, method, scenario, round(100*do.val,0), setn))





