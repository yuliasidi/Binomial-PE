#!/home/statsadmin/R/bin/Rscript

args <- commandArgs()
idx <- as.numeric(args[length(args)])+1
.libPaths('ysidi/lib')

library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(mice, warn.conflicts = F, quietly = T)

source("init.R")
source("ni.d.R")
source("add.X.R")
source("FM.CI.R")
source("p.rmle.fm.R")
source("miss.fun.R")
source("mice.run.R")
source("mi.comb.R")
source("miss.param.assign.R")

ss.bounds <- readRDS("ss.bounds.rds")

meth <- 'fm'
scenario <- 15

ss <- ss.bounds%>%
  dplyr::filter(method == meth, scenario.id == scenario)

do.val <- 0.10

rho.val <- 'p30'

setn <- 1

system.time({
  

  
 set.seed(8273 + idx + scenario + setn)                                                   
 #generate full data with desired correlation structure
 dt.H0 <- ni.d(N_T = ss$n.arm,
               N_C = ss$n.arm,
               p_T = ss$p_T - ss$M2,
               p_C = ss$p_T)%>%
   add.X(rho=0.3, ss$ub)
 
 #define missingness parameters and do rates
 m.param <- 
   tibble::tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mnar1", "mnar2"))
 m.param <- m.param%>%miss.param.assign()
                           
 #impose missing values and perform analysis
x1 <-  m.param%>%
   split(.$missing)%>%
   purrr::set_names(sort(m.param$missing))%>%
   purrr::map_df(.f=function(xx,df){
     miss.fun(df = dt.H0, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, do = do.val,
              M2 = ss$M2,
              ci.method = FM.CI,
              mice.anal = TRUE)
     },df = y, .id = 'missing')%>%
   dplyr::mutate(scenario.id = ss$scenario.id,
                 p_C = ss$p_C,
                 p_T = ss$p_C,
                 M2 = ss$M2,
                 type = 't.H0',
                 do = do.val)

}) 
saveRDS(x1, sprintf("outH0_set1_%s_%s%d_%d_%d.rds",rho.val, meth, scenario, round(100*do.val,0), idx))





