source("cluster/pgms/init.R")
source("funs/dt.sim.x2.cont.R")
source("funs/miss.impose.x2.cont.R")
source("funs/wald.ci.R")
source("funs/anal.miss.run.R")
source("funs/mice.nonign.run.R")
source("funs/mice.impute.logreg.p.R")
source("funs/nested.mi.comb.R")
source("funs/miss.param.assign.x2.cont.R")

library(dplyr)

ss.bounds <- readRDS("cluster/ss.bounds.rds")

method <- 'wald'
scenario <- 21
param <- 1
anal.type <- "bw"

ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

system.time({
  
  x1 <- parallel::mclapply(X = 1:100, 
                           mc.cores = 7,
                           FUN= function(x) {
                             
                             
 
   library(tidyr, warn.conflicts = F, quietly = T)
   library(dplyr, warn.conflicts = F, quietly = T)
   library(purrr, warn.conflicts = F, quietly = T)
   library(reshape2, warn.conflicts = F, quietly = T)
   library(mice, warn.conflicts = F, quietly = T)
   library(MASS, warn.conflicts = F, quietly = T)
                             
   
   set.seed(10000*scenario + x)                                                   
   #generate full data with desired correlation structure
   dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C, n.arm = ss$n.arm, 
                         mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01)
   ci.full <- dt0%>%wald.ci(ss$M2,'y')
   
   #define missingness parameters and do rates
   m.param <- miss.param.assign.x2.cont(do = do.val, anal.type = anal.type) 
                           
   #impose missing values and perform analysis
   #impose missing values and perform analysis
   ci.miss <- m.param%>%
     dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                         anal.miss.run, df = dt0, do = do.val,
                                         ci.method = wald.ci,
                                         dt.out = T,
                                         sing.anal = F,
                                         mice.anal = F,
                                         M2 = ss$M2, seed = 10000*scenario + x,
                                         seed.mice = 10000*scenario + x))%>%
     dplyr::select(missing, results)%>%
     dplyr::mutate(scenario.id = ss$scenario.id,
                   p_C = ss$p_C,
                   M2 = ss$M2,
                   type = 't.H1',
                   do = do.val,
                   sim.id = x)
   
   ci.all <- list(ci.full, ci.miss)%>%purrr::set_names(c("ci.full","ci.miss")) 
   return(ci.all)
 })
})



x2 <- bind_rows(
  x1%>%
    purrr::map_df(.f=function(x) x$ci.miss,.id = 'sim')%>%
    tidyr::unnest())

x3 <- x2%>%
  dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,
                         ifelse(trt=="C",1,0)))

x3.T <- x3%>%filter(is.na(y.m)==T, trt=="T")
x3.C <- x3%>%filter(is.na(y.m)==T, trt=="C")

x4 <- x3%>%
  filter(missing=="mnar1")%>%
  split(.$sim)%>%
  purrr::map_df(wald.ci, y = 'y', M2 = 0.1)

x5 <- x3%>%
  filter(missing=="mnar2")%>%
  split(.$sim)%>%
  purrr::map_df(wald.ci, y = 'y', M2 = 0.1)

