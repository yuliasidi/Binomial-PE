source("cluster/pgms/init_m5.R")
source("funs/dt.sim.x2.cont.R")
source("funs/miss.impose.x2.cont.R")
source("funs/wald.ci.R")
source("funs/anal.miss.run.R")
source("funs/mice.run.R")
source("funs/mi.comb.R")
source("funs/miss.param.assign.2x.cont.R")
source("funs/norm.run.R")

library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(mice, warn.conflicts = F, quietly = T)
library(norm, warn.conflicts = F, quietly = T)
library(MASS, warn.conflicts = F, quietly = T)

method <- 'wald'
rho.val <- 'p30'
scenario <- 21

ss.bounds <- readRDS("cluster/ss.bounds.rds")
ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7"),
                    bt = c(0, 0, 0.3, 0.6, .9, -0.3, -0.6, -.9),
                    bx1 = c(0, 2, 2, 2, 2, 2, 2, 2),
                    bx2 = c(0, 0, 0, 0, 0, 0, 0, 0),
                    by = c(0, 0, 0, 0, 0, 0, 0, 0)) 
 
 x1 <- parallel::mclapply(X = 1:500, 
                           mc.cores = 4,
                           FUN= function(x)
                           {

   set.seed(10000*scenario + x)                                                   
                             
   dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                         mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 10, r12 = 0.3, b1 = 0.1, b2 = 0.05) 
   
   m.param%>%
     dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2), 
                                         anal.miss.run, df = dt0, do = do.val,
                                         ci.method = wald.ci,
                                         norm.anal = T, M2 = ss$M2, seed = 10000*scenario + x))%>%
     dplyr::select(missing, results)%>%
     dplyr::mutate(scenario.id = ss$scenario.id,
                   p_C = ss$p_C,
                   M2 = ss$M2,
                   type = 't.H0',
                   do = do.val,
                   sim.id = x)
                             
                             
                           }
  
 )
 
 

 x1.ci<-bind_rows(x1)%>%
   unnest()%>%
   dplyr::filter(seq(1,n(),1)%in%c(seq(1,n(),2)))%>%
   unnest()
 
  x1.do<-bind_rows(x1)%>%
    unnest()%>%
    dplyr::filter(seq(1,n(),1)%in%c(seq(2,n(),2)))%>%
    unnest()
  
  x1.do%>%
    unnest()%>%
    mutate(diff.do = C - T)%>%
    group_by(missing)%>%
    summarise_at(.vars=c('C','T','diff.do'), .f = mean)
  
  
  
  
  #######################################
  ############ DO = 10% #################
  #######################################
  scenario <- 21
  
  ss.bounds <- readRDS("cluster/ss.bounds.rds")
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.1
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3"),
                    b.trt = c(0, 0, 0.04, 0.09),
                    b.X = c(0, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0))
  
  
  x1 <- parallel::mclapply(X = 1:100, 
                           mc.cores = 3,
                           FUN= function(x)
                           {
                             set.seed(871 + scenario + x)                                                   
                             #generate full data with desired correlation structure
                             dt.H0 <- ni.d(N_T = ss$n.arm,
                                           N_C = ss$n.arm,
                                           p_T = ss$p_C - ss$M2,
                                           p_C = ss$p_C)%>%
                               add.X(rho=0.3, ss$ub)
                             
                             m.param%>%
                               group_split(missing)%>%
                               purrr::set_names(sort(m.param$missing))%>%
                               purrr::map_df(.f=function(xx,df){
                                 miss.fun(df = dt.H0, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, do = do.val,
                                          M2 = ss$M2,
                                          ci.method = wald.ci,
                                          mice.anal = FALSE)
                               },df = y, .id = 'missing')%>%
                               dplyr::mutate(scenario.id = ss$scenario.id,
                                             p_C = ss$p_C,
                                             p_T = ss$p_C,
                                             M2 = ss$M2,
                                             type = 't.H0',
                                             do = do.val,
                                             sim.id = x)
                             
                           })
  
  
  bind_rows(x1)%>%
    mutate(diff.do = do.C - do.T)%>%
    group_by(missing)%>%
    summarise(mean(diff.do))
  
  
  