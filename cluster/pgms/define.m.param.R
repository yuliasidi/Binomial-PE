source("cluster/pgms/init.R")
source("funs/dt.sim.x2.cont.R")
source("funs/miss.impose.x2.cont.R")
source("funs/wald.ci.R")
source("funs/anal.miss.run.R")
#source("funs/mice.run.R")
source("funs/mice.nonign.run.R")
source("funs/mice.impute.logreg.p.R")
source("funs/nested.mi.comb.R")
source("funs/miss.param.assign.x2.cont.R")

library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(mice, warn.conflicts = F, quietly = T)
#library(norm, warn.conflicts = F, quietly = T)
library(MASS, warn.conflicts = F, quietly = T)


method <- 'wald'
scenario <- 21

ss.bounds <- readRDS("cluster/ss.bounds.rds")
ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

#mnar1: lack of efficacy in T -> phat.T observed > phat.T full, thus the phat difference is smaller 
# than it should be
#mnar2: overwhelming efficacy in C -> phat.T observed < phat.T full, thus the phat difference is smaller 
# than it should be

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7", "mnar1",
                              "mnar2"),
                  bt    = c(0,  0,  0.3,  0.6,  0.9, -0.3, -0.6, -.9,   0,    0),
                  bx2   = c(0,  2,  2,    2,    2,    2,    2,     2,   0,    0),
                  bx1   = c(0,  0,  0,    0,    0,    0,    0,     0,   0,    0),
                  by    = c(0,  0,  0,    0,    0,    0,    0,     0,  -0.4,    2),
                  b.ty  = c(0,  0,  0,    0,    0,    0,    0,     0,  -0.8,   -2)) 



######################################################
#### Check imposed missingness on param=1 for H0 ##### 
######################################################

x1 <- parallel::mclapply(X = 1:2, 
                           mc.cores = 4,
                           FUN= function(x)
                           {

  set.seed(10000*scenario + x)                                                   
                             
  dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                       mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01) 
 
  m.param%>%
     dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                         anal.miss.run, df = dt0, do = do.val,
                                         ci.method = wald.ci,
                                         sing.anal = T,
                                         mice.anal = F,
                                         M2 = ss$M2, seed = 10000*scenario + x,
                                         seed.mice = 10000*scenario + x,
                                         mu.T = 0.9, sd.T = 0.05))%>%
     dplyr::select(missing, results)%>%
     dplyr::mutate(scenario.id = ss$scenario.id,
                   p_C = ss$p_C,
                   M2 = ss$M2,
                   type = 't.H0',
                   do = do.val,
                   sim.id = x)
                             
                             
                           }
  
 )
 

 bind_rows(x1)%>%
   unnest()%>%
   dplyr::filter(seq(1,n(),1)%in%c(seq(1,n(),3)))%>%
   unnest()%>%
   filter(strategy=="cca")%>%
   group_by(missing)%>%
   summarise(type1=mean(reject.h0))
  
  x1.do<-bind_rows(x1)%>%
    unnest()%>%
    dplyr::filter(seq(1,n(),1)%in%c(seq(2,n(),3)))%>%
    unnest()
  
  x1.do%>%
    unnest()%>%
    mutate(diff.do = C - T)%>%
    group_by(missing)%>%
    summarise_at(.vars=c('C','T','diff.do'), .f = mean)
  

  
  
  
#########
# Look at one dataset
x<-10
set.seed(10000*scenario + x)                                                   

dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                    mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01) 

dt.miss <- m.param%>%
    dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                        anal.miss.run, df = dt0, do = do.val,
                                        ci.method = wald.ci,
                                        dt.out = T, M2 = ss$M2, seed = 10000*scenario + x,
                                        seed.mice = 10000*scenario + x))%>%
    dplyr::select(missing, results)%>%
    dplyr::mutate(scenario.id = ss$scenario.id,
                  p_C = ss$p_C,
                  M2 = ss$M2,
                  type = 't.H0',
                  do = do.val,
                  sim.id = x)

dt.mnar1 <- dt.miss%>%
  filter(missing=="mnar1")%>%
  unnest()%>%
  left_join(dt0%>%
              dplyr::select(pat_id, y), by = "pat_id")
dt.mnar1%>%
  group_by(trt)%>%
  summarise(mean(y), mean(y.m, na.rm=T))

mice.nonign.run(dt = dt.mnar1,n.mi = 2, m.mi = 10, M2 = ss$M2, ci.method = wald.ci, 
                seed.mice = 10000*scenario + x,
                mu.T = 0.9, sd.T = 0.05
                )


 
dt.mnar2 <- dt.miss%>%
   filter(missing=="mnar2")%>%
   unnest()%>%
   left_join(dt0%>%
               dplyr::select(pat_id, y), by = "pat_id")
 dt.mnar2%>%
   group_by(trt)%>%
   summarise(mean(y), mean(y.m, na.rm=T))

 
 ####################
 ## Look only at phats
 x1 <- parallel::mclapply(X = 1:1000, 
                          mc.cores = 4,
                          FUN= function(x)
                          {
                            
set.seed(10000*scenario + x)                                                   

dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                      mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01) 

m.param%>%
  dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                      miss.impose.x2.cont, df = dt0, do = do.val,
                                      seed = 10000*scenario + x))%>%
  dplyr::select(missing, results)

                          
  }
                          
 )
 
x2 <- bind_rows(x1)%>%
  unnest()%>%
  dplyr::group_by(missing,trt)%>%
  dplyr::summarise(phat = mean(y.m, na.rm = T))%>%
  spread(key = trt, value = phat)
 

###############################################
#### Check imposed missingness on param=2 ##### 
###############################################

x1 <- parallel::mclapply(X = 1:1000, 
                         mc.cores = 4,
                         FUN= function(x)
                         {
                           
 set.seed(10000*scenario + x)                                                   
 
 dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                       mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20,r12 = 0.3, b1 = -0.1, b2 = -0.01) 
 
 m.param%>%
   dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                       anal.miss.run, df = dt0, do = do.val,
                                       ci.method = wald.ci,
                                       mice.anal = F,
                                       norm.anal = F, M2 = ss$M2, seed = 10000*scenario + x,
                                       t.inc = T,
                                       seed.mice = 10000*scenario + x))%>%
   dplyr::select(missing, results)%>%
   dplyr::mutate(scenario.id = ss$scenario.id,
                 p_C = ss$p_C,
                 M2 = ss$M2,
                 type = 't.H0',
                 do = do.val,
                 sim.id = x)
 
 
                         }
                         
)


bind_rows(x1)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(1,n(),3)))%>%
  unnest()%>%
  filter(strategy=="cca")%>%
  group_by(missing)%>%
  summarise(type1=mean(reject.h0))

x1.do<-bind_rows(x1)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(2,n(),3)))%>%
  unnest()

x1.do%>%
  unnest()%>%
  mutate(diff.do = C - T)%>%
  group_by(missing)%>%
  summarise_at(.vars=c('C','T','diff.do'), .f = mean)

####################
## Look only at phats
x1 <- parallel::mclapply(X = 1:5000, 
                         mc.cores = 4,
                         FUN= function(x)
                         {
                           
 set.seed(10000*scenario + x)                                                   
 
 dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                       mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = 0.3, b1 = -0.1, b2 = -0.01) 
 
 m.param%>%
   dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                       miss.impose.x2.cont, df = dt0, do = do.val,
                                       seed = 10000*scenario + x))%>%
   dplyr::select(missing, results)
 
                         
                         }
                         
)

x2 <- bind_rows(x1)%>%
  unnest()%>%
  dplyr::group_by(missing,trt)%>%
  dplyr::summarise(phat = mean(y.m, na.rm = T))%>%
  spread(key = trt, value = phat)

 


######################################################
#### Check imposed missingness on param=1 for H1 ##### 
######################################################

x2 <- parallel::mclapply(X = 1:1000, 
                         mc.cores = 6,
                         FUN= function(x)
                         {
                           
                           set.seed(10000*scenario + x)                                                   
                           
                           dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C, n.arm = ss$n.arm, 
                                                 mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01) 
                           
                           m.param%>%
                             dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                                                 anal.miss.run, df = dt0, do = do.val,
                                                                 ci.method = wald.ci,
                                                                 sing.anal = T,
                                                                 mice.anal = F,
                                                                 M2 = ss$M2, seed = 10000*scenario + x,
                                                                 seed.mice = 10000*scenario + x,
                                                                 mu.T = 0.9, sd.T = 0.05))%>%
                             dplyr::select(missing, results)%>%
                             dplyr::mutate(scenario.id = ss$scenario.id,
                                           p_C = ss$p_C,
                                           M2 = ss$M2,
                                           type = 't.H1',
                                           do = do.val,
                                           sim.id = x)
                           
                           
                         }
                         
)


bind_rows(x2)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(1,n(),3)))%>%
  unnest()%>%
  filter(strategy=="cca")%>%
  group_by(missing)%>%
  summarise(type1=mean(reject.h0))

x1.do<-bind_rows(x2)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(2,n(),3)))%>%
  unnest()

x1.do%>%
  unnest()%>%
  mutate(diff.do = C - T)%>%
  group_by(missing)%>%
  summarise_at(.vars=c('C','T','diff.do'), .f = mean)


#######################################
############ DO = 15% #################
#######################################

method <- 'wald'
scenario <- 21

ss.bounds <- readRDS("cluster/ss.bounds.rds")
ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.15

#mnar1: lack of efficacy in T -> phat.T observed > phat.T full, thus the phat difference is smaller 
# than it should be
#mnar2: overwhelming efficacy in C -> phat.T observed < phat.T full, thus the phat difference is smaller 
# than it should be

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mnar1", "mnar2"),
                  bt   = c(0, 0, 0.4, 0.8, -0.4, -0.8, 0, 0),
                  bx2  = c(0, 1.5, 1.5, 1.5, 1.5, 1.5, 0, 0),
                  bx1  = c(0, 0, 0, 0, 0, 0, 0, 0),
                  by   = c(0, 0, 0, 0, 0, 0, -0.4,  2),
                  b.ty = c(0, 0, 0, 0, 0, 0, -0.8, -2))

###############################################
#### Check imposed missingness on param=1 ##### 
###############################################

x1 <- parallel::mclapply(X = 1:5000, 
                         mc.cores = 6,
                         FUN= function(x)
                         {
                           
                           set.seed(10000*scenario + x)                                                   
                           
                           dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                                                 mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01) 
                           
                           m.param%>%
                             dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                                                 anal.miss.run, df = dt0, do = do.val,
                                                                 ci.method = wald.ci,
                                                                 sing.anal = T,
                                                                 mice.anal = F,
                                                                 M2 = ss$M2, seed = 10000*scenario + x,
                                                                 seed.mice = 10000*scenario + x,
                                                                 mu.T = 0.9, sd.T = 0.05))%>%
                             dplyr::select(missing, results)%>%
                             dplyr::mutate(scenario.id = ss$scenario.id,
                                           p_C = ss$p_C,
                                           M2 = ss$M2,
                                           type = 't.H0',
                                           do = do.val,
                                           sim.id = x)
                           
                           
                         }
                         
)


bind_rows(x1)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(1,n(),3)))%>%
  unnest()%>%
  filter(strategy=="cca")%>%
  group_by(missing)%>%
  summarise(type1=mean(reject.h0))

x1.do<-bind_rows(x1)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(2,n(),3)))%>%
  unnest()

x1.do%>%
  unnest()%>%
  mutate(diff.do = C - T)%>%
  group_by(missing)%>%
  summarise_at(.vars=c('C','T','diff.do'), .f = mean)

#######################################
############ DO = 10% #################
#######################################

method <- 'wald'
scenario <- 21

ss.bounds <- readRDS("cluster/ss.bounds.rds")
ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.10

#mnar1: lack of efficacy in T -> phat.T observed > phat.T full, thus the phat difference is smaller 
# than it should be
#mnar2: overwhelming efficacy in C -> phat.T observed < phat.T full, thus the phat difference is smaller 
# than it should be

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mnar1", "mnar2"),
                  bt   = c(0, 0, 0.5, -0.5, 0, 0),
                  bx2  = c(0, 1.5, 1.5, 1.5, 0, 0),
                  bx1  = c(0, 0, 0, 0, 0, 0),
                  by   = c(0, 0, 0, 0, -0.4,  2),
                  b.ty = c(0, 0, 0, 0, -0.8, -2))

###############################################
#### Check imposed missingness on param=1 ##### 
###############################################

x1 <- parallel::mclapply(X = 1:1000, 
                         mc.cores = 6,
                         FUN= function(x)
                         {
                           
                           set.seed(10000*scenario + x)                                                   
                           
                           dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                                                 mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01) 
                           
                           m.param%>%
                             dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                                                 anal.miss.run, df = dt0, do = do.val,
                                                                 ci.method = wald.ci,
                                                                 sing.anal = T,
                                                                 mice.anal = F,
                                                                 M2 = ss$M2, seed = 10000*scenario + x,
                                                                 seed.mice = 10000*scenario + x,
                                                                 mu.T = 0.9, sd.T = 0.05))%>%
                             dplyr::select(missing, results)%>%
                             dplyr::mutate(scenario.id = ss$scenario.id,
                                           p_C = ss$p_C,
                                           M2 = ss$M2,
                                           type = 't.H0',
                                           do = do.val,
                                           sim.id = x)
                           
                           
                         }
                         
)


bind_rows(x1)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(1,n(),3)))%>%
  unnest()%>%
  filter(strategy=="cca")%>%
  group_by(missing)%>%
  summarise(type1=mean(reject.h0))

x1.do<-bind_rows(x1)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(2,n(),3)))%>%
  unnest()

x1.do%>%
  unnest()%>%
  mutate(diff.do = C - T)%>%
  group_by(missing)%>%
  summarise_at(.vars=c('C','T','diff.do'), .f = mean)


#######################################
############ DO = 5% #################
#######################################

method <- 'wald'
scenario <- 21

ss.bounds <- readRDS("cluster/ss.bounds.rds")
ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.05

#mnar1: lack of efficacy in T -> phat.T observed > phat.T full, thus the phat difference is smaller 
# than it should be
#mnar2: overwhelming efficacy in C -> phat.T observed < phat.T full, thus the phat difference is smaller 
# than it should be

m.param <- tibble(missing = c("mcar", "mar", "mnar1", "mnar2"),
                  bt   = c(0, 0, 0, 0),
                  bx2  = c(0, 1.5, 0, 0),
                  bx1  = c(0, 0, 0, 0),
                  by   = c(0, 0, -0.4,  2),
                  b.ty = c(0, 0, -0.8, -2))

###############################################
#### Check imposed missingness on param=1 ##### 
###############################################

x1 <- parallel::mclapply(X = 1:1000, 
                         mc.cores = 6,
                         FUN= function(x)
                         {
                           
                           set.seed(10000*scenario + x)                                                   
                           
                           dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                                                 mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01) 
                           
                           m.param%>%
                             dplyr::mutate(results = purrr::pmap(list(b.trt=bt, b.Y=by, b.X1=bx1, b.X2=bx2, b.ty = b.ty), 
                                                                 anal.miss.run, df = dt0, do = do.val,
                                                                 ci.method = wald.ci,
                                                                 sing.anal = T,
                                                                 mice.anal = F,
                                                                 M2 = ss$M2, seed = 10000*scenario + x,
                                                                 seed.mice = 10000*scenario + x,
                                                                 mu.T = 0.9, sd.T = 0.05))%>%
                             dplyr::select(missing, results)%>%
                             dplyr::mutate(scenario.id = ss$scenario.id,
                                           p_C = ss$p_C,
                                           M2 = ss$M2,
                                           type = 't.H0',
                                           do = do.val,
                                           sim.id = x)
                           
                           
                         }
                         
)


bind_rows(x1)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(1,n(),3)))%>%
  unnest()%>%
  filter(strategy=="cca")%>%
  group_by(missing)%>%
  summarise(type1=mean(reject.h0))

x1.do<-bind_rows(x1)%>%
  unnest()%>%
  dplyr::filter(seq(1,n(),1)%in%c(seq(2,n(),3)))%>%
  unnest()

x1.do%>%
  unnest()%>%
  mutate(diff.do = C - T)%>%
  group_by(missing)%>%
  summarise_at(.vars=c('C','T','diff.do'), .f = mean)

