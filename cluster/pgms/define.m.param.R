
source("cluster/pgms/init.R")
source("funs/ni.d.R")
source("funs/add.X.R")
source("funs/wald.ci.R")
source("funs/miss.fun.R")
source("funs/mice.run.R")
source("funs/mi.comb.R")
source("funs/miss.param.assign.R")

library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(mice, warn.conflicts = F, quietly = T)

method <- 'wald'
rho.val <- 'p30'
scenario <- 21

ss.bounds <- readRDS("cluster/ss.bounds.rds")
ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                  b.trt = c(0, 0, 0.025, 0.08, 0.15),
                  b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                  b.Y = c(0, 0, 0, 0, 0))


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
  filter(strategy =='cca')%>%
  mutate(diff.do = do.C - do.T)%>%
  group_by(missing)%>%
  summarise(mean(diff.do))

m.param.21 <- m.param%>%mutate(scenario.id = 21, do = 0.2)

###############################################
scenario <- 23

ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                  b.trt = c(0, 0, 0.025, 0.08, 0.15),
                  b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                  b.Y = c(0, 0, 0, 0, 0))


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

m.param.23 <- m.param%>%mutate(scenario.id = 23, do = 0.2)

###############################################
scenario <- 25

ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                  b.trt = c(0, 0, 0.025, 0.08, 0.15),
                  b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                  b.Y = c(0, 0, 0, 0, 0))


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

#m.param.25 <- m.param%>%mutate(scenario.id = 25, do = 0.2)

###############################################
scenario <- 27

ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                  b.trt = c(0, 0, 0.025, 0.08, 0.15),
                  b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                  b.Y = c(0, 0, 0, 0, 0))


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

###############################################
scenario <- 29

ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                  b.trt = c(0, 0, 0.025, 0.08, 0.15),
                  b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                  b.Y = c(0, 0, 0, 0, 0))


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

###############################################
scenario <- 1

ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

do.val <- 0.2

m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                  b.trt = c(0, 0, 0.025, 0.08, 0.15),
                  b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                  b.Y = c(0, 0, 0, 0, 0))


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

###############################################
  scenario <- 3
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0))
  
  
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
  

  ###############################################
  scenario <- 5
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0))
  
  
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

  ###############################################
  scenario <- 7
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0))
  
  
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

###############################################
  scenario <- 9
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0))
  
  
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

  ###############################################
  scenario <- 11
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0))
  
  
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

  ###############################################
  scenario <- 13
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0))
  
  
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

  ###############################################
  scenario <- 15
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0))
  
  
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

  ###############################################
  scenario <- 17
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0))
  
  
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

  ###############################################
  scenario <- 19
  
  ss <- ss.bounds%>%
    dplyr::filter(method == "wald", scenario.id == scenario)
  
  do.val <- 0.2
  
  m.param <- tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mar7"),
                    b.trt = c(0, 0, 0.025, 0.08, 0.15, -0.025, -0.08, -0.15),
                    b.X = c(0, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
                    b.Y = c(0, 0, 0, 0, 0, 0, 0, 0)) 
  
  
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
  
  
  