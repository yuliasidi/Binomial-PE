source("cluster/pgms/init_m5.R")
source("funs/dt.sim.x2.cont.R")
source("funs/miss.impose.x2.cont.R")
source("funs/wald.ci.R")
source("funs/anal.miss.run.R")
source("funs/mice.run.R")
source("funs/mi.comb.R")
source("funs/miss.param.assign.x2.cont.R")
source("funs/norm.run.R")

library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(MASS, warn.conflicts = F, quietly = T)

method <- 'wald'
rho.val <- 'p30'
scenario <- 21

ss.bounds <- readRDS("cluster/ss.bounds.rds")
ss <- ss.bounds%>%
  dplyr::filter(method == "wald", scenario.id == scenario)

x1 <- parallel::mclapply(X = 1:5000, 
                         mc.cores = 4,
                         FUN= function(x)
                         {

set.seed(10000*scenario + x)                                                   

#x1 - disease status at bl -> x1 higher means a better outcome
#x2 - SBP at bl -> x2 higher means a less favorable outcome
#since x1 and x2 affect the outcome in differrent directions -> r12 must be negative
dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                     mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01) 


phat <- dt0%>%
  group_by(trt)%>%
  summarise(phat = mean(y))%>%
  spread(trt, phat)

glm.res <- summary(glm(y ~ x1 + x2 + as.factor(trt), family = binomial, data = dt0))$coefficients[,1]%>%
  tibble()%>%
  dplyr::rename(est =".")%>%
  dplyr::mutate(param.est = seq(1,4,1))

 out.list <- list(phat, glm.res)%>%purrr::set_names("phat", "glm.res") 
 return(out.list)
    })

#check the estimated phats
x1%>%
  purrr::map_df(.f=function(x) x$phat,.id = 'sim')%>%
  unnest()%>%
  summarise(mean(C), mean(T))

# mean(C)   mean(T)
# 0.847     0.747


#check the parameters estimates from logistic model
x1%>%
  purrr::map_df(.f=function(x) x$glm.res,.id = 'sim')%>%
  unnest()%>%
  group_by(param.est)%>%
  summarise(mean(est))

# param.est    mean(est)
# int           2.36   
# x1            0.0998
# x2           -0.0100
# trt(T)       -0.641 

#############################################
# Different set of values for r12, b1 and b2
############################################

x3 <- parallel::mclapply(X = 1:5000, 
                         mc.cores = 4,
                         FUN= function(x)
                         {

set.seed(10000*scenario + x)                                                   

#x1 - disease status at bl -> x1 higher means a worse outcome
#x2 - SBP at bl -> x2 higher means a less favorable outcome
#since x1 and x2 affect the outcome in the same direction -> r12 must be positive
dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                     mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = 0.3, b1 = -0.1, b2 = -0.01) 


phat <- dt0%>%
 group_by(trt)%>%
 summarise(phat = mean(y))%>%
 spread(trt, phat)

glm.res <- summary(glm(y ~ x1 + x2 + as.factor(trt), family = binomial, data = dt0))$coefficients[,1]%>%
 tibble()%>%
 dplyr::rename(est =".")%>%
 dplyr::mutate(param.est = seq(1,4,1))

out.list <- list(phat, glm.res)%>%purrr::set_names("phat", "glm.res") 
return(out.list)
})

#check the estimated phats
x3%>%
  purrr::map_df(.f=function(x) x$phat,.id = 'sim')%>%
  unnest()%>%
  summarise(mean(C), mean(T))

# mean(C)   mean(T)
# 0.847     0.747


#check the parameters estimates from logistic model
x3%>%
  purrr::map_df(.f=function(x) x$glm.res,.id = 'sim')%>%
  unnest()%>%
  group_by(param.est)%>%
  summarise(mean(est))

# param.est    mean(est)
# int           3.16   
# x1            0.102
# x2           -0.00999
# trt(T)       -0.640 
