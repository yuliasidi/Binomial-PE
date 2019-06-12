source("cluster/pgms/init.R")
source("funs/dt.sim.x2.cont.R")
source("funs/miss.impose.x2.cont.R")
source("funs/wn.ci.R")
source("funs/wald.ci.R")
source("funs/FM.CI.R")
source("funs/p.rmle.fm.R")


library(tidyr, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)
library(reshape2, warn.conflicts = F, quietly = T)
library(MASS, warn.conflicts = F, quietly = T)

ss.bounds <- readRDS("cluster/ss.bounds.rds")

method <<- 'wn'
scenario <- 29
param <- 1
anal.type <- "sing"

ss <- ss.bounds%>%
  dplyr::filter(method == "wn", scenario.id == scenario)


system.time({
  x1 <- parallel::mclapply(X = 1:10000, 
                           mc.cores = 7,
                           FUN= function(x) 
                             
 {
   
   
   
   set.seed(100000*scenario + x)                                                   
   #generate full data with desired correlation structure
   dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                         mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01)
   ci.wn <- dt0%>%wn.ci(ss$M2,'y')
   ci.wald <- dt0%>%wald.ci(ss$M2,'y')
   ci.fm <- dt0%>%FM.CI(ss$M2,'y')
   
   
   ci.all <- list(ci.wn, ci.wald, ci.fm)%>%purrr::set_names(c("ci.wn","ci.wald", "ci.fm")) 
   
   return(ci.all)
   
 })
})

wald.10k <- x1%>%
  purrr::map_df(.f=function(x) x$ci.wald,.id = 'sim')%>%
  summarise_at(.vars=c('C_phat', 'T_phat', 'reject.h0'), .funs = mean)

wn.10k <- x1%>%
  purrr::map_df(.f=function(x) x$ci.wn,.id = 'sim')%>%
  summarise_at(.vars=c('C_phat', 'T_phat', 'reject.h0'), .funs = mean)

fm.10k <- x1%>%
  purrr::map_df(.f=function(x) x$ci.fm,.id = 'sim')%>%
  summarise_at(.vars=c('C_phat', 'T_phat', 'reject.h0'), .funs = mean)


system.time({
  x1 <- parallel::mclapply(X = 1:100000, 
                           mc.cores = 7,
                           FUN= function(x) 
                             
                           {
                             
                             
                             
                             set.seed(100000*scenario + x)                                                   
                             #generate full data with desired correlation structure
                             dt0 <- dt.sim.x2.cont(p_C = ss$p_C, p_T = ss$p_C - ss$M2, n.arm = ss$n.arm, 
                                                   mu1 = 4, mu2 = 100, sigma1 = 1, sigma2 = 20, r12 = -0.3, b1 = 0.1, b2 = -0.01)
                             ci.wn <- dt0%>%wn.ci(ss$M2,'y')
                             ci.wald <- dt0%>%wald.ci(ss$M2,'y')
                             ci.fm <- dt0%>%FM.CI(ss$M2,'y')
                             
                             
                             ci.all <- list(ci.wn, ci.wald, ci.fm)%>%purrr::set_names(c("ci.wn","ci.wald", "ci.fm")) 
                             
                             return(ci.all)
                             
                           })
})

wald.100k <- x1%>%
  purrr::map_df(.f=function(x) x$ci.wald,.id = 'sim')%>%
  summarise_at(.vars=c('C_phat', 'T_phat', 'reject.h0'), .funs = mean)

wn.100k <- x1%>%
  purrr::map_df(.f=function(x) x$ci.wn,.id = 'sim')%>%
  summarise_at(.vars=c('C_phat', 'T_phat', 'reject.h0'), .funs = mean)

fm.100k <- x1%>%
  purrr::map_df(.f=function(x) x$ci.fm,.id = 'sim')%>%
  summarise_at(.vars=c('C_phat', 'T_phat', 'reject.h0'), .funs = mean)

check.num.sim <-
  bind_rows(wald.10k%>%
              mutate(method = "wald"), 
            wn.10k%>%
              mutate(method = 'wn'), fm.10k%>%
              mutate(method = 'fm'))%>%
  mutate(num.sim = '10k')%>%
  bind_rows(
    bind_rows(wald.100k%>%
                mutate(method = "wald"), 
              wn.100k%>%
                mutate(method = "wn"), 
              fm.100k%>%
                mutate(method = "fm"))%>%
                                        mutate(num.sim = '100k'))
saveRDS(check.num.sim, "cluster/checks/check_num_sim.rds")

xtable::xtable(check.num.sim)
