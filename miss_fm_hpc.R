library(future)

source('Step_0_init.R')
source("funs/p.rmle.fm.R")
source("funs/miss.fun.R")
source("funs/miss.param.assign.R")
source("funs/FM.CI.R")
source("funs/mice.run.R")
source("funs/mi.comb.R")

dt <- readRDS("dtfullfm_4.rds") 

dt0 <- dt%>%
  dplyr::select(scenario.id, p_C, p_T, M2, t.H0, t.H1)%>%
  tidyr::gather("type", "value", c(t.H0, t.H1))%>%
  tidyr::unnest(value)

x <- dt0%>%dplyr::group_split(type,sim.id)

#define missingness parameters
m.param <- tibble::tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mnar1", "mnar2"))
m.param <- m.param%>%miss.param.assign()

#cores <- future::availableCores()

# cores <- parallel::detectCores()
# print(cores)

#process <- future::tweak(future::multiprocess,workers = cores)

#future::plan(process)

library(parallel)
cl <- makeCluster(Sys.getenv()["SLURM_NTASKS"], type = "MPI")

#set.seed(34231)
#seedmice = 9875

parallel::clusterExport(cl, varlist = ls())
system.time({x1 <- parallel::clusterApply(cl, x,fun=function(y){
  
  library(tidyr, warn.conflicts = F, quietly = T)
  library(dplyr, warn.conflicts = F, quietly = T)
  library(purrr, warn.conflicts = F, quietly = T)
  library(reshape2, warn.conflicts = F, quietly = T)
  library(mice, warn.conflicts = F, quietly = T)
  
  set.seed(34231)
  
  m.param%>%
    group_split(missing)%>%
    purrr::set_names(sort(m.param$missing))%>%
    purrr::map_df(.f=function(xx,df){
      miss.fun(df = df, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, method = "psort",
               dt.out = FALSE, ci.method = FM.CI)
    },df = y, .id = 'missing')
  
  })
})

stopCluster(cl)

x2 <- bind_rows(x1)

x3 <- x2%>%
  dplyr::group_by(strategy, type, missing)%>%
  dplyr::summarise(err = mean(reject.h0))

saveRDS(x3,"results/cca_fm_4_do10.rds")

