source("Step_0_init.R")
source("funs/ni.d.R")
source("funs/add.X.R")
source("funs/wald.ci.R")
source("funs/miss.fun.R")
source("funs/mice.run.R")
source("funs/mi.comb.R")

source("funs/miss.param.assign.R")

ss.bounds <- readRDS("cluster/ss.bounds.rds")

ss1 <- ss.bounds%>%slice(1) 
ss2 <- ss.bounds%>%slice(30) 

ss<-ss2

do.val = 0.10

#generate the full data along with covaraite X

system.time({
  
  #generate full data with desired correlation structure
  
  set.seed(8273+i)
  dt.H0 <- ni.d(N_T = ss$n.arm, 
                N_C = ss$n.arm, 
                p_T = ss$p_T - ss$M2,
                p_C = ss$p_T)%>%
    add.X(rho=0.3, ss$ub)
  
  #define missingness parameters and do rates
  m.param <- tibble::tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mnar1", "mnar2"))
  m.param <- m.param%>%miss.param.assign()
  
  anal.out <- m.param%>%
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
                  do = do.val)
  
  
})





