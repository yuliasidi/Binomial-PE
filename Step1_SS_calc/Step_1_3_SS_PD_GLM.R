#!/home/statsadmin/R/bin/Rscript


source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])+1
.libPaths('ysidi/lib')

SS0 <- readRDS("SS_Wald_FM_WN_power90.rds")

n.sim.ss <- 5000

set.seed(9873)

#Split the data, so that each line is run as a job
SS.idx <- SS0%>%
  dplyr::slice(idx)

glm.dt <- SS.idx%>%
  dplyr::as_tibble()%>%
  dplyr::mutate(N_T = N.total.FM/2,N_C = N_T,
                t.GLM=purrr::pmap(list(N_T,N_C,p_T,p_C), .f = function(..., n.sim.ss){
                  purrr::rerun(n.sim.ss,ni.d(...))%>%bind_rows(.id = "sim.id")
                },n.sim.ss = n.sim.ss))

glm.dt_new <- recourse_glm(glm.dt,n.sim.ss)

SS1 <- glm.dt_new%>%
  dplyr::mutate(N.total.GLM = 2*N_T)%>%
  dplyr::select(-N_T, -power.est, -flag, -N_C, -t.GLM.CI, -power.GLM)

saveRDS(SS1, file = sprintf('SS_addGLM_%02d.rds',idx))
