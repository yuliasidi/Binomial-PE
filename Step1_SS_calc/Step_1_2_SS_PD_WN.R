#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])+1
.libPaths('ysidi/lib')

SS0 <- readRDS("SS_Wald_FM_power90.rds")

n.sim.ss <- 5000
set.seed(3425)

#Split the data, so that each line is run as a job

SS.idx <- SS0%>%
  dplyr::slice(idx)


wn <- SS.idx %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    
    N_T = N.total.FM / 2, N_C = N_T,
    
    t.WN = purrr::pmap(list(N_T, N_C, p_T, p_C), .f = function(..., n.sim.ss) {
      
      purrr::rerun(n.sim.ss, ni.d(...)) %>% 
        dplyr::bind_rows(.id = "sim.id")
      
    }, 
    n.sim.ss = n.sim.ss
    )
  )

wn_new <- recourse_wn(wn, n.sim.ss)

SS1 <- wn_new %>%
  dplyr::mutate(N.total.WN = 2 * N_T) %>%
  dplyr::select(-N_T, -power.est, -flag, -N_C, -t.WN, -t.wn.CI, -power.WN)

saveRDS(SS1, file = sprintf('SS_addWN_%02d.rds',idx))
