source("Step_0_init.R")

SS <- readRDS("SS_Wald_FM_WN_GLM_power90.rds")

SS.d <- SS%>%
  dplyr::mutate(SS.min.max.p = round(abs(pmax(N.total.Wald, N.total.FM, N.total.WN, N.total.GLM)/
                  pmin(N.total.Wald, N.total.FM, N.total.WN, N.total.GLM)-1),2),
                SS.min.max.d = abs(pmax(N.total.Wald, N.total.FM, N.total.WN, N.total.GLM)-
                                     pmin(N.total.Wald, N.total.FM, N.total.WN, N.total.GLM)))%>%
  dplyr::mutate(N.total = pmax(N.total.Wald, N.total.FM, N.total.WN, N.total.GLM))%>%
  dplyr::select(scenario.id, p_C, p_T, alpha, power, M2, N.total)
    
saveRDS(SS.d, 'SS_power90.rds')    
    