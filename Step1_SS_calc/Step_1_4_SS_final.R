source("Step_0_init.R")

SS0 <- readRDS("SS_Wald_FM_WN_GLM_power90.rds")

SS.d <- SS0%>%
  dplyr::mutate(SS.min.max.p = round(abs(pmax(N.total.Wald, N.total.FM, N.total.WN)/
                  pmin(N.total.Wald, N.total.FM, N.total.WN)-1),2),
                SS.min.max.d = abs(pmax(N.total.Wald, N.total.FM, N.total.WN)-
                                     pmin(N.total.Wald, N.total.FM, N.total.WN)))
SS.max <- SS0%>%
  dplyr::mutate(N.total = pmax(N.total.Wald, N.total.FM, N.total.WN))%>%
  dplyr::select(scenario.id, p_C, p_T, alpha, power, M2, N.total)

SS.min <- SS0%>%
  dplyr::mutate(N.total = pmin(N.total.Wald, N.total.FM, N.total.WN))%>%
  dplyr::select(scenario.id, p_C, p_T, alpha, power, M2, N.total)

    
saveRDS(SS.max, 'SS_power90nmax.rds')    
saveRDS(SS.min, 'SS_power90nmin.rds')   


###############################
## Save each SS separetely ####
###############################

SS.Wald <- SS0%>%
  dplyr::mutate(N.total = N.total.Wald)%>%
  dplyr::select(scenario.id, p_C, p_T, alpha, power, M2, N.total)


SS.FM <- SS0%>%
  dplyr::mutate(N.total = N.total.FM)%>%
  dplyr::select(scenario.id, p_C, p_T, alpha, power, M2, N.total)

SS.WN <- SS0%>%
  dplyr::mutate(N.total = N.total.WN)%>%
  dplyr::select(scenario.id, p_C, p_T, alpha, power, M2, N.total)

saveRDS(SS.Wald,'SS_power90Wald.rds')
saveRDS(SS.FM,'SS_power90FM.rds')
saveRDS(SS.WN,'SS_power90WN.rds')
