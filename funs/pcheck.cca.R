pcheck.cca <- function(df, miss.type = "mar"){
  df%>%
    dplyr::filter(strategy=="cca")%>%
    dplyr::right_join(ss%>%
                        dplyr::select(scenario.id, p_C, M2), by = "scenario.id")%>%
    dplyr::mutate(p_C.check = round(mean_pc-p_C,2),
                  p_T.check = round(mean_pt - (p_C - M2),2))%>%
    dplyr::filter(grepl(miss.type, missing)>0)
  
}