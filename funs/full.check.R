full.check <- function(dtname, sc){
  dtname%>%
    purrr::map_df(.f=function(x) x$ci.full,.id = 'sim')%>%
    summarise_at(.vars=c('C_phat', 'T_phat', 'reject.h0'), .funs = mean)%>%
    dplyr::mutate(scenario.id=sc)%>%
    left_join(ss%>%
                dplyr::select(scenario.id, p_C, M2), by = "scenario.id")
  
}
