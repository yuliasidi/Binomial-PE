do.check <- function(dtname){
  
  bind_rows(
    dtname%>%
      purrr::map_df(.f=function(x) x$ci.miss,.id = 'sim')%>%
      unnest()%>%
      dplyr::filter(seq(1,n(),1)%in%c(seq(2,n(),3)))%>%
      unnest())%>%
    group_by(scenario.id, missing)%>%
    summarise(doC=mean(C), doT=mean(T), do.diff=round(mean(C)-mean(T),4))%>%
    miss.desc() 
}
