#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')


pat <- expand.grid(
  MISSING=c('mcar','mar1','mar2','mar3','mar4','mar5','mar6','mnar1','mnar2'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)
pat <- pat%>%
  dplyr::mutate(pattype = sprintf("%s%s",MISSING, PERCENT),
                i = seq(0,44,1))

pat_job <- pat%>%
  dplyr::filter(i == idx)

t <- purrr::map_df(
  list.files(pattern = pat_job$pattype,  full.names = TRUE),
  readRDS)

mice.sum <-bind_cols(
  t%>%
    tidyr::unnest()%>%
    dplyr::select(-t.H1.mice)%>%
    tidyr::unnest()%>%
    mi.res.sum()%>%
    dplyr::group_by(scenario.id)%>%
    dplyr::summarise(type1 = mean(reject.h0)),
  t%>%
    tidyr::unnest()%>%
    dplyr::select(-t.H0.mice)%>%
    tidyr::unnest()%>%
    mi.res.sum()%>%
    dplyr::group_by(scenario.id)%>%
    dplyr::summarise(power = mean(reject.h0)),
  t%>%
    tidyr::unnest()%>%
    dplyr::select(-t.H1.mice)%>%
    tidyr::unnest()%>%
    mi.res.sum()%>%
    bias.mi.fun(),
  .id = "scenario.id")

