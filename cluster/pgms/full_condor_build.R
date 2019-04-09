library(dplyr)
library(purrr)

sc <- 
  expand.grid(
    scenario = seq(1,30,2),
    do.val = seq(0.1,0.20,0.1),
    setn = c(1),
    stringsAsFactors = FALSE)

sc.l <- as.list(sc)


purrr::pwalk(.l = sc.l,
             .f = function(scenario, do.val, setn){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH0_condor.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn)
                 ),
                 file = file.path('cluster/pgms/fm',
                                  sprintf("fullH0set%d_%d_do%d.R", setn, scenario, round(100*do.val,0))
                 ),
                 sep='\n') 
             })



purrr::pwalk(.l = sc.l,
             .f = function(scenario, do.val, setn){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH1_condor.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn)
                 ),
                 file = file.path('cluster/pgms/fm',
                                  sprintf("fullH1set%d_%d_do%d.R", setn, scenario, round(100*do.val,0))
                 ),
                 sep='\n') 
             })
