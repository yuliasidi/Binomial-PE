library(dplyr)
library(purrr)

sc <- 
  expand.grid(
    scenario = seq(17,17,1),
    do.val = 0.2,
    param = 1,
    stringsAsFactors = FALSE)



#build program for cca/best/worst analysis
purrr::pwalk(.l = as.list(sc),
             .f = function(scenario, do.val, param){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_fm_sing.tmpl'),
                   data = list(sc_id = scenario,
                               val = do.val,
                               param_id = param)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont',
                                  sprintf("2xcontH0_sc%s_do%s_param%s_sing.R", 
                                          scenario, round(100*do.val,0), param)
                 ),
                 sep='\n') 
             })

#build program for nested mice analysis
purrr::pwalk(.l = as.list(sc),
             .f = function(scenario, do.val, param){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_fm_mice.tmpl'),
                   data = list(sc_id = scenario,
                               val = do.val,
                               param_id = param)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont',
                                  sprintf("2xcontH0_sc%s_do%s_param%s_mice.R", 
                                          scenario, round(100*do.val,0), param)
                 ),
                 sep='\n') 
             })
