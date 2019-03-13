library(dplyr)
library(purrr)

sc <- 
  expand.grid(
    scenario = seq(1,30,2),
    do.val = seq(0.1,0.20,0.1),
    stringsAsFactors = FALSE)

sc.l <- as.list(sc)


purrr::pwalk(.l = sc.l,
             .f = function(scenario, do.val){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/fullH0_hpc.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val)
                 ),
                 file = file.path('cluster/pgms/wald',
                                  sprintf("fullH0_%d_do%d.R", scenario, round(100*do.val,0))
                 ),
                 sep='\n') 
             })



purrr::pwalk(.l = sc.l,
             .f = function(scenario, do.val)
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH1_hpc.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val)
                 ),
                 file = file.path('cluster/pgms/wald',
                                  sprintf("fullH1_%d_do%d.R", scenario, round(100*do.val,0))
                 ),
                 sep='\n')
)




sc.nomiss <- 
  expand.grid(
    scenario = seq(1,30,2),
    stringsAsFactors = FALSE)

sc.nomiss.l <- as.list(sc.nomiss)


purrr::pwalk(.l = sc.nomiss.l,
             .f = function(scenario){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/fullH0_nomiss_hpc.tmpl'),
                   data = list(scenario = scenario)
                 ),
                 file = file.path('cluster/pgms/wald',
                                  sprintf("fullH0_%d.R", scenario)
                 ),
                 sep='\n') 
             })


purrr::pwalk(.l = sc.nomiss.l,
             .f = function(scenario){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/fullH1_nomiss_hpc.tmpl'),
                   data = list(scenario = scenario)
                 ),
                 file = file.path('cluster/pgms/wald',
                                  sprintf("fullH1_%d.R", scenario)
                 ),
                 sep='\n') 
             })
