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
                   readLines('cluster/pgms/tmpls/fullH0_hpc.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val)
                 ),
                 file = file.path('cluster/pgms/wald',
                                  sprintf("fullH0_%d_do%d.R", scenario, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


sc.n1 <- 
  expand.grid(
    scenario = 21,
    do.val = c(0.1, 0.2),
    setn = 1,
    rho.c = c("p30","m30"),
    stringsAsFactors = FALSE)%>%
  mutate(rho = case_when(rho.c == "p30" ~ 0.3,
                             rho.c == "m30" ~ -0.3))

purrr::pwalk(.l = as.list(sc.n1%>%filter(rho.c =="m30")),
             .f = function(scenario, do.val, setn, rho, rho.c){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH0_hpc_setn1.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn, 
                               rval = rho,
                               rc = rho.c)
                 ),
                 file = file.path('cluster/pgms/wald/setn/m30',
                                  sprintf("fullH0_%d_do%d_set%d_m30.R", scenario, round(100*do.val,0), setn)
                 ),
                 sep='\n') 
             })

purrr::pwalk(.l = as.list(sc.n1),
             .f = function(scenario, do.val, setn){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH1_hpc_setn1.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn)
                 ),
                 file = file.path('cluster/pgms/wald/setn',
                                  sprintf("fullH1_%d_do%d_set%d.R", scenario, round(100*do.val,0), setn)
                 ),
                 sep='\n') 
             })

sc.n2 <- 
  expand.grid(
    scenario = 21,
    do.val = c(0.1, 0.2),
    setn = 2,
    stringsAsFactors = FALSE)

purrr::pwalk(.l = as.list(sc.n2),
             .f = function(scenario, do.val, setn){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH0_hpc_setn2.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn)
                 ),
                 file = file.path('cluster/pgms/wald/setn',
                                  sprintf("fullH0_%d_do%d_set%d.R", scenario, round(100*do.val,0), setn)
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


##############
### m=5
##############
sc.n1 <- 
  expand.grid(
    scenario = seq(1,30,1),
    do.val = c(0.1, 0.2),
    setn = 1,
    rho.c = c("p30","m30"),
    stringsAsFactors = FALSE)%>%
  mutate(rho = case_when(rho.c == "p30" ~ 0.3,
                         rho.c == "m30" ~ -0.3))%>%
  mutate(bound = case_when(rho.c == "p30" ~ 'u',
                           rho.c == "m30" ~ 'l'))


purrr::pwalk(.l = as.list(sc.n1%>%filter(rho.c =="p30")),
             .f = function(scenario, do.val, setn, rho, rho.c, bound){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH0_hpc_m5_setn1.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn, 
                               rval = rho,
                               rc = rho.c,
                               ulb = bound)
                 ),
                 file = file.path('cluster/pgms/wald/setn/mi5/p30',
                                  sprintf("fullH0_%d_do%d_set%d_p30_mi5.R", scenario, round(100*do.val,0), setn)
                 ),
                 sep='\n') 
             })


purrr::pwalk(.l = as.list(sc.n1%>%filter(rho.c =="m30")),
             .f = function(scenario, do.val, setn, rho, rho.c, bound){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH0_hpc_m5_setn1.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn, 
                               rval = rho,
                               rc = rho.c,
                               ulb = bound)
                 ),
                 file = file.path('cluster/pgms/wald/setn/mi5/m30',
                                  sprintf("fullH0_%d_do%d_set%d_m30_mi5.R", scenario, round(100*do.val,0), setn)
                 ),
                 sep='\n') 
             })


purrr::pwalk(.l = as.list(sc.n1%>%filter(rho.c =="p30")),
             .f = function(scenario, do.val, setn, rho, rho.c, bound){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH1_hpc_m5_setn1.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn, 
                               rval = rho,
                               rc = rho.c,
                               ulb = bound)
                 ),
                 file = file.path('cluster/pgms/wald/setn/mi5/p30',
                                  sprintf("fullH1_%d_do%d_set%d_p30_mi5.R", scenario, round(100*do.val,0), setn)
                 ),
                 sep='\n') 
             })


purrr::pwalk(.l = as.list(sc.n1%>%filter(rho.c =="m30")),
             .f = function(scenario, do.val, setn, rho, rho.c, bound){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/fullH1_hpc_m5_setn1.tmpl'),
                   data = list(scenario = scenario,
                               val = do.val,
                               setn = setn, 
                               rval = rho,
                               rc = rho.c,
                               ulb = bound)
                 ),
                 file = file.path('cluster/pgms/wald/setn/mi5/m30',
                                  sprintf("fullH1_%d_do%d_set%d_m30_mi5.R", scenario, round(100*do.val,0), setn)
                 ),
                 sep='\n') 
             })


