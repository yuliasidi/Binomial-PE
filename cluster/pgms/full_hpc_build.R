library(dplyr)
library(purrr)


#####################################
## Wald - SLURM, H0, do=15%, sing ##
####################################

ll <- c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30)

do.val <- 0.15

purrr::walk(ll,
             .f = function(scenario.id){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_wald_sing.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val)
                 ),
                 file = file.path('cluster/pgms/wald/2xcont/do15',
                                  sprintf("2xcontH0_sc%s_do%s_param1_sing.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


#####################################
## Wald - SLURM, H0, do=15%, MICE ##
####################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  #dplyr::slice(1)
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))


do.val <-0.15

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_wald_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wald/2xcont/do15',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


#####################################
## Wald - SLURM, H0, do=10%, MICE ##
####################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  #dplyr::slice(1)
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))


do.val <-0.10

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_wald_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wald/2xcont/do10',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

#####################################
## Wald - SLURM, H0, do=5%, MICE ##
####################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  #dplyr::slice(1)
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))


do.val <-0.05

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_wald_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wald/2xcont/do5',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


##################################
## WN - SLURM, H0, do=15%, MICE ##
##################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  #dplyr::slice(1)
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))


do.val <-0.15

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_wn_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wn/2xcont/do15',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

##################################
## WN - SLURM, H0, do=10%, MICE ##
##################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  #dplyr::slice(1)
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))


do.val <-0.10

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_wn_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wn/2xcont/do10',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

##################################
## WN - SLURM, H0, do=5%, MICE ##
##################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  #dplyr::slice(1)
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))


do.val <-0.05

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_wn_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wn/2xcont/do5',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


#####################################
## WN - SLURM, H1, do=20%, sing ##
####################################

ll <- c(seq(9,16,1),18,19,20,22,23,24,27,28,29,30)

do.val <- 0.20

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_wn_sing.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wn/2xcont',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

##################################
## FM - SLURM, H0, do=20%, MICE ##
##################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))

do.val <-0.2

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_mice_fm_slurm.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

##################################
## FM - SLURM, H0, do=15%, MICE ##
##################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))

do.val <-0.15

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_mice_fm_slurm.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont/do15',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

##################################
## FM - SLURM, H0, do=10%, MICE ##
##################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30))

do.val <-0.10

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_mice_fm_slurm.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont/do10',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

##################################
## FM - SLURM, H0, do=5%, MICE ##
##################################

k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  dplyr::filter(scenario.id%in%c(1,3,5,seq(7,16,1),18,20,22))

do.val <-0.05

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH0_mice_fm_slurm.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont/do5',
                                  sprintf("2xcontH0_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


#####################################
## WN - Local, H0, do=15%,  sing ##
####################################

ll <- c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30)

do.val <- 0.15

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH0_wn_sing.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wn/2xcont/do15',
                                 sprintf("2xcontH0_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

#####################################
## WN - Local, H0, do=10%,  sing ##
####################################

ll <- c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30)

do.val <- 0.10

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH0_wn_sing.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wn/2xcont/do10',
                                 sprintf("2xcontH0_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

#####################################
## WN - Local, H0, do=5%,  sing   ##
####################################

ll <- c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30)

do.val <- 0.05

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH0_wn_sing.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wn/2xcont/do5',
                                 sprintf("2xcontH0_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })


#####################################
## WN - SLURM, H1, do=20%,  mice ##
####################################

ll <- seq(1,30,1)

do.val <- 0.20

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_wn_mice.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wn/2xcont',
                                 sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

#####################################
## Wald - Local, H0, do=10%, sing ##
####################################

ll <- c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30)

do.val <- 0.10

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH0_wald_sing_local.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wald/2xcont/do10',
                                 sprintf("2xcontH0_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

#####################################
## Wald - Local, H0, do=10%, sing ##
####################################

ll <- c(1,3,5,seq(7,16,1),18,20,22,24,27,28,29,30)

do.val <- 0.05

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH0_wald_sing_local.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wald/2xcont/do5',
                                 sprintf("2xcontH0_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

#####################################
## FM - SLURM, H0, do=5%,  sing ##
####################################

ll <- c(1,27,28,29,30)

do.val <- 0.05

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH0_fm_sing_slurm.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/fm/2xcont/do5',
                                 sprintf("2xcontH0_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

##########################################
## FM - SLURM- no MPI, H0, do=5%,  sing ##
##########################################

ll <- c(3,5,seq(7,16,1),18,20,22,24)

do.val <- 0.05

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH0_fm_sing_slurm_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/fm/2xcont/do5',
                                 sprintf("2xcontH0_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })



############################################
## Wald - SLURM- no MPI, H1, do=15%, sing ##
############################################

ll <- seq(1,30,1)

do.val <- 0.15

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_wald_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wald/2xcont/do15',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })


############################################
## Wald - SLURM- no MPI, H1, do=10%, sing ##
############################################

ll <- seq(1,30,1)

do.val <- 0.10

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_wald_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wald/2xcont/do10',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

############################################
## Wald - SLURM- no MPI, H1, do=5%, sing ##
############################################

ll <- seq(1,30,1)

do.val <- 0.05

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_wald_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wald/2xcont/do5',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

############################################
## FM - SLURM- no MPI, H1, do=15%, sing  ##
############################################

ll <- seq(1,30,1)

do.val <- 0.15

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_fm_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/fm/2xcont/do15',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

############################################
## FM - SLURM- no MPI, H1, do=10%, sing  ##
############################################

ll <- seq(1,30,1)

do.val <- 0.10

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_fm_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/fm/2xcont/do10',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

############################################
## FM - SLURM- no MPI, H1, do=5%, sing  ##
############################################

ll <- seq(1,30,1)

do.val <- 0.05

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_fm_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/fm/2xcont/do5',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })


############################################
## WN - SLURM- no MPI, H1, do=15%, sing  ##
############################################

ll <- seq(1,30,1)

do.val <- 0.15

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_wn_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wn/2xcont/do15',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

############################################
## WN - SLURM- no MPI, H1, do=10%, sing  ##
############################################

ll <- seq(1,30,1)

do.val <- 0.10

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_wn_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wn/2xcont/do10',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

############################################
## WN - SLURM- no MPI, H1, do=5%, sing   ##
############################################

ll <- seq(1,30,1)

do.val <- 0.05

purrr::walk(ll,
            .f = function(scenario.id){
              cat(
                whisker::whisker.render(
                  readLines('cluster/pgms/tmpls/2xcontH1_wn_sing_nompi.tmpl'),
                  data = list(sc_id = scenario.id,
                              val = do.val)
                ),
                file = file.path('cluster/pgms/wn/2xcont/do5',
                                 sprintf("2xcontH1_sc%s_do%s_param1_sing.R", 
                                         scenario.id, round(100*do.val,0))
                ),
                sep='\n') 
            })

############################################
## FM - SLURM-  H1, do=20%, mice          ##
############################################

ll <- seq(1,30,1)

do.val <- 0.2

k.assign <- readRDS("cluster/k.assign.rds")

do.val <-0.2

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_fm_mice_slurm.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


############################################
## FM - SLURM-  H1, do=15%, mice          ##
############################################

ll <- seq(1,30,1)

do.val <- 0.15

k.assign <- readRDS("cluster/k.assign.rds")


purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_fm_mice_slurm.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont/do15',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

############################################
## FM - SLURM-  H1, do=10%, mice          ##
############################################

ll <- seq(1,30,1)

do.val <- 0.10

k.assign <- readRDS("cluster/k.assign.rds")


purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_fm_mice_slurm.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont/do10',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

############################################
## FM - SLURM-  H1, do=5%, mice           ##
############################################

ll <- seq(1,23,1)

do.val <- 0.05

k.assign <- readRDS("cluster/k.assign.rds")
k.assign <- k.assign%>%
  filter(scenario.id%in%seq(1,23,1))

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_fm_mice_slurm.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/fm/2xcont/do5',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

#####################################
## WN - SLURM, H1, do=15%, mice   ##
####################################

k.assign <- readRDS("cluster/k.assign.rds")

do.val <- 0.15

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_wn_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wn/2xcont/do15',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

#####################################
## WN - SLURM, H1, do=10%, mice   ##
####################################
k.assign <- readRDS("cluster/k.assign.rds")

ll <- seq(1,30,1)

do.val <- 0.10

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_wn_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wn/2xcont/do10',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

#####################################
## WN - SLURM, H1, do=5%, mice    ##
####################################
k.assign <- readRDS("cluster/k.assign.rds")

k.assign <- k.assign%>%
  filter(scenario.id%in%seq(1,23,1))

ll <- seq(1,23,1)

do.val <- 0.05

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_wn_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wn/2xcont/do5',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


####################################
## Wald - SLURM, H1, do=15%, mice ##
####################################

k.assign <- readRDS("cluster/k.assign.rds")

do.val <- 0.15

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_wald_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wald/2xcont/do15',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })


####################################
## Wald - SLURM, H1, do=10%, mice ##
####################################

k.assign <- readRDS("cluster/k.assign.rds")

do.val <- 0.10

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_wald_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wald/2xcont/do10',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })

####################################
## Wald - SLURM, H1, do=5%, mice  ##
####################################

ll <- seq(1,23,1)

do.val <- 0.05

k.assign <- readRDS("cluster/k.assign.rds")
k.assign <- k.assign%>%
  filter(scenario.id%in%seq(1,23,1))

purrr::pwalk(.l = as.list(k.assign),
             .f = function(scenario.id, mu.k1, mu.k2){
               cat(
                 whisker::whisker.render(
                   readLines('cluster/pgms/tmpls/2xcontH1_wald_mice.tmpl'),
                   data = list(sc_id = scenario.id,
                               val = do.val,
                               k1 = mu.k1,
                               k2 = mu.k2)
                 ),
                 file = file.path('cluster/pgms/wald/2xcont/do5',
                                  sprintf("2xcontH1_sc%s_do%s_param1_mice.R", 
                                          scenario.id, round(100*do.val,0))
                 ),
                 sep='\n') 
             })
