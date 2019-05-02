source("Step_0_init.R")
source("True_CP/CP_functions.R")
source ("True_CP/funs/cp_el_2prop.fm.R")
library(future)
library(furrr)


bineval.025 <- data.frame(expand.grid(n1 = seq(90,2100,10), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.025)

bineval.10 <- data.frame(expand.grid(n1 = seq(90,2100,10), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.1)

bineval.20 <- data.frame(expand.grid(n1 = seq(90,2100,10), p1 = seq(0.65,0.95,0.1)))%>%
  mutate(n2 = n1,
         p2= p1 - 0.2)



###################################
## Expected Length for Wald CIs ###
###################################

bineval.025.el.wald <- bind_cols(bineval.025,el = pmap_dbl(bineval.025,el_2prop.wald, alpha=0.025))


myprocess <- future::tweak(future::multiprocess,
                           workers = 6)
future::plan(myprocess)
# 
# system.time({
#   bineval.10.cp.wald  <- 
#     bind_cols(bineval.10,el = purrr::pmap_dbl(bineval.10,el_2prop.wald, alpha=0.025))
#   
# })

system.time({
  bineval.10.el.wald  <- 
    bind_cols(bineval.10,el = furrr::future_pmap_dbl(bineval.10,el_2prop.wald, alpha=0.025,
                                                     .progress = TRUE))
})

bineval.20.el.wald  <- bind_cols(bineval.20,
                                 el = furrr::future_pmap_dbl(bineval.20,el_2prop.wald, alpha=0.025,
                                                              .progress = TRUE))


saveRDS(bineval.025.el.wald, "True_CP/summary/Expected_length/bineval.025.el.wald.rds")
saveRDS(bineval.10.el.wald, "True_CP/summary/Expected_length/bineval.10.el.wald.rds")
saveRDS(bineval.20.el.wald, "True_CP/summary/Expected_length/bineval.20.el.wald.rds")



####################################
## Expected Length/CP for FM CIs ###
####################################

myprocess <- future::tweak(future::multiprocess,
                           workers = 6)
future::plan(myprocess)

bineval.025.fm <- bineval.025%>%
  dplyr::mutate(M2 = 0.025)%>%
  as_tibble()%>%
  mutate(results = furrr::future_pmap(list(n1,p1,n2,p2,M2),
                          cp_el_2prop.fm, alpha=0.025,
                          .progress = T))

bineval.025.fm.all <- bineval.025.fm%>%
  dplyr::mutate(cp = map_dbl(results, .f=function(x) x$cp),
                el = map_dbl(results, .f=function(x) x$el),
                na.sum = map_dbl(results, .f=function(x) x$na.sum))%>%
  dplyr::select(-results)

bineval.10.fm <- bineval.10%>%
  dplyr::mutate(M2 = 0.1)%>%
  as_tibble()%>%
  mutate(results = furrr::future_pmap(list(n1,p1,n2,p2,M2),
                                      cp_el_2prop.fm, alpha=0.025,
                                      .progress = T))

bineval.10.fm.all <- bineval.10.fm%>%
  dplyr::mutate(cp = map_dbl(results, .f=function(x) x$cp),
                el = map_dbl(results, .f=function(x) x$el),
                na.sum = map_dbl(results, .f=function(x) x$na.sum))%>%
  dplyr::select(-results)

bineval.20.fm <- bineval.20%>%
  dplyr::mutate(M2 = 0.2)%>%
  as_tibble()%>%
  mutate(results = furrr::future_pmap(list(n1,p1,n2,p2,M2),
                                      cp_el_2prop.fm, alpha=0.025,
                                      .progress = T))

bineval.20.fm.all <- bineval.20.fm%>%
  dplyr::mutate(cp = map_dbl(results, .f=function(x) x$cp),
                el = map_dbl(results, .f=function(x) x$el),
                na.sum = map_dbl(results, .f=function(x) x$na.sum))%>%
  dplyr::select(-results)



saveRDS(bineval.025.fm.all, "True_CP/summary/bineval.025.fm.all.rds")
saveRDS(bineval.10.fm.all, "True_CP/summary/bineval.10.fm.all.rds")
saveRDS(bineval.20.fm.all, "True_CP/summary/bineval.20.fm.all.rds")

#####################################
## Expected Length for Wilson CIs ###
#####################################

myprocess <- future::tweak(future::multiprocess,
                           workers = 6)
future::plan(myprocess)

bineval.025.el.wilson <- bind_cols(bineval.025,el = furrr::future_pmap_dbl(bineval.025,
                                                                       el_2prop.wilson, alpha=0.025,
                                                                       .progress = T))

bineval.10.el.wilson <- bind_cols(bineval.10,el = furrr::future_pmap_dbl(bineval.10,
                                                                     el_2prop.wilson, alpha=0.025,
                                                                     .progress = T))

bineval.20.el.wilson <- bind_cols(bineval.10,el = furrr::future_pmap_dbl(bineval.20,
                                                                     el_2prop.wilson, alpha=0.025,
                                                                     .progress = T))



saveRDS(bineval.025.el.wilson, "True_CP/summary/Expected_length/bineval.025.el.wilson.rds")
saveRDS(bineval.10.el.wilson, "True_CP/summary/Expected_length/bineval.10.el.wilson.rds")
saveRDS(bineval.20.el.wilson, "True_CP/summary/Expected_length/bineval.20.el.wilson.rds")
