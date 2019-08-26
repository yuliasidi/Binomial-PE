ll <- seq(1,30,1)


sc_nsim <-
           map_df(ll, 
                   .f = function(sc) {
                     df <- readRDS(list.files("cluster/out/wald/2xcont/do5", 
                                              sprintf("cont2xH0_wald_mice_sc%s_do5_param1.rds", sc), 
                                              full.names = T))
                     df1 <- df%>%
                       purrr::map_df(.f=function(x) x$ci.full,.id = 'sim')%>%
                       summarise(nsim = n())
                   })
  


x1.sc25.mice.do5.1 <- readRDS("cluster/out/wn/2xcont/do5/cont2xH0_wn_mice_sc25_do5_param1.rds")
x1.sc25.mice.do5.2 <- readRDS("cluster/out/wn/2xcont/do5/cont2xH0_wn_mice_sc25_do5_param12.rds")
x1.sc25.mice.do5 <- append(x1.sc25.mice.do5.1,x1.sc25.mice.do5.2)
remove(x1.sc25.mice.do5.1, x1.sc25.mice.do5.2)

sc_nsim_wn <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/wn/2xcont/do5", 
                                    sprintf("cont2xH0_wn_mice_sc%s_do5_param1.rds", sc), 
                                    full.names = T))
           df1 <- df%>%
             purrr::map_df(.f=function(x) x$ci.full,.id = 'sim')%>%
             summarise(nsim = n())
         })


sc_nsim_wn <- sc_nsim_wn%>%
  mutate(i = seq(1,30,1))%>%
  filter(i!=25)%>%
  bind_rows(x1.sc25.mice.do5%>%
              purrr::map_df(.f=function(x) x$ci.full,.id = 'sim')%>%
              summarise(nsim = n())%>%
              mutate(i = 25))%>%
  arrange(i)
  
  

sc_nsim_fm <-
  map_df(ll, 
         .f = function(sc) {
           df <- readRDS(list.files("cluster/out/fm/2xcont/do5", 
                                    sprintf("cont2xH0_fm_mice_sc%s_do5_param1.rds", sc), 
                                    full.names = T))
           df1 <- df%>%
             purrr::map_df(.f=function(x) x$ci.full,.id = 'sim')%>%
             summarise(nsim = n())
         })


nsim <- bind_cols(sc_nsim_wald%>%
                    rename(nsim_wald = nsim),
                  sc_nsim_wn%>%
                    rename(nsim_wn = nsim),
                  sc_nsim_fm%>%
                    rename(nsim_fm = nsim))

saveRDS(nsim, "cluster/checks/nsim_per_sc.rds")
