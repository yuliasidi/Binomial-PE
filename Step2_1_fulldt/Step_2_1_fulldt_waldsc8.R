#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

.libPaths('ysidi/lib')

dt.full <- readRDS(file = "dtfullwald_21.rds")

dt.full <- readRDS(file = "testfull_01.rds")

#########################################################
########      Generate Patient Level Data     ###########
#########################################################
set.seed(3865)

dt.full.X0.1 <- add.X.rho(dt.full, r = 0.1) 
dt.full.X0.7 <- add.X.rho(dt.full, r = 0.7) 

saveRDS(dt.full.X0.1, file = 'dtfullwaldsc8X01.rds')
saveRDS(dt.full.X0.9, file = 'dtfullwaldsc8X09.rds')


check0.1 <- bind_rows(dt.full.X0.1$t.H0[[1]]%>%check_rho(),
                      dt.full.X0.1$t.H1[[1]]%>%check_rho())
check0.7 <- bind_rows(dt.full.X0.7$t.H0[[1]]%>%check_rho(),
                      dt.full.X0.7$t.H1[[1]]%>%check_rho())

saveRDS(check0.1, file = 'dtfullwaldsc8X01ch.rds')
saveRDS(check0.9, file = 'dtfullwaldsc8X09ch.rds')



t<-dt.full.X0.8$t.H0[[1]]

check_rho <- function(df){
  df
  t1<-t%>%
    tidyr::nest(-sim.id)%>%
    dplyr::mutate(cor = map(data , .f = function(df){
      cor(df$y,df$X, method = "spearman")
    }))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::summarise(mean.cor = round(mean(cor),2), sd = round(sqrt(var(cor)),2))
}


