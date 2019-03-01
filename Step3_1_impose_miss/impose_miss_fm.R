library(future)

source('Step_0_init.R')
source("funs/miss.fun.R")

dt <- readRDS("clusterdata/dtfullfm_5.rds") 

dt0 <- dt%>%
  dplyr::select(scenario.id, p_C, p_T, M2, t.H0, t.H1)%>%
  tidyr::gather("type", "value", c(t.H0, t.H1))%>%
  tidyr::unnest(value)

x <- dt0%>%dplyr::group_split(type,sim.id)

#define missingness parameters
m.param <- tibble::tibble(missing = c("mcar", "mar1", "mar2", "mar3", "mar4", "mar5", "mar6", "mnar1", "mnar2"))
m.param <- m.param%>%miss.param.assign()


myprocess <- future::tweak(future::multiprocess,
                           workers = future::availableCores() - 2)

future::plan(myprocess)

set.seed(3423)
x1 <- furrr::future_map(x,.f=function(y){
  m.param%>%
    group_split(missing)%>%
    purrr::set_names(sort(m.param$missing))%>%
    purrr::map_df(.f=function(xx,df){
      miss.fun(df = df, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, method = "pweight")
    } ,df = y,.id = 'missing')  
},.progress = TRUE)

x2 <- furrr::future_map2(x1, 'y.m', FM.CI,.options = furrr::future_options(scheduling = 10))

x3 <- bind_rows(x2)

x4 <- x3%>%
  dplyr::group_by(type, missing)%>%
  dplyr::summarise(err = mean(reject.h0))

saveRDS(x4,"DataSummaries/fm/cca_pweight_do10.rds")

set.seed(34231)
x1.alt <- furrr::future_map(x,.f=function(y){
  m.param%>%
    group_split(missing)%>%
    purrr::set_names(sort(m.param$missing))%>%
    purrr::map_df(.f=function(xx,df){
      miss.fun(df = df, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, method = "psort")
    } ,df = y,.id = 'missing')  
},.progress = TRUE)

x2 <- furrr::future_map2(x1.alt, 'y.m', FM.CI,.options = furrr::future_options(scheduling = 10))

x3 <- bind_rows(x2)

x4.alt <- x3%>%
  dplyr::group_by(type, missing)%>%
  dplyr::summarise(err = mean(reject.h0))

saveRDS(x4,"DataSummaries/fm/cca_psort_do10.rds")

x5 <- left_join(x4%>%
  tidyr::spread(type,err)%>%
  dplyr::rename(type1.pweight = t.H0,
                power.pweight = t.H1),
x4.alt%>%
  tidyr::spread(type,err)%>%
  dplyr::rename(type1.sort = t.H0,
                power.sort = t.H1),
by = 'missing')


x6 <- left_join(x5, m.param, by = "missing")%>%
  dplyr::select(c(missing, b.trt, b.Y, b.X, type1.pweight, type1.sort,
                power.pweight, power.sort))

saveRDS(x6, "DataSummaries/fm/cca_do10.rds")

###################################################################################
#check the distribution of p for r=1 and r=0 for one particular simulation scenario
# and compare pweight and psort methods

k<-x[14444]

k1 <- furrr::future_map(k,.f=function(y){
  m.param%>%
    group_split(missing)%>%
    purrr::set_names(sort(m.param$missing))%>%
    purrr::map_df(.f=function(xx,df){
      miss.fun(df = df, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, method = "pweight")
    } ,df = y,.id = 'missing')  
},.progress = TRUE)

k2 <- furrr::future_map(k,.f=function(y){
  m.param%>%
    group_split(missing)%>%
    purrr::set_names(sort(m.param$missing))%>%
    purrr::map_df(.f=function(xx,df){
      miss.fun(df = df, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, method = "psort")
    } ,df = y,.id = 'missing')  
},.progress = TRUE)

k1.1 <- k1[[1]]

k1.g <- k1.1%>%
  ggplot2::ggplot(aes(p)) +
  ggplot2::geom_density(aes(fill=factor(r)), alpha = 0.8) +
  ggplot2::facet_wrap(~c(missing)) +
  ggplot2::labs(title = "Missing according to weighted P")

k2.1 <- k2[[1]]

k2.g <- k2.1%>%
  ggplot2::ggplot(aes(p)) +
  ggplot2::geom_density(aes(fill=factor(r)), alpha = 0.8) +
  ggplot2::facet_wrap(~c(missing)) +
  ggplot2::labs(title = "Missing according to sorted P")

pdf("DataSummaries/fm/graphs/cca_pweight_dist10.pdf")
k1.g
dev.off()

pdf("DataSummaries/fm/graphs/cca_psort_dist10.pdf")
k2.g
dev.off()



#what happens if do is set to 25% -> similar results to the above

# k1 <- furrr::future_map(k,.f=function(y){
#   m.param%>%
#     group_split(missing)%>%
#     purrr::set_names(sort(m.param$missing))%>%
#     purrr::map_df(.f=function(xx,df){
#       miss.fun(df = df, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, method = "pweight", 
#                do = 0.25)
#     } ,df = y,.id = 'missing')  
# },.progress = TRUE)
# 
# k2 <- furrr::future_map(k,.f=function(y){
#   m.param%>%
#     group_split(missing)%>%
#     purrr::set_names(sort(m.param$missing))%>%
#     purrr::map_df(.f=function(xx,df){
#       miss.fun(df = df, b.trt = xx$b.trt, b.Y = xx$b.Y,b.X = xx$b.X, method = "psort",
#                do = 0.25)
#     } ,df = y,.id = 'missing')  
# },.progress = TRUE)
# 
# k1.1 <- k1[[1]]
# 
# k1.g <- k1.1%>%
#   ggplot2::ggplot(aes(p)) +
#   ggplot2::geom_density(aes(fill=factor(r)), alpha = 0.8) +
#   ggplot2::facet_wrap(~c(missing)) +
#   ggplot2::labs(title = "Missing according to weighted P")
# 
# k2.1 <- k2[[1]]
# 
# k2.g <- k2.1%>%
#   ggplot2::ggplot(aes(p)) +
#   ggplot2::geom_density(aes(fill=factor(r)), alpha = 0.8) +
#   ggplot2::facet_wrap(~c(missing)) +
#   ggplot2::labs(title = "Missing according to sorted P")



