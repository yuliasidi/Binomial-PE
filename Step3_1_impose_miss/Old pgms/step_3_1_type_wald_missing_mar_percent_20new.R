#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])
.libPaths('ysidi/lib')


dt.full.X <- readRDS(file = sprintf('dtfullwald_%d.rds',idx))

set.seed(8762+idx)

#generate mar for 5-25% by 5% DO
dt.mar <- miss.apply.do(dt.full.X, b.trt=log(1), b.y=log(1), b.X=-log(10), do=0.20)

dt.mar.check <- dt.mar%>%
  dplyr::mutate(do.H0 = purrr::map2(t.H0.m, 0.20, check.miss),
                do.H0 = purrr::map_dbl(do.H0, as.numeric),
                do.H1 = purrr::map2(t.H1.m, 0.20, check.miss),
                do.H1 = purrr::map_dbl(do.H1, as.numeric))%>%
  dplyr::select(-t.H0.m, -t.H1.m)


saveRDS(dt.mar.check,file = sprintf('walddochmar20_%d.rds',idx))

saveRDS(dt.mar,file = sprintf('dtwaldmar20_%d.rds',idx))

#check missingness mechanism: randomly select two scenarios and calculate
#proportion of assigned missing for each probability value (rounded by 2 decimal points)
# for both MAR and MNAR there should be positive correlation

check.mech.mar <- dt.mar%>%
  dplyr::mutate(mech.H0  = purrr::map(t.H0.m , check.mech.p),
                mech.H1  = purrr::map(t.H1.m , check.mech.p))%>%
  dplyr::select(-t.H0.m, -t.H1.m)

saveRDS(check.mech.mar,file = sprintf('mechmarwald20_%d.rds',idx))
