#!/home/statsadmin/R/bin/Rscript

source('Step_0_init.R')

args <- commandArgs()
idx <- as.numeric(args[length(args)])+1
.libPaths('ysidi/lib')



#dt.full.X <- readRDS(file = sprintf('dtfullwald_%02d.rds',idx))

dt.full.X <- readRDS('testfull.rds')

#generate MCAR for 5-25% by 5% DO
dt.mcar <- miss.apply(dt.full.X, b.trt=log(1), b.y=log(1), b.X=log(1))

#generate MAR for 5-25% by 5% DO
dt.mar <- miss.apply(dt.full.X, b.trt=log(1), b.y=log(1), b.X=log(10))

#generate MNAR for 5-25% by 5% DO
dt.mnar <- miss.apply(dt.full.X, b.trt=log(1), b.y=log(10), b.X=log(10))


dt.mcar.check <- check.do(dt.mcar)
dt.mar.check  <- check.do(dt.mar)
dt.mnar.check <- check.do(dt.mnar)

dt.miss.check <- dt.mcar.check%>%
  dplyr::mutate(miss = 'mcar')%>%
  dplyr::bind_rows(dt.mar.check%>%
                     dplyr::mutate(miss = 'mar'), 
                   dt.mnar.check%>%
                     dplyr::mutate(miss = 'mnar'))

saveRDS(dt.miss.check,file = sprintf('docheck_%02d.rds',idx))

saveRDS(dt.mcar,file = sprintf('dtmcarwald_%02d.rds',idx))
saveRDS(dt.mar, file = sprintf('dtmarwald_%02d.rds',idx))
saveRDS(dt.mnar,file = sprintf('dtmnarwald_%02d.rds',idx))

#check missingness mechanism: randomly select two scenarios and calculate
#proportion of assigned missing for each probability value (rounded by 2 deciamal points)
# for both MAR and MNAR there should be positive correlation
check.mech.mcar <- check.mech(dt.mcar)
check.mech.mar <- check.mech(dt.mar)
check.mech.mnar <- check.mech(dt.mnar)

saveRDS(check.mech.mcar,file = sprintf('mechmcarwald_%02d.rds',idx))
saveRDS(check.mech.mar, file = sprintf('mechmarwald_%02d.rds',idx))
saveRDS(check.mech.mnar,file = sprintf('mechmnarwald_%02d.rds',idx))
