library(ssh)
library(condor)
library(future)

future::plan(future::multiprocess)

a %<-% future.apply::future_lapply(
  'my-archive/dtfullfm*.rds',
  FUN = function(x){
    condor::pull(session = ssh_connect(Sys.getenv('UCONN_USER')),
                 from = x,
                 to='clusterdata')
  }
)

f <- future::futureOf(a)
future::resolved(f)
