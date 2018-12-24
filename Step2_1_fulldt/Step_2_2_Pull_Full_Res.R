source('Step_0_init.R')


simcheck <- purrr::map_df(
  list.files(path = "output/data", pattern = "check",  full.names = TRUE),
  readRDS
)

saveRDS(simcheck, 'checksum_each.rds')


dtsum.wald <- purrr::map_df(
  list.files(path = "output/data", pattern = "dtfullsumwald",  full.names = TRUE),
  readRDS
)


dtsum.fm <- purrr::map_df(
  list.files(path = "output/data", pattern = "dtfullsumfm",  full.names = TRUE),
  readRDS
)


dtsum.wn <- purrr::map_df(
  list.files(path = "output/data", pattern = "dtfullsumwn",  full.names = TRUE),
  readRDS
)

dtfullsum <-dtsum.wald%>%
  rename(N.Wald = N.total)%>%
  right_join(dtsum.fm%>%
               rename(N.FM = N.total)%>%
               select(scenario.id, N.FM, type1.FM, power.FM), by = 'scenario.id')%>%
  right_join(dtsum.wn%>%
               rename(N.WN = N.total)%>%
               select(scenario.id, N.WN, type1.WN, power.WN), by = 'scenario.id')


saveRDS(dtfullsum, 'dtfullsum_each.rds')

condor::cleanup_local(dir = 'output',tag = 'Full')
condor::cleanup_local(dir = 'output',tag = 'dtfull')
condor::cleanup_local(dir = 'output',tag = 'check')

unlink('Full1SSWald.condor')
unlink('Full1SSFM.condor')
unlink('Full1SSWN.condor')



#########################################
#### Check 20000 simulations for Wald ###
#########################################

simcheck <- purrr::map_df(
  list.files(path = "output/data", pattern = "check",  full.names = TRUE),
  readRDS
)

saveRDS(simcheck, 'checksum_wald20000.rds')


dtsum.wald <- purrr::map_df(
  list.files(path = "output/data", pattern = "dtfullsumwald",  full.names = TRUE),
  readRDS
)

saveRDS(dtsum.wald, 'dtfullsum_wald20000.rds')

