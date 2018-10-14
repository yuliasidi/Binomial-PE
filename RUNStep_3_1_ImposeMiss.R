library(condor)
source('Step_0_init.R')


# generate condor files for all the scenarios
scenarios <- expand.grid(
  TYPE=c('wald','fm','wn'),
  MISSING=c('mcar','mar', 'mnar'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)

scenarios_list <- as.list(scenarios)

app_temp <- function(TYPE,MISSING,PERCENT){
  
  build_template(
    file = sprintf('step_3_1_type_%s_missing_%s_percent_%s.R',TYPE,MISSING,PERCENT),
    args = c('$(Process)'),
    tag = sprintf('%s_%s_%s',TYPE,MISSING,PERCENT),
    jobs = 30,
    init_dir = 'jobs/run',
    template_file = sprintf('impose_miss/step_3_1_type_%s_missing_%s_percent_%s.condor',TYPE,MISSING,PERCENT),
    input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                    sprintf('../savedata/dtfull%s_$(Process).rds',TYPE)),
    job_type = 'short')
}

purrr::pwalk(scenarios_list, app_temp)



library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

scenarios1 <- scenarios%>%
  dplyr::mutate(fnameR = 
                  sprintf('impose_miss/step_3_1_type_%s_missing_%s_percent_%s.R',TYPE,MISSING,PERCENT),
                fnameC = 
                  sprintf('impose_miss/step_3_1_type_%s_missing_%s_percent_%s.condor',TYPE,MISSING,PERCENT),
                fnameC.submit = 
                  sprintf('step_3_1_type_%s_missing_%s_percent_%s.condor',TYPE,MISSING,PERCENT))

fname.listR <- as.vector(scenarios1$fnameR)
fname.listC <- as.vector(scenarios1$fnameC)
fname.listC.submit <- as.vector(scenarios1$fnameC.submit)


ssh::scp_upload(session,
                files = fname.listR,
                to = '~')
ssh::scp_upload(session,
                files = fname.listC,
                to = '~')


condor::create_dirs(session, file = 'impose_miss/step_3_1_type_wn_missing_mnar_percent_25.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R'),
                to = '~/jobs/run'
)

c.submit <- function(i){
  condor::condor_submit(session,fname.listC.submit[i],sleeptime='5m')  
}

purrr::walk(seq(1,45,1), c.submit)

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

condor::pull(session,
             from = c('jobs/run/err'),
             to = c('output'))

condor::read_errs()

condor::pull(session,
             from = c('jobs/run/log',
                      'jobs/run/out',
                      'jobs/run/wndo*.rds',
                      'jobs/run/fmdo*.rds',
                      'jobs/run/walddo*.rds',
                      'jobs/run/mech*.rds'),
             to = c('output',
                    'output',
                    'output/data',
                    'output/data',
                    'output/data',
                    'output/data'))

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dt*.rds jobs/savedata')

condor::cleanup_remote(session)

ssh::ssh_disconnect(session)

#check that the desired do level was achieved:
doch <- purrr::pmap_df(scenarios_list, read_doch)

doch.sum <-
  doch%>%
  group_by(TYPE,MISSING,PERCENT)%>%
  summarise(n=n())


mech <- purrr::pmap_df(scenarios_list, read_mech)

mech.sum <-
  mech%>%
  group_by(TYPE,MISSING,PERCENT)%>%
  summarise(n=n())

saveRDS(doch,"simchecks/docheck.rds")
saveRDS(mech,"simchecks/mechcheck.rds")

condor::cleanup_local(dir = 'output',tag = 'fm')
condor::cleanup_local(dir = 'output',tag = 'wn')
condor::cleanup_local(dir = 'output',tag = 'wald')

