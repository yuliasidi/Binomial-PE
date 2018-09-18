library(condor)

build_template(
  file = 'Step_2_1_Full_Data_1SSWald.R',
  args = c('$(Process)'),
  tag = 'Full1SSWald',
  jobs = 30,
  init_dir = 'jobs/run',
  template_file = 'Full1SSWald.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  'SS_power90Wald.rds'),
  job_type = 'short')


library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Step_2_1_Full_Data_1SSWald.R','Full1SSWald.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'Full1SSWald.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R',
                          'SS_power90Wald.rds'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'Full1SSWald.condor',sleeptime='10m')

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dtfullwald*.rds jobs/savedata')

condor::pull(session,
             from = c('jobs/run/log',
                      'jobs/run/out',
                      'jobs/run/err',
                      'jobs/run/dtfull*.rds',
                      'jobs/run/check*.rds'),
             to = c('output',
                    'output',
                    'output',
                    'output/data',
                    'output/data'))

condor::read_errs()

condor::cleanup_remote(session)

library(purrr)

dtfullsum <- purrr::map_df(
  list.files(path = "output/data", pattern = "dt",  full.names = TRUE),
  readRDS
)

saveRDS(dtfullsum, 'dtfullsum_nmin.rds')

simcheck <- purrr::map_df(
  list.files(path = "output/data", pattern = "check",  full.names = TRUE),
  readRDS
)

saveRDS(simcheck, 'Simchecks/dtfullnminsum_check.rds')

condor::cleanup_local(dir = 'output',tag = 'Full')
condor::cleanup_local(dir = 'output',tag = 'dtfull')
condor::cleanup_local(dir = 'output',tag = 'check')

ssh::ssh_disconnect(session)

unlink('Full1SSWald.condor')
