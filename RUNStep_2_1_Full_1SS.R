library(condor)

build_template(
  file = 'Step_2_1_Full_Data_1SS.R',
  args = c('$(Process)'),
  tag = 'Full1SS',
  jobs = 30,
  init_dir = 'jobs/run',
  template_file = 'Full1SS.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  'SS_power90.rds'),
  job_type = 'short')


library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Step_2_1_Full_Data_1SS.R','Full1SS.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'Full1SS.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R',
                          'SS_power90.rds'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'Full1SS.condor',sleeptime='10m')

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dtfull_*.rds jobs/savedata')

condor::pull(session,
             from = c('jobs/run/log',
                      'jobs/run/out',
                      'jobs/run/err',
                      'jobs/run/*.rds'),
             to = c('output',
                    'output',
                    'output',
                    'output/data'))

condor::read_errs()

condor::cleanup_remote(session)

library(purrr)

dtfullsum <- purrr::map_df(
  list.files(path = "output/data", pattern = "dt",  full.names = TRUE),
  readRDS
)

simcheck <- purrr::map_df(
  list.files(path = "output/data", pattern = "check",  full.names = TRUE),
  readRDS
)


saveRDS(simcheck, 'Simchecks/dtfullsum_check.rds')

condor::cleanup_local(dir = 'output',tag = 'check')

ssh::ssh_disconnect(session)

unlink('Full1SS.condor')
