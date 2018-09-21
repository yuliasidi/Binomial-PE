library(condor)

build_template(
  file = 'Step_2_1_Full_Data_1SSnmin.R',
  args = c('$(Process)'),
  tag = 'Full1SSnmin',
  jobs = 30,
  init_dir = 'jobs/run',
  template_file = 'Full1SSnmin.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  'SS_power90nmin.rds'),
  job_type = 'standard')

build_template(
  file = 'Step_2_1_Full_Data_1SSnmax.R',
  args = c('$(Process)'),
  tag = 'Full1SSnmax',
  jobs = 30,
  init_dir = 'jobs/run',
  template_file = 'Full1SSnmax.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  'SS_power90nmax.rds'),
  job_type = 'standard')

library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Step_2_1_Full_Data_1SSnmin.R','Full1SSnmin.condor',
                          'Step_2_1_Full_Data_1SSnmax.R','Full1SSnmax.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'Full1SSnmin.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R',
                          'SS_power90nmin.rds','SS_power90nmax.rds'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'Full1SSnmin.condor',sleeptime='10m')
condor::condor_submit(session,'Full1SSnmax.condor',sleeptime='10m')

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dtfullnmin*.rds jobs/savedata')

condor::pull(session,
             from = c('jobs/run/log',
                      'jobs/run/out',
                      'jobs/run/err',
                      'jobs/run/dtfullsum*.rds',
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

saveRDS(dtfullsum, 'dtfullsum_nmin100000.rds')

simcheck <- purrr::map_df(
  list.files(path = "output/data", pattern = "check",  full.names = TRUE),
  readRDS
)

saveRDS(simcheck, 'Simchecks/dtfullnminsum_check.rds')

condor::cleanup_local(dir = 'output',tag = 'Full')
condor::cleanup_local(dir = 'output',tag = 'dtfull')
condor::cleanup_local(dir = 'output',tag = 'check')

ssh::ssh_disconnect(session)

unlink('Full1SS.condor')
