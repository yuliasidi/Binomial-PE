library(condor)

build_template(
  file = 'Step_2_1_Full_Data_1SSFM.R',
  args = c('$(Process)'),
  tag = 'Full1SSFM',
  jobs = 30,
  init_dir = 'jobs/run',
  template_file = 'Full1SSFM.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  'SS_power90FM.rds'),
  job_type = 'short')


library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Step_2_1_Full_Data_1SSFM.R','Full1SSFM.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'Full1SSFM.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R',
                          'SS_power90FM.rds'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'Full1SSFM.condor',sleeptime='10m')

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dtfullfm*.rds jobs/savedata')

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

ssh::ssh_disconnect(session)

