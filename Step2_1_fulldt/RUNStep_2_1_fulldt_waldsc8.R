library(condor)

build_template(
  file = 'Step_2_1_fulldt_waldsc8.R',
  args = c('$(Process)'),
  tag = 'Xadd',
  jobs = 1,
  init_dir = 'jobs/run',
  template_file = 'fulldt_waldsc8.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  '../savedata/dtfullwald_21.rds'),
  job_type = 'short')


library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Step_2_1_fulldt_waldsc8.R','fulldt_waldsc8.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'fulldt_waldsc8.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'fulldt_waldsc8.condor',sleeptime='10m')

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dtfullwaldsc8X01.rds jobs/savedata')
ssh::ssh_exec_wait(session, 'mv jobs/run/dtfullwaldsc8X09.rds jobs/savedata')

condor::pull(session,
             from = c('jobs/run/log',
                      'jobs/run/out',
                      'jobs/run/err',
                      'jobs/run/dtfull*.rds'),
             to = c('output',
                    'output',
                    'output',
                    'output/data'))

condor::read_errs()

condor::cleanup_remote(session)

ssh::ssh_disconnect(session)

corr_ch <- purrr::map_df(
  list.files(path = "output/data",  full.names = TRUE),
  readRDS)
