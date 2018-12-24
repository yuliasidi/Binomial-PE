library(condor)

build_template(
  file = 'Step_2_1_fulldt_1SSwald_addXp15.R',
  args = c('$(Process)'),
  tag = 'Xaddp15',
  jobs = 30,
  init_dir = 'jobs/run',
  template_file = 'fulldt_wald_addXp15.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R','bounds.rds',
                  '../savedata/dtfullwald_$(Process).rds'),
  job_type = 'short')


library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Step_2_1_fulldt_1SSwald_addXp15.R','fulldt_wald_addXp15.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'fulldt_wald_addXp15.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R','bounds.rds'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'fulldt_wald_addXp15.condor',sleeptime='10m')

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

# MOVE DATAFILES YOU WANT TO SAFE
#ssh::ssh_exec_wait(session, 'mv jobs/run/dtfullwaldsc8X01.rds jobs/savedata')
#ssh::ssh_exec_wait(session, 'mv jobs/run/dtfullwaldsc8X09.rds jobs/savedata')

condor::pull(session,
             from = c('jobs/run/log',
                      'jobs/run/out',
                      'jobs/run/err',
                      'jobs/run/chdtfull*.rds'),
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

condor::cleanup_local("output/data", tag="ch")
condor::cleanup_local("output/err",  tag="add")
condor::cleanup_local("output/log",  tag="add")
condor::cleanup_local("output/out",  tag="add")
