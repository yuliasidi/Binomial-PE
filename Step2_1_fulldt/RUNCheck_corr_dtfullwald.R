library(condor)
source('Step_0_init.R')

build_template(
    file = 'Check_corr_dtfullwald.R',
    args = c('$(Process)'),
    tag = 'corcheck',
    jobs = 30,
    init_dir = 'jobs/run',
    template_file = 'Check_corr_dtfullwald.condor',
    input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                    '../savedata/dtfullwald_$(Process).rds'),
    job_type = 'short')

library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Check_corr_dtfullwald.R','Check_corr_dtfullwald.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'Check_corr_dtfullwald.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'Check_corr_dtfullwald.condor',sleeptime='10m')

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
                      'jobs/run/dt*.rds'),
             to = c('output',
                    'output',
                    'output/data'))

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dt*.rds jobs/savedata')

condor::cleanup_remote(session)

ssh::ssh_disconnect(session)

corr_ch <- purrr::map_df(
  list.files(path = "output/data",  full.names = TRUE),
  readRDS)

saveRDS(corr_ch,'Simchecks/corrwald03.rds' )

condor::cleanup_local(dir = 'output',tag = 'wald')
unlink("Check_corr_dtfullwald.condor")
