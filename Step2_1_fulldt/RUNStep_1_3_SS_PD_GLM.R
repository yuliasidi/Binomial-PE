library(condor)

build_template(
  file = 'Step_1_3_SS_PD_GLM.R',
  args = c('$(Process)'),
  tag = 'SS_GLM',
  jobs = 30,
  init_dir = 'jobs/run',
  template_file = 'SS_GLM.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  'SS_Wald_FM_WN_power90.rds'),
  job_type = 'short')


library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Step_1_3_SS_PD_GLM.R','SS_GLM.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'SS_GLM.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R',
                          'SS_Wald_FM_WN_power90.rds'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'SS_GLM.condor')

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

condor::pull(session,
             from = c('jobs/run/log',
                      'jobs/run/out',
                      'jobs/run/err',
                      'jobs/run/*.rds'),
             to = c('output',
                    'output',
                    'output',
                    'output/data'))
unlink('output/data/SS_Wald_FM_WN_power90.rds')

condor::cleanup_remote(session)

library(purrr)

SS_addGLM <- purrr::map_df(
  list.files(path = "output/data",full.names = TRUE),
  readRDS
)

saveRDS(SS_addGLM, 'SS_Wald_FM_GLM_power90.rds')

condor::cleanup_local(dir = 'output',tag = 'SS_GLM')

ssh::ssh_disconnect(session)

unlink('SS_GLM.condor')
