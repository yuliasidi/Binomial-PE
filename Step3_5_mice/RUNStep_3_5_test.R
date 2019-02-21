library(condor)
source('Step_0_init.R')



  build_template(
    file = 'step_3_5_test.R',
    args = c('$(Process)'),
    tag = 'test',
    jobs = 1,
    init_dir = 'jobs/run',
    template_file ='Step3_5_mice/wald/step_3_5_test.condor',
    input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R', 'df_names.rds',
                    '../savedata/dtwaldxp80mnar225_$(Process).rds'),
    job_type = "test")



library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))


ssh::scp_upload(session,
                files = "Step3_5_mice/wald/step_3_5_test.R",
                to = '~')
ssh::scp_upload(session,
                files = "Step3_5_mice/wald/step_3_5_test.condor",
                to = '~')


#condor::create_dirs(session, file = 'Step3_5_mice/wald/step_3_5_test.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R', 'PE_Bin_PD_Functions.R', 'df_names.rds'),
                to = '~/jobs/run'
)

condor::condor_submit(session,"step_3_5_test.condor",sleeptime='1m')  

#Run only wald rho=max
purrr::walk(seq(45,45,1), c.submit)

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
                      'jobs/run/*.rds'),
             to = c('output',
                    'output',
                    'output/data'))

# MOVE DATAFILES YOU WANT TO SAFE
#ssh::ssh_exec_wait(session, 'mv jobs/run/dt*.rds jobs/savedata')

condor::cleanup_remote(session)

ssh::ssh_disconnect(session)

scenarios.cca <- scenarios%>%
  mutate(ANAL = 'cca')

scenarios.cca_waldp30 <- as.list(scenarios.cca%>%slice(1:45))
cca_waldp30 <- purrr::pmap_df(scenarios.cca_waldp30, read_anal)

saveRDS(cca_waldp30,"DataSummaries/cca_waldp30.rds")

#condor::cleanup_local(dir = 'output',tag = 'fm')
#condor::cleanup_local(dir = 'output',tag = 'wn')
condor::cleanup_local(dir = 'output',tag = 'cca')

