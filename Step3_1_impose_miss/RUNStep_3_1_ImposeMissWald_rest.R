library(condor)
source('Step_0_init.R')


# generate condor files for all the scenarios
scenarios <- data.frame(
  MISSING=c('mar4','mnar1','mnar2'),
  PERCENT=c('15','15','25'),
  TYPE=c('waldxm30','waldxm30','waldxm30'))
 
scenarios_list <- as.list(scenarios)


build_template(
    file = 'step_3_1_type_waldxm30_missing_mar4_percent_15_rest.R',
    args = c('$(Process)'),
    tag = 'mar4_rest',
    jobs = 1,
    init_dir = 'jobs/run',
    template_file = 'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mar4_percent_15_rest.condor',
    input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  '../savedata/dtfullwaldxm30_0.rds'),
    job_type = 'short')


build_template(
  file = 'step_3_1_type_waldxm30_missing_mnar1_percent_15_rest.R',
  args = c('$(Process)'),
  tag = 'mnar1_rest',
  jobs = 1,
  init_dir = 'jobs/run',
  template_file = 'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mnar1_percent_15_rest.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  '../savedata/dtfullwaldxm30_0.rds'),
  job_type = 'short')


build_template(
  file = 'step_3_1_type_waldxm30_missing_mnar2_percent_25_rest.R',
  args = c('$(Process)'),
  tag = 'mnar2_rest',
  jobs = 1,
  init_dir = 'jobs/run',
  template_file = 'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mnar2_percent_25_rest.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  '../savedata/dtfullwaldxm30_8.rds'),
  job_type = 'short')


library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))



ssh::scp_upload(session,
                files = c('Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mar4_percent_15_rest.R',
                          'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mnar1_percent_15_rest.R',
                          'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mnar2_percent_25_rest.R',
                          'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mar4_percent_15_rest.condor',
                          'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mnar1_percent_15_rest.condor',
                          'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mnar2_percent_25_rest.condor'
                          ),
                to = '~')
condor::create_dirs(session, file = 'Step3_1_impose_miss/wald/step_3_1_type_waldxm30_missing_mar4_percent_15_rest.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'step_3_1_type_waldxm30_missing_mar4_percent_15_rest.condor',sleeptime='5m')  
condor::condor_submit(session,'step_3_1_type_waldxm30_missing_mnar1_percent_15_rest.condor',sleeptime='5m')  
condor::condor_submit(session,'step_3_1_type_waldxm30_missing_mnar2_percent_25_rest.condor',sleeptime='5m')  

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

condor::pull(session,
             from = c('jobs/run/err'),
             to = c('output'))

condor::read_errs()

condor::pull(session,
             from = c('jobs/run/*doch*.rds'),
             to = c('output/data'))

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dt*.rds jobs/savedata')

condor::cleanup_remote(session)

ssh::ssh_disconnect(session)



#condor::cleanup_local(dir = 'output',tag = 'fm')
#condor::cleanup_local(dir = 'output',tag = 'wn')
condor::cleanup_local(dir = 'output',tag = 'wald')

