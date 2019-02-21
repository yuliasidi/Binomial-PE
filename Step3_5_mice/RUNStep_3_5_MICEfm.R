library(condor)
source('Step_0_init.R')


# generate condor files for all the scenarios
scenarios <- expand.grid(
  TYPE=c('wald'),
  REPN=seq(1,100,1),
  #TYPE=c('wald','waldxm30','waldxmm','waldxp80'),
  MISSING=c('mcar','mar1','mar2','mar3','mar4','mar5','mar6','mnar1','mnar2'),
  PERCENT=sprintf('%02d',seq(5,25,5))
  ,stringsAsFactors = FALSE)

 
scenarios_list <- as.list(scenarios)

app_temp <- function(TYPE,MISSING,PERCENT,REPN){
  
  build_template(
    file = sprintf('step_3_5_type_%s_missing_%s_percent_%s_%d.R',TYPE,MISSING,PERCENT,REPN),
    args = c('$(Process)'),
    tag = sprintf('mice%s_%s_%s_%d',TYPE,MISSING,PERCENT,REPN),
    jobs = 30,
    mem = 10,
    init_dir = 'jobs/run',
    template_file = sprintf('Step3_5_mice/wald/step_3_5_type_%s_missing_%s_percent_%s_%d.condor',TYPE,MISSING,PERCENT,REPN),
    input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R', 'df_names.rds',
                    sprintf('../savedata/dt%s%s%s_$(Process).rds',TYPE, MISSING, PERCENT)),
    job_type = "short")
}


purrr::pwalk(scenarios_list, app_temp)



library(ssh)

session_hpc <- ssh_connect(Sys.getenv('UCONN_USER_HPC'))

scenarios1 <- scenarios%>%
  dplyr::mutate(fnameR = 
                  sprintf('Step3_5_mice/wald/step_3_5_type_%s_missing_%s_percent_%s_%d.R',TYPE,MISSING,PERCENT,REPN),
                fnameC = 
                  sprintf('Step3_5_mice/wald/step_3_5_type_%s_missing_%s_percent_%s_%d.condor',TYPE,MISSING,PERCENT,REPN),
                fnameC.submit = 
                  sprintf('step_3_5_type_%s_missing_%s_percent_%s_%d.condor',TYPE,MISSING,PERCENT, REPN))

fname.listR <- as.vector(scenarios1$fnameR)
fname.listC <- as.vector(scenarios1$fnameC)
fname.listC.submit <- as.vector(scenarios1$fnameC.submit)


ssh::scp_upload(session,
                files = fname.listR,
                to = '~')
ssh::scp_upload(session,
                files = fname.listC,
                to = '~')


condor::create_dirs(session, file = 'Step3_5_mice/wald/step_3_5_type_wald_missing_mnar1_percent_25_1.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R', 'PE_Bin_PD_Functions.R', 'df_names.rds'),
                to = '~/jobs/run'
)

c.submit <- function(i){
  condor::condor_submit(session,fname.listC.submit[i],sleeptime='5m')  
}

#Run only wald rho=max
purrr::walk(seq(901,950,1), c.submit)


condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

condor::pull(session,
             from = c('jobs/run/err'),
             to = c('output'))

condor::read_errs()

condor::pull(session,
             from = c('jobs/run/micewaldmcar05*.rds'),
             to = c('output/data'))

# MOVE DATAFILES YOU WANT TO SAFE
#ssh::ssh_exec_wait(session, 'mv jobs/run/dt*.rds jobs/savedata')

condor::cleanup_remote(session)

ssh::ssh_disconnect(session)



t <- purrr::map_df(
  list.files(path = "output/data", 
             pattern = "mcar05",  full.names = TRUE),
  readRDS)
t.H0.mice <-t%>%
  tidyr::unnest()%>%
  dplyr::select(-t.H1.mice)%>%
  tidyr::unnest()%>%
  mi.res.sum()
  

head(t.H0.mice)

scenarios.cca <- scenarios%>%
  mutate(ANAL = 'cca')

scenarios.cca_waldp30 <- as.list(scenarios.cca%>%slice(1:45))
cca_waldp30 <- purrr::pmap_df(scenarios.cca_waldp30, read_anal)

saveRDS(cca_waldp30,"DataSummaries/cca_waldp30.rds")

#condor::cleanup_local(dir = 'output',tag = 'fm')
#condor::cleanup_local(dir = 'output',tag = 'wn')
condor::cleanup_local(dir = 'output',tag = 'cca')

