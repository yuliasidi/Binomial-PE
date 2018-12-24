library(condor)

build_template(
  file = 'CP_FM.R',
  args = c('$(Process)'),
  tag = 'cp_fm',
  jobs = 808,
  init_dir = 'jobs/run',
  template_file = 'True_CP/CP_FM.condor',
  input_files = c('../myfiles/lib/', 'bineval.025.rds'),
  job_type = 'short')

build_template(
  file = 'CP_FM10.R',
  args = c('$(Process)'),
  tag = 'cp_fm10',
  jobs = 808,
  init_dir = 'jobs/run',
  template_file = 'True_CP/CP_FM10.condor',
  input_files = c('../myfiles/lib/', 'bineval.10.rds'),
  job_type = 'short')


build_template(
  file = 'CP_FM20.R',
  args = c('$(Process)'),
  tag = 'cp_fm20',
  jobs = 808,
  init_dir = 'jobs/run',
  template_file = 'True_CP/CP_FM20.condor',
  input_files = c('../myfiles/lib/', 'bineval.20.rds'),
  job_type = 'short')
library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('True_CP/CP_FM.condor','True_CP/CP_FM.R'),
                to = '~')

ssh::scp_upload(session,
                files = c('True_CP/CP_FM10.condor','True_CP/CP_FM10.R',
                          'True_CP/CP_FM20.condor','True_CP/CP_FM20.R'),
                to = '~')

condor::create_dirs(session, file = 'True_CP/CP_FM.condor')
#condor::create_dirs(session, file = 'True_CP/CP_FM10.condor')

ssh::scp_upload(session,
                files = 'True_CP/bineval.025.rds',
                to = '~/jobs/run'
)
condor::condor_submit(session,'CP_FM.condor',sleeptime='5s')  

ssh::scp_upload(session,
                files = c('True_CP/bineval.10.rds','True_CP/bineval.20.rds'),
                to = '~/jobs/run'
)
condor::condor_submit(session,'CP_FM10.condor',sleeptime='5m')  
condor::condor_submit(session,'CP_FM20.condor',sleeptime='5m')  


condor::pull(session,
             from = c('jobs/run/err/*fm20*'),
             to = c('True_CP/output/err'))

read_errs_cp <- function(err_path = 'True_CP/output/err/'){
  
  err_files <- list.files(err_path,full.names = TRUE)
  
  err_files <- err_files[file.info(err_files)['size']>0]
  
  if(length(err_files)==0){
    message('No errors')
    return(invisible(NULL))
  }
  
  
  err_lines <- purrr::map_chr(err_files,function(x){
    paste0(readLines(x),collapse = '\n')
  })
  
  names(err_lines) <- basename(err_files)
  
  err_lines%>%
    tibble::enframe()%>%
    dplyr::group_by(!!rlang::sym('value'))%>%
    dplyr::summarise(name=paste0(!!rlang::sym('name'),collapse = ','))%>%
    dplyr::ungroup()%>%
    dplyr::summarise(s = sprintf('%s\n\n%s',!!rlang::sym('name'),!!rlang::sym('value')))%>%
    dplyr::pull(!!rlang::sym('s'))%>%
    writeLines()
  
}

read_errs_cp()

condor::pull(session,
             from = c('jobs/run/*fm20*.rds'),
             to = c('True_CP/output/data'))

condor::cleanup_remote(session)


bineval.025.cp.fm <- purrr::map_df(
  list.files(path = "True_CP/output/data", 
             pattern = 'binevalcp_fm_',  full.names = TRUE),
  readRDS)

bineval.10.cp.fm <- purrr::map_df(
  list.files(path = "True_CP/output/data", 
             pattern = 'binevalcp_fm10_',  full.names = TRUE),
  readRDS)


bineval.20.cp.fm <- purrr::map_df(
  list.files(path = "True_CP/output/data", 
             pattern = 'binevalcp_fm20_',  full.names = TRUE),
  readRDS)

saveRDS(bineval.025.cp.fm, 'True_CP/summary/bineval.025.cp.fm.rds')
saveRDS(bineval.10.cp.fm, 'True_CP/summary/bineval.10.cp.fm.rds')
saveRDS(bineval.20.cp.fm, 'True_CP/summary/bineval.20.cp.fm.rds')


condor::cleanup_local(dir = 'True_CP/output',tag = 'fm')


