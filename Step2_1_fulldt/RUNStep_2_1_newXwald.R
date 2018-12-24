library(condor)

build_template(
  file = 'Step_2_1_newXwald.R',
  args = c('$(Process)'),
  tag = 'newXwald',
  jobs = 30,
  init_dir = 'jobs/run',
  template_file = 'Step2_1_fulldt/newXwald.condor',
  input_files = c('../myfiles/lib/','Step_0_init.R', 'PE_Bin_PD_Functions.R',
                  'bounds.rds', sprintf('../savedata/dtfullwald_$(Process).rds')),
  job_type = 'short')


library(ssh)

session <- ssh_connect(Sys.getenv('UCONN_USER'))

ssh::scp_upload(session,
                files = c('Step2_1_fulldt/Step_2_1_newXwald.R',
                          'Step2_1_fulldt/newXwald.condor'),
                to = '~'
)

condor::create_dirs(session, file = 'Step2_1_fulldt/newXwald.condor')

ssh::scp_upload(session,
                files = c('Step_0_init.R','PE_Bin_PD_Functions.R',
                          'bounds.rds'),
                to = '~/jobs/run'
)

condor::condor_submit(session,'newXwald.condor',sleeptime='10m')

condor::condor_q(session)

#my laptop
#dir.create('example/output/data',recursive = TRUE)

# MOVE DATAFILES YOU WANT TO SAFE
ssh::ssh_exec_wait(session, 'mv jobs/run/dtfullwald*.rds jobs/savedata')

condor::pull(session,
             from = c('jobs/run/log',
                      'jobs/run/out',
                      'jobs/run/err',
                      'jobs/run/chdt*.rds'),
             to = c('output',
                    'output',
                    'output',
                    'output/data'))

condor::read_errs()

condor::cleanup_remote(session)

ssh::ssh_disconnect(session)

#check different corr. coef.
library(dplyr)
library(purrr)

bounds <- readRDS("bounds.rds")

bounds.corr.ch <- bounds%>%
  mutate(idx = seq(0,29,1))%>%
  as_tibble()%>%
  mutate(corr.dt.m30 = map(idx,.f=function(idx){
    list.files(path = "output/data/", pattern =  sprintf("*m30_%s.rds",idx),
               full.names = TRUE)  
  }))%>%
  mutate(corr.dt.pmm = map(idx,.f=function(idx){
    list.files(path = "output/data/", pattern =  sprintf("*pmm_%s.rds",idx),
               full.names = TRUE)  
  }))%>%
  mutate(corr.dt.pp = map(idx,.f=function(idx){
    list.files(path = "output/data/", pattern =  sprintf("*pp_%s.rds",idx),
               full.names = TRUE)  
  }))%>%
  mutate(corr.m30 = map(corr.dt.m30,readRDS))

bounds.corr.mm <- bounds.corr.ch%>%
  filter(corr.dt.pmm!="character(0)")%>%
  mutate(corr.mm  = map(corr.dt.pmm,readRDS))%>%
  select(scenario.id,corr.mm)

bounds.corr.pp <- bounds.corr.ch%>%
  filter(corr.dt.pp!="character(0)")%>%
  mutate(corr.pp  = map(corr.dt.pp,readRDS))%>%
  select(scenario.id,corr.pp)

corr.ch <- bounds.corr.ch%>%
  mutate(lower.bound = pmax(rho.bound.H0.l,rho.bound.H0.l),
         upper.bound = pmin(rho.bound.H0.u,rho.bound.H0.u))%>%
  select(scenario.id, lower.bound, upper.bound,corr.m30)

corr.ch <- corr.ch%>%
  left_join(bounds.corr.mm, by = "scenario.id")%>%
  left_join(bounds.corr.pp, by = "scenario.id")

saveRDS(corr.ch, "Simchecks/corrwalddiff.rds")


mm <- corr.ch%>%select(lower.bound,corr.mm)
pp <- corr.ch%>%select(upper.bound,corr.pp)


condor::cleanup_local(dir = 'output',tag = 'wald')

