library(condor)
s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))

create_slurm(job = 'fmtest',
             time = 720,
             script = 'miss_fm_hpc.R',ntasks = 190)%>%
  cat(file = 'submit.slurm',sep='\n')

ssh::scp_upload(s,c('miss_fm_hpc.R','submit.slurm'))
condor::ssh_fn(s,'sbatch submit.slurm')

condor::ssh_fn(s,'cat results.log')
condor::ssh_fn(s,'rm results/results.log')

condor::ssh_fn(s,'ls')


ssh::ssh_disconnect(s)
