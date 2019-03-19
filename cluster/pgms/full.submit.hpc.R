s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))

rfile.sub <- 'fullH0_17_do10_set1.R'

tmpl <- whisker::whisker.render(template = readLines('cluster/pgms/tmpls/full.slurm.tmpl'), 
                                data = list(rfile = rfile.sub,
                                            n = 100,
                                            time = 180))
cat(tmpl, '\n', file ="cluster/pgms/ffll.slurm")



ssh::scp_upload(s,c(sprintf('cluster/pgms/wald/setn/%s', rfile.sub),'cluster/pgms/full.slurm'))
condor::ssh_fn(s,'sbatch full.slurm')
