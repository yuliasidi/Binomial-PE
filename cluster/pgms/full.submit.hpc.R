s <- ssh::ssh_connect( Sys.getenv('UCONN_USER_HPC'))

rfile.sub <- 'test.R'

tmpl <- whisker::whisker.render(template = readLines('cluster/pgms/tmpls/full.slurm.tmpl'), 
                                data = list(rfile = rfile.sub,
                                            n = 5,
                                            time = 5))
cat(tmpl, '\n', file ="cluster/pgms/full.slurm")



ssh::scp_upload(s,c(sprintf('cluster/pgms/wald/%s', rfile.sub),'cluster/pgms/full.slurm'))
condor::ssh_fn(s,'sbatch full.slurm')
