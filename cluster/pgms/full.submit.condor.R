
session <- ssh::ssh_connect(Sys.getenv('UCONN_USER'))

rfile.sub <- 'fullH0set1_15_do10.R'

tmpl <- whisker::whisker.render(template = readLines('cluster/pgms/full.condor.tmpl'), 
                                data = list(rfile = rfile.sub,
                                            njobs = 5000))
cat(tmpl, '\n', file ="cluster/pgms/full.condor")


ssh::scp_upload(session,
                files = c(sprintf('cluster/pgms/fm/%s', rfile.sub) ,'cluster/pgms/full.condor'),
                to = '~')



condor::condor_submit(session,'full.condor',notify=FALSE)  


