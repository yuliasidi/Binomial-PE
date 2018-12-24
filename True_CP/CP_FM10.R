#!/home/statsadmin/R/bin/Rscript
library(dplyr, warn.conflicts = F, quietly = T)
library(purrr, warn.conflicts = F, quietly = T)

args <- commandArgs()
idx <- as.numeric(args[length(args)])+1
.libPaths('ysidi/lib')

bineval <- readRDS("bineval.10.rds")

bineval.idx <- bineval%>%
  dplyr::slice(idx)%>%
  select(-n)

cp_2prop.fm <- function(n1,p1,n2,p2,M2,alpha){
  dt.bin <- data.frame(expand.grid(x = seq(0,n1,1),
                                   y = seq(0,n2,1)))
  dt.bin <- dt.bin%>%
    mutate(x.pmf = dbinom(x,n1,p1),
           y.pmf = dbinom(y,n2,p2),
           pmf   = x.pmf * y.pmf,
           a = 2, 
           b = -1*(2 + x/n1 + y/n2 + M2*3), 
           c = M2^2 + M2*(2*x/n1+2) + x/n1 + y/n2,
           d = -x/n1*M2*(1+M2))%>%
    mutate(
      v = b^3/(27*a^3)-b*c/(6*a^2)+d/(2*a),
      sv = sign(v),
      #sv = ifelse(sv==0,1,sv),
      u = sv*(b^2/(9*a^2)-c/(3*a))^0.5,
      #u1 = ifelse(u==0,0.0001,u),
      w_val = v/u^3
    )%>%
    mutate(w_val = case_when(w_val  >= 1 ~ as.numeric(1),
                             w_val <= (-1) ~ as.numeric(-1),
                             TRUE ~ as.numeric(w_val)))
  
  # if(any(is.na(dt.bin$w_flag)))
  #   message(sprintf('rows are NA:\n%s',
  #                   paste0(which(is.na(dt.bin$w_flag)),
  #                          collapse = ', ')
  #                   )
  #   )
  
  dt.bin <- dt.bin%>%
    #dplyr::filter(is.na(w_flag)=="FALSE")%>%
    mutate(
      w = 1/3*(pi+acos(w_val))
    )%>%
    mutate(p1.rmle = 2*u*cos(w)-b/(3*a),
           p2.rmle = p1.rmle-M2)%>%
    select(-a, -b, -c, -d, -v, -u, -w)%>%
    mutate(ci.l = x/n1-y/n2 + 
             qnorm(alpha)*sqrt(p1.rmle*(1-p1.rmle)/n1 + p2.rmle*(1-p2.rmle)/n2),
           ci.u = x/n1-y/n2 + 
             qnorm(1-alpha)*sqrt(p1.rmle*(1-p1.rmle)/n1 + p2.rmle*(1-p2.rmle)/n2),
           p = p1-p2,
           I = ifelse(p >= ci.l & p <= ci.u,1,0))
  cp <- as.numeric(dt.bin%>%
                     filter(I==1)%>%
                     summarise(cp=sum(pmf)))
  return(cp)  
}


bineval.idx.cp <- bind_cols(bineval.idx, cp = pmap_dbl(bineval.idx,
                                                       cp_2prop.fm, alpha=0.025))


saveRDS(bineval.idx.cp, file = sprintf('binevalcp_fm10_%02d.rds',idx))

