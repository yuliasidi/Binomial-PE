#!/home/statsadmin/R/bin/Rscript


##################################################
#### Functions for PE Binomial PE Simulations ####
##################################################

# Calculates RMLE for phats- FM method
p.rmle.fm <- function(df, M2=M2){
  df%>%
    mutate(a = 2, 
           b = -1*(2 + p_C + p_T + M2*3), 
           c = M2^2 + M2*(2*p_C+2) + p_C + p_T,
           d = -p_C*M2*(1+M2))%>%
    mutate(v = b^3/(27*a^3)-b*c/(6*a^2)+d/(2*a),
           u = sign(v)*(b^2/(9*a^2)-c/(3*a))^0.5,
           w = 1/3*(pi+acos(v/u^3)))%>%
    mutate(p_C.rmle = 2*u*cos(w)-b/(3*a),
           p_T.rmle = p_C.rmle-M2)%>%
    select(-a, -b, -c, -d, -v, -u, -w)
}

# Simulates the desired binomial proportions
ni.d <- function(N_T,N_C,p_T,p_C){
  data_frame(
    pat_id = seq(1,N_T+N_C,1),
    y = c(rbinom(N_T,1,p_T),rbinom(N_C,1,p_C)),
    trt = rep(c('T','C'),c(N_T,N_C))
  )
  
}

# Produces point estimate and Wald CI for difference in proportions & test H
Wald.CI <- function(df, M2, y){
  df%>%
    group_by(sim.id, trt)%>%
    summarise(phat = mean(y, na.rm = T), n = sum(!is.na(y)))%>%
    recast(sim.id ~ trt + variable, measure.var = c("phat",'n'))%>%
    mutate(phat.d = C_phat-T_phat, 
           ci.l = C_phat-T_phat - qnorm(1-alpha)*sqrt(C_phat*(1-C_phat)/(C_n)+
                                                        T_phat*(1-T_phat)/(T_n)),
           ci.u = C_phat-T_phat + qnorm(1-alpha)*sqrt(C_phat*(1-C_phat)/(C_n)+
                                                        T_phat*(1-T_phat)/(T_n)),
           reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
}

# Produces point estimate and FM CI for difference in proportions & test H
FM.CI <- function(df, M2, y){
  df%>%
    group_by(sim.id, trt)%>%
    summarise(phat = mean(y, na.rm = T), n = sum(!is.na(y)))%>%
    recast(sim.id ~ trt + variable, measure.var = c("phat",'n'))%>%
    rename(p_T = T_phat, p_C = C_phat)%>%
    p.rmle.fm(M2 = M2)%>%
    mutate(phat.d = p_C-p_T, 
           ci.l = phat.d - qnorm(1-alpha)*sqrt(p_C.rmle*(1-p_C.rmle)/C_n+
                                                 p_T.rmle*(1-p_T.rmle)/T_n),
           ci.u = phat.d + qnorm(1-alpha)*sqrt(p_C.rmle*(1-p_C.rmle)/C_n+
                                                 p_T.rmle*(1-p_T.rmle)/T_n),
           reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
}

# Summarizes number of rejections of H0
reject.H0 <- function(df){
  df%>%summarise(sum=sum(reject.h0)/n())
}


# Calculated Wilson-Newcombe CI
wn.CI <- function(df, M2, y){
  
  df%>%
    group_by(sim.id, trt)%>%
    summarise(phat = mean(y, na.rm = T), n = sum(!is.na(y)))%>%
    recast(sim.id ~ trt + variable, measure.var = c("phat",'n'))%>%
    dplyr::mutate(z = qnorm(1-alpha),
                  l.C = (C_phat + z^2/(2*C_n) - 
                           z*sqrt((C_phat*(1-C_phat)+z^2/(4*C_n))/C_n))/(1+z^2/C_n),
                  u.C = (C_phat + z^2/(2*C_n) + 
                           z*sqrt((C_phat*(1-C_phat)+z^2/(4*C_n))/C_n))/(1+z^2/C_n),
                  
                  l.T = (T_phat + z^2/(2*T_n) - 
                           z*sqrt((T_phat*(1-T_phat)+z^2/(4*T_n))/T_n))/(1+z^2/T_n),
                  u.T = (T_phat + z^2/(2*T_n) + 
                           z*sqrt((T_phat*(1-T_phat)+z^2/(4*T_n))/T_n))/(1+z^2/T_n),
                  
                  ci.l = C_phat-T_phat-sqrt((C_phat-l.C)^2+(u.T-T_phat)^2),
                  ci.u = C_phat-T_phat+sqrt((u.C-C_phat)^2+(T_phat-l.T)^2),
                  reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
}


# Samples binomial outcome for WN SS calculation
sample_wn <- function(wn, n.sim.ss){
  
  wn%>%
    dplyr::mutate(
      N_C = N_T,
      t.WN=purrr::pmap(list(N_T,N_C,p_T,p_C),
                       .f = function(..., n.sim.ss){
                         purrr::rerun(n.sim.ss,ni.d(...))%>%
                           dplyr::bind_rows(.id = "sim.id")
                       },n.sim.ss = n.sim.ss)
    ) 
}

# Calculates power for WN (used in WN SS calculation)
power_wn <- function(wn){
  
  wn%>%
    mutate(
      t.wn.CI = pmap(list(t.WN, N_T = N_T, N_C = N_C, M2 = M2, alpha = alpha), wn.CI),
      power.WN = map(t.wn.CI, reject.H0),
      power.WN = map_dbl(power.WN,as.numeric),
      power.est = round(power.WN,2)
    )
  
} 

# Recourse for calculating SS for WN
recourse_wn <- function(wn,n.sim.ss,store = NULL){
  
  cat('\nsampling...\n')
  
  x <- wn%>%
    sample_wn(n.sim.ss = n.sim.ss)%>%
    power_wn()  
  
  x1 <- x%>%
    dplyr::mutate(
      flag = dplyr::case_when(
        power.est-power>0 ~ 'decrease',
        power.est-power<0 ~ 'increase',
        TRUE ~ 'drop')
    )
  
  store <- dplyr::bind_rows(
    store,
    x1%>%dplyr::filter(flag=='drop')
  )
  
  x2 <- x1%>%
    dplyr::filter(flag!='drop')
  
  cat(sprintf('nrows left: %s\n',nrow(x2)))
  
  if(nrow(x2)>0){
    
    x2 <- x2%>%
      dplyr::mutate(
        N_T=ceiling(dplyr::if_else(flag=='decrease',N_T*0.99,N_T*1.01))
      )
    
    recourse_wn(x2,n.sim.ss,store)
    
  }else{
    
    return(store)
    
  }
  
}

# Imposting MCAR values for DO of 10%, 15%, and 20%
mcar.fun <- function(df){
  
  df%>%
    group_by(sim.id)%>%
    mutate(R.mcar.do10 = as.numeric(pat_id%in%sample(n()*0.1)),
           R.mcar.do15 = as.numeric(pat_id%in%sample(n()*0.15)),
           R.mcar.do20 = as.numeric(pat_id%in%sample(n()*0.2)),
           y.mcar.do10 = case_when(R.mcar.do10==1 ~ as.numeric(NA),
                                    TRUE ~ as.numeric(y)),
           y.mcar.do15 = case_when(R.mcar.do15==1 ~ as.numeric(NA),
                                    TRUE ~ as.numeric(y)),
           y.mcar.do20 = case_when(R.mcar.do20==1 ~ as.numeric(NA),
                                    TRUE ~ as.numeric(y)))
  
}


# Calculattion for point estimates and var from GLM
glm_pdiff <- function(df, M2){
  
  m <- glm(y ~ trt, family = binomial(link = "logit"), data = df)
  sum.m <- summary.glm(m)
  
  p_T.est <- 1/(1+exp(-1*(sum.m$coefficients[1,1]+sum.m$coefficients[2,1])))
  p_C.est <- 1/(1+exp(-1*(sum.m$coefficients[1,1])))
  
  sigma.T2 <- sum.m$cov.unscaled[2,2]
  sigma.C2 <- sum.m$cov.unscaled[1,1]
  sigma.TC <- sum.m$cov.unscaled[1,2]
  
  phat.d <- p_C.est - p_T.est
  # The variance below is derived by Multivariate Delta Method- cite Reeve 2018
  sigma.diff2 <- (p_T.est*(1 - p_T.est))^2*(sigma.T2 + sigma.C2 + 2*sigma.TC) + 
    (p_C.est*(1 - p_C.est))^2*sigma.C2 - 
    2*p_C.est*(1 - p_C.est)*p_T.est*(1 - p_T.est)*(sigma.TC + sigma.C2)
  
  ci.l <- phat.d - qnorm(1-alpha)*sqrt(sigma.diff2)
  ci.u <- phat.d + qnorm(1-alpha)*sqrt(sigma.diff2)
  
  
  glm.est <- data_frame(phat.d, sigma.diff2, ci.l, ci.u)%>%
    mutate(reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
  
  #return(list("phat.d" = phat.d, "sigma.diff2" = sigma.diff2))
  return(glm.est)
}

# Sample binomial data for GLM SS calculation
sample_glm <- function(glm.dt, n.sim.ss){
  glm.dt%>%
    dplyr::mutate(
      N_C = N_T,
      t.GLM=purrr::pmap(list(N_T,N_C,p_T,p_C),
                        .f = function(..., n.sim.ss){
                          purrr::rerun(n.sim.ss,ni.d(...))%>%
                            dplyr::bind_rows(.id = "sim.id")
                        },n.sim.ss = n.sim.ss)
    ) 
}

# Calculate Power for GLM SS calculation
power_glm_SS <- function(glm.dt){
  glm.dt%>%
    dplyr::mutate(t.GLM.CI = purrr::map2(t.GLM, M2, .f = function(df, M2){
      df%>%
        tidyr::nest(-sim.id)%>%
        dplyr::mutate(x = pmap(list(data, M2 = M2), glm_pdiff))%>%
        tidyr::unnest(x)%>%
        dplyr::select(-data)
    }))%>%
    dplyr::select(-t.GLM)%>%
    dplyr::mutate(power.GLM = map(t.GLM.CI, reject.H0),
                  power.GLM = map_dbl(power.GLM,as.numeric),
                  power.est = round(power.GLM,2)
    )
  
}


# Recoursive function for GLM SS calculation 
recourse_glm <- function(glm.dt,n.sim.ss,store = NULL){
  
  cat('\nsampling...\n')
  
  x <- glm.dt%>%
    sample_glm(n.sim.ss = n.sim.ss)%>%
    power_glm_SS()  
  
  
  x1 <- x%>%
    dplyr::mutate(
      flag = dplyr::case_when(
        power.est-power>0 ~ 'decrease',
        power.est-power<0 ~ 'increase',
        TRUE ~ 'drop')
    )
  
  store <- dplyr::bind_rows(
    store,
    x1%>%dplyr::filter(flag=='drop')
  )
  
  
  x2 <- x1%>%
    dplyr::filter(flag!='drop')
  
  cat(sprintf('nrows left: %s\n',nrow(x2)))
  
  if(nrow(x2)>0){
    
    x2 <- x2%>%
      dplyr::mutate(
        N_T=ceiling(dplyr::if_else(flag=='decrease',N_T*0.99,N_T*1.01))
      )
    
    
    recourse_glm(x2,n.sim.ss,store)
    
  }else{
    
    return(store)
    
  }
  
}

# Calculate type-I error for GLM
type1_glm <- function(df){
  df%>%
    dplyr::mutate(GLM.H0 = purrr::map2(t.GLM.H0, M2, .f = function(df, M2){
      df%>%
        tidyr::nest(-sim.id)%>%
        dplyr::mutate(x = pmap(list(data, M2 = M2), glm_pdiff))%>%
        tidyr::unnest(x)%>%
        dplyr::select(-data)
    }))%>%
    #dplyr::select(-t.GLM)%>%
    dplyr::mutate(type1.GLM = map(GLM.H0, reject.H0),
                  type1.GLM = map_dbl(type1.GLM,as.numeric)
    )
  
}

# Calculate power for GLM
power_glm <- function(df){
  df%>%
    dplyr::mutate(GLM.H1 = purrr::map2(t.GLM.H1, M2, .f = function(df, M2){
      df%>%
        tidyr::nest(-sim.id)%>%
        dplyr::mutate(x = pmap(list(data, M2 = M2), glm_pdiff))%>%
        tidyr::unnest(x)%>%
        dplyr::select(-data)
    }))%>%
    #dplyr::select(-t.GLM)%>%
    dplyr::mutate(power.GLM = map(GLM.H1, reject.H0),
                  power.GLM = map_dbl(power.GLM,as.numeric)
    )
  
}

# Adds continutios variable X from N(4,1), such that the correlation is equal to rho
add.X <- function(df, rho=0.3, rho.bound){
  df.C <- df%>%
    filter(trt=="C")%>%
    mutate(X = rnorm(length(trt),4,1))%>%
    mutate(X = case_when(X<0 ~ 0, 
                         X>10 ~ 10, 
                         TRUE ~ as.numeric(X)))
  df.T <- df%>%
    filter(trt=="T")%>%
    mutate(X = rnorm(length(trt),4,1))%>%
    mutate(X = case_when(X<0 ~ 0, 
                         X>10 ~ 10, 
                         TRUE ~ as.numeric(X)))
  
  t.order.C <- df.C%>%
    slice(1:floor(abs(rho)*n()/abs(rho.bound)))
  t.asis.C  <- df.C%>%
    slice(floor(abs(rho)*n()/abs(rho.bound))+1:n())
  
  t.order.T <- df.T%>%
    slice(1:floor(abs(rho)*n()/abs(rho.bound)))
  t.asis.T  <- df.T%>%
    slice(floor(abs(rho)*n()/abs(rho.bound))+1:n())
  
  if (rho.bound>0){
    t.ordered.C <- bind_cols(t.order.C%>%select(-X)%>%arrange(y), 
                             t.order.C%>%select(X)%>%arrange(X))
    t.ordered.T <- bind_cols(t.order.T%>%select(-X)%>%arrange(y), 
                             t.order.T%>%select(X)%>%arrange(X))
  } 
  
  if (rho.bound<0){
    t.ordered.C <- bind_cols(t.order.C%>%select(-X)%>%arrange(desc(y)), 
                             t.order.C%>%select(-y)%>%arrange(X)%>%select(X))
    t.ordered.T <- bind_cols(t.order.T%>%select(-X)%>%arrange(desc(y)), 
                             t.order.T%>%select(-y)%>%arrange(X)%>%select(X))
  }
  
  df3 <- bind_rows(t.ordered.T, t.ordered.C, t.asis.T, t.asis.C)
  
  return(df3)
}

# Function that checks that simulated p_C/p_T is according to what is specified for a relevant scenario
check_p <- function(df, p_C, p_T, M2){
  df%>%
    dplyr::group_by(sim.id, trt)%>%
    dplyr::summarise(p = round(mean(y),2))%>%
    dplyr::ungroup()%>%
    reshape2::dcast(sim.id ~ trt, value.var = "p")%>%
    dplyr::mutate(c.p_C = p_C - C, c.p_T = p_T - T - M2)%>%
    dplyr::summarise(mean.c.p_C = round(mean(c.p_C),2), mean.c.p_T = round(mean(c.p_T),2), 
                     sd.c.p_C = round(sqrt(var(c.p_C)),2), sd.c.p_T = round(sqrt(var(c.p_T)),2))
}

# Function that checks simulated correlation

check_rho <- function(df){
  df%>%
    tidyr::nest(-sim.id)%>%
    dplyr::mutate(cor = map(data , .f = function(df){
      cor(df$y,df$X, method = "spearman")
    }))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::summarise(mean.cor = round(mean(cor),4), sd = round(sqrt(var(cor)),4))
}

# Function which imposes MAR by X (BL diesase status available for all the patients)
mar.fun <- function(df){
  
  df%>%
    group_by(sim.id, trt)%>%
    arrange(sim.id, trt, X)%>%
    mutate(nrow=1:n(),
           R.mar.do10 = as.numeric(floor(n()*(1-0.1))<nrow),
           R.mar.do15 = as.numeric(floor(n()*(1-0.15))<nrow),
           R.mar.do20 = as.numeric(floor(n()*(1-0.2))<nrow),
           y.mar.do10 = case_when(R.mar.do10==1 ~ as.numeric(NA),
                                  TRUE ~ as.numeric(y)),
           y.mar.do15 = case_when(R.mar.do15==1 ~ as.numeric(NA),
                                  TRUE ~ as.numeric(y)),
           y.mar.do20 = case_when(R.mar.do20==1 ~ as.numeric(NA),
                                  TRUE ~ as.numeric(y)))
  
  
}


# Function that checks %DO
check_mar <- function(df){
  
  df%>%
    dplyr::group_by(sim.id, trt)%>%
    dplyr::summarise(c.do10 = round(mean(R.mar.do10), 2),
                     c.do15 = round(mean(R.mar.do15), 2),
                     c.do20 = round(mean(R.mar.do20), 2))%>%
    dplyr::ungroup()%>%
    dplyr::summarise(cc.do10 = mean(c.do10), cc.do15 = mean(c.do15), cc.do20 = mean(c.do20))
}

# Function that imposed MNAR
mnar.fun <- function(df){
  
  df%>%
    group_by(sim.id, trt)%>%
    arrange(sim.id, trt, y)%>%
    mutate(nrow=1:n(),
           R.mnar.do10 = as.numeric(floor(n()*(1-0.1))<nrow),
           R.mnar.do15 = as.numeric(floor(n()*(1-0.15))<nrow),
           R.mnar.do20 = as.numeric(floor(n()*(1-0.2))<nrow),
           y.mnar.do10 = case_when(R.mnar.do10==1 ~ as.numeric(NA),
                                   TRUE ~ as.numeric(y)),
           y.mnar.do15 = case_when(R.mnar.do15==1 ~ as.numeric(NA),
                                   TRUE ~ as.numeric(y)),
           y.mnar.do20 = case_when(R.mnar.do20==1 ~ as.numeric(NA),
                                   TRUE ~ as.numeric(y)))
  
  
}

#model that generates missing data
miss.fun <- function(df, b.trt = log(1), b.y = log(1), b.X = log(1), do = 0.1){
  tmp <- df%>%
    dplyr::mutate(trtn = case_when(trt=='T' ~ 1, 
                                   TRUE ~ 0),
                  b.trt = b.trt,
                  b.y = b.y,
                  b.X = b.X)
  tmp1 <- tmp%>%
    dplyr::group_by(sim.id)%>%
    dplyr::summarise(etrt = mean(trtn),
                     eX = mean(X)/10,
                     ey = mean(y))%>%
    dplyr::mutate(int = -log(1/do-1) - b.trt*etrt - b.y*ey - b.X*eX)
  tmp2 <- dplyr::left_join(tmp, tmp1, by="sim.id")%>%
    dplyr::mutate(p = 1/(1+exp(-1*(int + b.trt*trtn + b.y*y + b.X*X/10))))
  
  sampl.miss <- tmp2%>%
    group_by(sim.id)%>%
    sample_frac(do, weight = p)%>%
    mutate(r = 1)%>%
    select(sim.id, pat_id, r)
  
  out <- dplyr::left_join(tmp2,sampl.miss, by =c('sim.id','pat_id'))%>%
    dplyr::mutate(r = ifelse(is.na(r),0,r),
                  y.m = ifelse(r==1,as.numeric(NA),y))#%>%
   #dplyr::select(-y, -etrt, -eX, -ey, -b.trt, -b.y, -b.X, -int, -trtn)
  
  return(out)
}

#check for amount of missing data per simulation
check.miss <- function(df, do){
  df%>%
    group_by(sim.id)%>%
    summarise(rm = round(mean(r),2) - do)%>%
    ungroup(sim.id)%>%
    summarise(r.all.check = sum(rm))
}

#function that applied the missingness model
miss.apply.do <- function(df, b.trt=log(1), b.y=log(1), b.X=log(1), do=0.1){
  df%>%
    dplyr::mutate(t.H0.m = purrr::pmap(list(t.H0,b.trt, b.y, b.X, do = do), 
                                        miss.fun),
                  t.H1.m = purrr::pmap(list(t.H1,b.trt, b.y, b.X, do = do), 
                                       miss.fun))%>%
    dplyr::select(scenario.id, M2, p_T, t.H0.m, t.H1.m)
} 

#check missing mechanism
check.mech <-function(df){
  df%>%
    dplyr::mutate(mech.H0.m5  = purrr::map(t.H0.m5 , check.mech.p),
                  mech.H0.m10 = purrr::map(t.H0.m10, check.mech.p),
                  mech.H0.m15 = purrr::map(t.H0.m15, check.mech.p),
                  mech.H0.m20 = purrr::map(t.H0.m20, check.mech.p),
                  mech.H0.m25 = purrr::map(t.H0.m25, check.mech.p),
                  mech.H1.m5  = purrr::map(t.H1.m5 , check.mech.p),
                  mech.H1.m10 = purrr::map(t.H1.m10, check.mech.p),
                  mech.H1.m15 = purrr::map(t.H1.m15, check.mech.p),
                  mech.H1.m20 = purrr::map(t.H1.m20, check.mech.p),
                  mech.H1.m25 = purrr::map(t.H1.m25, check.mech.p))%>%
    dplyr::select(scenario.id, mech.H0.m5, mech.H0.m10, mech.H0.m15, mech.H0.m20, mech.H0.m25,
                  mech.H1.m5, mech.H1.m10, mech.H1.m15, mech.H1.m20, mech.H1.m25) 
}


check.mech.p <- function(df){
  df%>%
    dplyr::mutate(rp = round(p,2))%>%
    dplyr::group_by(sim.id, rp)%>%
    dplyr::summarise(rm = mean(r))%>%
    dplyr::ungroup(sim.id, rp)%>%
    dplyr::filter(sim.id==sample(n.sim,1)|sim.id==sample(n.sim,1))%>%
    dplyr::select(sim.id, rp, rm)
}

#read all the files that check do rates in the incomplete data
read_doch <- function(TYPE, MISSING, PERCENT){
  purrr::map_df(
    list.files(path = "output/data", 
               pattern = sprintf("%sdoch%s%s", TYPE, MISSING, PERCENT),  full.names = TRUE),
    readRDS)%>%
    dplyr::mutate(TYPE=sprintf('%s', TYPE), 
                  MISSING = sprintf('%s', MISSING), 
                  PERCENT = sprintf('%s', PERCENT))
} 

#read all the files that check missing mechanisms in the incomplete data
read_mech <- function(TYPE, MISSING, PERCENT){
  purrr::map_df(
    list.files(path = "output/data", 
               pattern = sprintf("mech%s%s%s", MISSING, TYPE, PERCENT),  full.names = TRUE),
    readRDS)%>%
    dplyr::mutate(TYPE=sprintf('%s', TYPE), 
                  MISSING = sprintf('%s', MISSING), 
                  PERCENT = sprintf('%s', PERCENT))
} 


#Calculate relative bias and take a mean over the n simulations
bias.fun <- function(df, M2, y){
  df%>%
    group_by(sim.id, trt)%>%
    summarise(phat = mean(y, na.rm = T), n = sum(!is.na(y)))%>%
    recast(sim.id ~ trt + variable, measure.var = c("phat",'n'))%>%
    mutate(bias = (C_phat-T_phat-M2)/M2)%>%
    dplyr::select(sim.id, bias)%>%
    dplyr::ungroup(sim.id, trt)%>%
    summarise(bias.m = mean(bias))
}

#read all the files that check do rates in the incomplete data
read_anal <- function(ANAL,TYPE, MISSING, PERCENT){
  purrr::map_df(
    list.files(path = "output/data", 
               pattern = sprintf("%s%s%s%s", ANAL, TYPE, MISSING, PERCENT),  full.names = TRUE),
    readRDS)
  #%>%
  #  dplyr::mutate(TYPE=sprintf('%s', TYPE), 
  #                MISSING = sprintf('%s', MISSING), 
  #                PERCENT = sprintf('%s', PERCENT))
} 

read_analnew <- function(ANAL,TYPE, MISSING, PERCENT){
  purrr::map_df(
    list.files(path = "output/data", 
               pattern = sprintf("%s%s%snew%s", ANAL, MISSING, TYPE, PERCENT),  full.names = TRUE),
    readRDS)%>%
    dplyr::mutate(TYPE=sprintf('%s', TYPE), 
                  MISSING = sprintf('%s', MISSING), 
                  PERCENT = sprintf('%s', PERCENT))
} 


#Plot type-I errors for CCA
type1.cca.plot <- function(df, type = 'wald', missing = 'mcar', title = 'Wald, MCAR', ylim = c(0,0.05)){
  type1.g <- df%>%
    dplyr::filter(TYPE== type, MISSING == missing)%>%
    dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_C,M2))%>%
    ggplot2::ggplot(aes(PERCENT, type1)) +
    geom_segment(aes(x = PERCENT, y = type1full, xend = PERCENT, yend = type1), colour="red") +
    geom_point(size=0.5) +
    geom_hline(yintercept = c(0.9*alpha, 1.1*alpha), linetype=2) +
    scale_y_continuous(limits = ylim) +
    facet_wrap(~ scenario.idc) +
    theme_bw() +
    xlab("% of Missing Observations")+
    ylab("Type-I error")+
    ggtitle(sprintf("Type I Error: %s, CCA Analysis",title))
  
  return(type1.g)
} 


#Plot relative bias for CCA
bias.cca.plot <- function(df, type = 'wald', missing = 'mcar', title = 'Wald, MCAR', ylim = c(-.02,0.02)){
  bias.g <- df%>%
    dplyr::filter(TYPE== type, MISSING == missing)%>%
    dplyr::mutate(scenario.idc = sprintf('p=%s & M=%s',p_T,M2))%>%
    ggplot2::ggplot(aes(PERCENT, bias)) +
    geom_segment(aes(x = PERCENT, y = 0, xend = PERCENT, yend = bias)) +
    geom_point(size=0.5) +
    scale_y_continuous(limits = ylim) +
    facet_wrap(~ scenario.idc) +
    theme_bw() +
    xlab("% of Missing Observations")+
    ylab("Relative bias")+
    ggtitle(sprintf("Relative bias: %s, CCA Analysis",title))

  return(bias.g)
} 

#generate new df with added X acc to specified rho
add.X.rho <- function(df, r){
  df%>%
    mutate(t.H1.X = map(t.H1, .f = function(df, rho = r){
      df%>%nest(-sim.id)%>%
        mutate(t.X = map(data, add.X, rho=r, 
                         rho.bound=ifelse(r>0,rho.bound.H1.u,rho.bound.H1.l)))%>%
        select(-data)%>%
        unnest()
    }))%>%
    select(-t.H1)%>%
    rename(t.H1 = t.H1.X)%>%
    mutate(t.H0.X = map(t.H0, .f = function(df, rho = r){
      df%>%nest(-sim.id)%>%
        mutate(t.X = map(data,add.X, rho=r,
                         rho.bound=ifelse(r>0,rho.bound.H1.u,rho.bound.H1.l)))%>%
        select(-data)%>%
        unnest()
    }))%>%
    select(-t.H0)%>%
    rename(t.H0 = t.H0.X)
}

#lower correlation bound calcualtion
bound.l <- function(df){
  df.T <- df%>%
    filter(trt=='T')
  df.C <- df%>%
    filter(trt=='C')
  df.sort.T <-bind_cols(df.T%>%select(-X)%>%arrange(desc(y)), 
                        df.T%>%select(X)%>%arrange(X))
  df.sort.C <-bind_cols(df.C%>%select(-X)%>%arrange(desc(y)), 
                        df.C%>%select(X)%>%arrange(X))
  df.sort <- bind_rows(df.sort.T, df.sort.C)
  rho.bound <-cor(df.sort$y, df.sort$X, method = "spearman")
}

#upper correlation bound calcualtion
bound.u <- function(df){
  df.T <- df%>%
    filter(trt=='T')
  df.C <- df%>%
    filter(trt=='C')
  df.sort.T <-bind_cols(df.T%>%select(-X)%>%arrange(y), 
                        df.T%>%select(X)%>%arrange(X))
  df.sort.C <-bind_cols(df.C%>%select(-X)%>%arrange(y), 
                        df.C%>%select(X)%>%arrange(X))
  df.sort <- bind_rows(df.sort.T, df.sort.C)
  rho.bound <-cor(df.sort$y, df.sort$X, method = "spearman")
}

dt.miss.check <-function(df,do){
  df%>%
    dplyr::mutate(do.H0 = purrr::map2(t.H0.m, do, check.miss),
                  do.H0 = purrr::map_dbl(do.H0, as.numeric),
                  do.H1 = purrr::map2(t.H1.m, do, check.miss),
                  do.H1 = purrr::map_dbl(do.H1, as.numeric))%>%
    dplyr::select(-t.H0.m, -t.H1.m)} 

cca.wald.anal <- function(df){
  df1 <- df%>%
    dplyr::mutate(t.H0.m1 = map(t.H0.m, .f=function(df){
      df%>%
        dplyr::select(-y)%>%
        dplyr::rename(y=y.m)
    }),
    t.H1.m1 = map(t.H1.m, .f=function(df){
      df%>%
        dplyr::select(-y)%>%
        dplyr::rename(y=y.m)
    }))%>%
    dplyr::select(-t.H0.m,-t.H1.m)%>%
    dplyr::rename(t.H0.m = t.H0.m1, t.H1.m = t.H1.m1)
  
  df2 <- df1%>%
    dplyr::mutate(t.H0.CI = pmap(list(df=t.H0.m, M2 =  M2), Wald.CI),
                  t.H1.CI = pmap(list(df=t.H1.m, M2 =  M2), Wald.CI),
                  t.H0.bias = pmap(list(t.H0.m, M2 = M2), bias.fun))
  
  df2 <- df2%>%
    mutate(type1 = map(t.H0.CI, reject.H0),
           power = map(t.H1.CI, reject.H0))
  
  df3 <- df2%>%
    select(scenario.id, p_T, M2, type1, power, t.H0.bias)%>%
    mutate(type1 = map_dbl(type1, as.numeric),
           power = map_dbl(power, as.numeric),
           bias  = map_dbl(t.H0.bias, as.numeric))%>%
    dplyr::select(-t.H0.bias)
  
  return(df3)
  
}

worst.wald.anal <- function(df){
  df1 <- df%>%
    dplyr::mutate(t.H0.m1 = map(t.H0.m, .f=function(df){
      df%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,0))%>%
        dplyr::select(-y.m)
    }),
    t.H1.m1 = map(t.H1.m, .f=function(df){
      df%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,0))%>%
        dplyr::select(-y.m)
    }))%>%
    dplyr::select(-t.H0.m,-t.H1.m)%>%
    dplyr::rename(t.H0.m = t.H0.m1, t.H1.m = t.H1.m1)
  
  df2 <- df1%>%
    dplyr::mutate(t.H0.CI = pmap(list(df=t.H0.m, M2 =  M2), Wald.CI),
                  t.H1.CI = pmap(list(df=t.H1.m, M2 =  M2), Wald.CI),
                  t.H0.bias = pmap(list(t.H0.m, M2 = M2), bias.fun))
  
  df2 <- df2%>%
    mutate(type1 = map(t.H0.CI, reject.H0),
           power = map(t.H1.CI, reject.H0))
  
  df3 <- df2%>%
    select(scenario.id, p_T, M2, type1, power, t.H0.bias)%>%
    mutate(type1 = map_dbl(type1, as.numeric),
           power = map_dbl(power, as.numeric),
           bias  = map_dbl(t.H0.bias, as.numeric))%>%
    dplyr::select(-t.H0.bias)
  
  return(df3)
  
}

best.wald.anal <- function(df){
  df1 <- df%>%
    dplyr::mutate(t.H0.m1 = map(t.H0.m, .f=function(df){
      df%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,1))%>%
        dplyr::select(-y.m)
    }),
    t.H1.m1 = map(t.H1.m, .f=function(df){
      df%>%
        dplyr::mutate(y=ifelse(is.na(y.m)=="FALSE",y.m,1))%>%
        dplyr::select(-y.m)
    }))%>%
    dplyr::select(-t.H0.m,-t.H1.m)%>%
    dplyr::rename(t.H0.m = t.H0.m1, t.H1.m = t.H1.m1)
  
  df2 <- df1%>%
    dplyr::mutate(t.H0.CI = pmap(list(df=t.H0.m, M2 =  M2), Wald.CI),
                  t.H1.CI = pmap(list(df=t.H1.m, M2 =  M2), Wald.CI),
                  t.H0.bias = pmap(list(t.H0.m, M2 = M2), bias.fun))
  
  df2 <- df2%>%
    mutate(type1 = map(t.H0.CI, reject.H0),
           power = map(t.H1.CI, reject.H0))
  
  df3 <- df2%>%
    select(scenario.id, p_T, M2, type1, power, t.H0.bias)%>%
    mutate(type1 = map_dbl(type1, as.numeric),
           power = map_dbl(power, as.numeric),
           bias  = map_dbl(t.H0.bias, as.numeric))%>%
    dplyr::select(-t.H0.bias)
  
  return(df3)
  
}

#Run CCA on different datasets with mar
cca.mar.apply <- function(mar1, pm, cor){
  dt.mar5   <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar5waldx%s%d_%d.rds' ,mar1,pm,cor,idx))
  dt.mar10  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar10waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar15  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar15waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar20  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar20waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar25  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar25waldx%s%d_%d.rds',mar1,pm,cor,idx))
  
  cca.X<- cca.wald.anal(dt.mar5)%>%
    mutate(DO=0.05)%>%
    rbind(cca.wald.anal(dt.mar10)%>%
            mutate(DO=0.10),
          cca.wald.anal(dt.mar15)%>%
            mutate(DO=0.15),
          cca.wald.anal(dt.mar20)%>%
            mutate(DO=0.20),
          cca.wald.anal(dt.mar25)%>%
            mutate(DO=0.25))
  return(cca.X)
}

#Run best case scenario imputation on different datasets with mar
best.mar.apply <- function(mar1, pm, cor){
  dt.mar5   <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar5waldx%s%d_%d.rds' ,mar1,pm,cor,idx))
  dt.mar10  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar10waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar15  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar15waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar20  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar20waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar25  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar25waldx%s%d_%d.rds',mar1,pm,cor,idx))
  
  best.X<- best.wald.anal(dt.mar5)%>%
    mutate(DO=0.05)%>%
    rbind(best.wald.anal(dt.mar10)%>%
            mutate(DO=0.10),
          best.wald.anal(dt.mar15)%>%
            mutate(DO=0.15),
          best.wald.anal(dt.mar20)%>%
            mutate(DO=0.20),
          best.wald.anal(dt.mar25)%>%
            mutate(DO=0.25))
  return(best.X)
}

#Run best case scenario imputation on different datasets with mar
worst.mar.apply <- function(mar1, pm, cor){
  dt.mar5   <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar5waldx%s%d_%d.rds' ,mar1,pm,cor,idx))
  dt.mar10  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar10waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar15  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar15waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar20  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar20waldx%s%d_%d.rds',mar1,pm,cor,idx))
  dt.mar25  <- readRDS(file = sprintf('scenario23/dtMAR/dt%dmar25waldx%s%d_%d.rds',mar1,pm,cor,idx))
  
  worst.X<- worst.wald.anal(dt.mar5)%>%
    mutate(DO=0.05)%>%
    rbind(worst.wald.anal(dt.mar10)%>%
            mutate(DO=0.10),
          worst.wald.anal(dt.mar15)%>%
            mutate(DO=0.15),
          worst.wald.anal(dt.mar20)%>%
            mutate(DO=0.20),
          worst.wald.anal(dt.mar25)%>%
            mutate(DO=0.25))
  return(worst.X)
}

#Type-I error plot for different X/missingness
plot.type1 <- function(df,title){
  type1.g <- df%>%
    tidyr::unnest(eval)%>%
    dplyr::mutate(
      cor=cor/100,
      pm=factor(pm,labels=c('Negative','Positive')),
      beta=sprintf('beta[T]=%s,beta[X]=%s',b.trt,b.X)
    )%>%
    ggplot(aes(x=DO,y=type1,colour=beta, shape=beta)) + 
    geom_point() + 
    geom_hline(yintercept=c(0.9,1.1)*alpha,
               linetype=2) + 
    facet_grid(pm~cor) +
    theme(legend.position = 'bottom') +
    labs(x='Dropout Rate',y='Type-I Error') +
    ggtitle(sprintf('%s',title))
  
  return(type1.g)
}

#Power plot for different X/missingness
plot.power <- function(df,title){
  power.g <- df%>%
    tidyr::unnest(eval)%>%
    dplyr::mutate(
      cor=cor/100,
      pm=factor(pm,labels=c('Negative','Positive')),
      beta=sprintf('beta[T]=%s,beta[X]=%s',b.trt,b.X)
    )%>%
    ggplot(aes(x=DO,y=power,colour=beta, shape=beta)) + 
    geom_point() + 
    facet_grid(pm~cor) +
    theme(legend.position = 'bottom') +
    labs(x='Dropout Rate',y='Power') +
    ggtitle(sprintf('%s',title))
  
  return(power.g)
}

#Relative bias plot for different X/missingness
plot.bias <- function(df,title){
  bias.g <- df%>%
    tidyr::unnest(eval)%>%
    dplyr::mutate(
      cor=cor/100,
      pm=factor(pm,labels=c('Negative','Positive')),
      beta=sprintf('beta[T]=%s,beta[X]=%s',b.trt,b.X)
    )%>%
    ggplot(aes(x=DO,y=bias,colour=beta, shape=beta)) + 
    geom_point() + 
    facet_grid(pm~cor) +
    theme(legend.position = 'bottom') +
    labs(x='Dropout Rate',y='Relative Bias') +
    ggtitle(sprintf('%s',title))
  
  return(bias.g)
}

#saving plottly graphs
gg2plotly <- function(gg,file){
  
  tf <- tempfile(fileext = '.html')
  on.exit(unlink(tf),add = TRUE)
  
  gg%>%
    plotly::ggplotly()%>%
    htmlwidgets::saveWidget(file = tf,selfcontained = TRUE)
  
  invisible(file.copy(from = tf,to = file,overwrite = TRUE))
}

#MICE generate complete data
mice.im.comp <- function(dt, n.mi=5){
  dt.mice <- dt%>%
    select(trtn,y.m,X)
  
  dt.mice.imp <- mice(dt.mice, m=n.mi, defaultMethod = "logreg")
  
  imp.n <- data.frame(i = seq(1,dt.mice.imp$m,1))
  imp.n <- as.list(imp.n)
  
  dt.mice.comp <- purrr::pmap_dfr(imp.n, .f=function(i){
    dt <- complete(dt.mice.imp,i)
  }, .id = "i")
  
  return(dt.mice.comp)
  
}

#Combine imputed datasets- Rubin's rules
mi.res.sum <- function(df, M2){
  df1 <- df%>%
    group_by(sim.id,i,trtn)%>%
    summarise(phat = mean(y.m), n = n())%>%
    recast(sim.id + i ~ variable + trtn, measure.var = c("phat",'n'))%>%
    mutate(phat.d = phat_0-phat_1,
           var.d = phat_0*(1-phat_0)/(n_0) + phat_1*(1-phat_1)/(n_1))%>%
    group_by(sim.id)%>%
    summarise(qbar = mean(phat.d),
              ubar = mean(var.d),
              B = var(phat.d))%>%
    mutate(T.var = ubar + (num.mi+1)/num.mi*B, 
           v = floor((num.mi - 1)*(1 + 1/(num.mi+1)*ubar/B)^2),
           
           ci.l = qbar - qt(1-alpha, v)*sqrt(T.var),
           ci.u = qbar + qt(1-alpha, v)*sqrt(T.var),
           reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
}

#calculate bias after using MI
bias.mi.fun <- function(df, M2){
  df%>%
    group_by(sim.id)%>%
    mutate(bias = (qbar-M2)/M2)%>%
    dplyr::select(sim.id, bias)%>%
    dplyr::ungroup(sim.id)%>%
    summarise(bias.m = mean(bias))
}

mice.apply <- function(df.orig){
  df.orig%>%
    mutate(t.H0 = map(t.H0.m, .f = function(df, n.mi=num.mi){
      df1 <- df%>%
        nest(-sim.id)
      df2 <- df1%>%
        mutate(out = purrr::map(data,mice.im.comp, n.mi=n.mi))
      df3 <- df2%>%
        select(sim.id, out)%>%
        unnest()
      return(df3)
    }),
    t.H1 = map(t.H1.m, .f = function(df, n.mi=num.mi){
      df1 <- df%>%
        nest(-sim.id)
      df2 <- df1%>%
        mutate(out = purrr::map(data,mice.im.comp, n.mi=n.mi))
      df3 <- df2%>%
        select(sim.id, out)%>%
        unnest()
      return(df3)
    }))%>%
    select(-t.H0.m, -t.H1.m)%>%
    mutate(mice.H0 = map2(t.H0, M2, mi.res.sum),
           mice.H1 = map2(t.H1, M2, mi.res.sum))%>%
    select(-t.H0,-t.H1)%>%
    mutate(type1 = map(mice.H0, reject.H0),
           power = map(mice.H1, reject.H0),
           type1 = map_dbl(type1, as.numeric),
           power = map_dbl(power, as.numeric),
           bias = map2(mice.H0, M2, bias.mi.fun),
           bias = map_dbl(bias, as.numeric))
  
}

plot.type1.scenario <- function(df, ylim, miss.type='notmnar'){
  df%>%
    dplyr::mutate(
      beta=case_when(miss.type=='notmnar' ~ sprintf("beta[T]=%s, beta[X]=%s",b.trt,b.X),
                     TRUE ~ sprintf("beta[T]=%s, beta[X]=%s, beta[Y]=%s",b.trt,b.X, b.Y)),
      f=sprintf("Delta*':%s'*', '*p[C]*': %s'",M2,p_T)
    )%>%
    ggplot(aes(x=do,y=type1,colour=beta, shape=beta)) + 
    geom_point() + 
    geom_hline(yintercept=c(0.9,1.1)*alpha,
               linetype=2) + 
    facet_wrap(~f,labeller = label_parsed) +
    theme(legend.position = 'bottom') +
    labs(x='Dropout Rate',y='Type-I Error',colour=NULL,shape=NULL) +
    scale_y_continuous(limits = ylim)
}

plot.power.scenario <- function(df, ylim, miss.type='notmnar'){
  df%>%
    dplyr::mutate(
      beta=case_when(miss.type=='notmnar' ~ sprintf("beta[T]=%s, beta[X]=%s",b.trt,b.X),
                     TRUE ~ sprintf("beta[T]=%s, beta[X]=%s, beta[Y]=%s",b.trt,b.X, b.Y)),
      f=sprintf("Delta*':%s'*', '*p[C]*': %s'",M2,p_T)
    )%>%
    ggplot(aes(x=do,y=power,colour=beta, shape=beta)) + 
    geom_point() + 
    facet_wrap(~f,labeller = label_parsed) +
    theme(legend.position = 'bottom') +
    labs(x='Dropout Rate',y='Power',colour=NULL,shape=NULL) +
    scale_y_continuous(limits = ylim)
  
}

plot.bias.scenario <- function(df, ylim, miss.type='notmnar'){
  df%>%
    dplyr::mutate(
      beta=case_when(miss.type=='notmnar' ~ sprintf("beta[T]=%s, beta[X]=%s",b.trt,b.X),
                     TRUE ~ sprintf("beta[T]=%s, beta[X]=%s, beta[Y]=%s",b.trt,b.X, b.Y)),
      f=sprintf("Delta*':%s'*', '*p[C]*': %s'",M2,p_T)
    )%>%
    ggplot(aes(x=do,y=bias,colour=beta, shape=beta)) + 
    geom_point() + 
    facet_wrap(~f,labeller = label_parsed) +
    theme(legend.position = 'bottom') +
    labs(x='Dropout Rate',y='Relative Bias',colour=NULL,shape=NULL) +
    scale_y_continuous(limits = ylim)
}

miss.param.assign <- function(df){
  # MCAR: .trt=0, b.y=0, b.X=0
  b.trt0 <- 0
  b.X0 <- 0
  # MAR1: b.trt=0, b.y=0, b.X=2
  b.trt1 <- 0
  b.X1 <- 2
  # MAR2: b.trt=2, b.y=log(1), b.X=2
  b.trt2 <- 2
  b.X2 <- 2
  # MAR3: b.trt=0, b.y=0, b.X=-2
  b.trt3 <- 0
  b.X3 <- -2
  # MAR4: b.trt=-2, b.y=0, b.X=-2
  b.trt4 <- -2
  b.X4 <- -2
  # MAR5: b.trt=2, b.y=0, b.X=-2
  b.trt5 <- 2
  b.X5 <- -2
  # MAR6: b.trt=-2, b.y=0, b.X=2
  b.trt6 <- -2
  b.X6 <- 2
  # MNAR1: b.trt=-2, b.y=0, b.X=2
  b.trt7 <- 0
  b.X7 <- 2
  b.Y7 <- 2
  # MNAR2: b.trt=-2, b.y=0, b.X=2
  b.trt8 <- 1
  b.X8 <- -2
  b.Y8 <- -2
  
  df%>%
    mutate(b.trt = case_when(missing=='mcar'  ~ as.numeric(b.trt0),
                             missing=='mar1'  ~ as.numeric(b.trt1),
                             missing=='mar2'  ~ as.numeric(b.trt2),
                             missing=='mar3'  ~ as.numeric(b.trt3),
                             missing=='mar4'  ~ as.numeric(b.trt4),
                             missing=='mar5'  ~ as.numeric(b.trt5),
                             missing=='mar6'  ~ as.numeric(b.trt6),
                             missing=='mnar1' ~ as.numeric(b.trt7),
                             missing=='mnar2' ~ as.numeric(b.trt8)),
           b.X = case_when(missing=='mcar'  ~ as.numeric(b.X0),
                           missing=='mar1'  ~ as.numeric(b.X1),
                           missing=='mar2'  ~ as.numeric(b.X2),
                           missing=='mar3'  ~ as.numeric(b.X3),
                           missing=='mar4'  ~ as.numeric(b.X4),
                           missing=='mar5'  ~ as.numeric(b.X5),
                           missing=='mar6'  ~ as.numeric(b.X6),
                           missing=='mnar1' ~ as.numeric(b.X7),
                           missing=='mnar2' ~ as.numeric(b.X8)),
           b.Y = case_when(missing=='mnar1' ~ as.numeric(b.Y7),
                           missing=='mnar2' ~ as.numeric(b.Y8),
                           TRUE ~ as.numeric(0)))
}
