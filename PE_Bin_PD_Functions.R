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
Wald.CI <- function(df, n, M2){
  
  df%>%
    group_by(sim.id, trt)%>%
    summarise(phat = mean(y))%>%
    dcast(sim.id ~ trt, value.var = "phat")%>%
    mutate(phat.d = C-T, ci.l = C-T - qnorm(1-alpha)*sqrt(C*(1-C)/(n/2)+T*(1-T)/(n/2)),
           ci.u = C-T + qnorm(1-alpha)*sqrt(C*(1-C)/(n/2)+T*(1-T)/(n/2)),
           reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
}

# Produces point estimate and FM CI for difference in proportions & test H
FM.CI <- function(df, n, M2){
  df%>%
    group_by(sim.id, trt)%>%
    summarise(phat = mean(y))%>%
    dcast(sim.id ~ trt, value.var = "phat")%>%
    rename(p_T = T, p_C = C)%>%
    p.rmle.fm(M2 = M2)%>%
    mutate(phat.d = p_C-p_T, 
           ci.l = phat.d - qnorm(1-alpha)*sqrt(p_C.rmle*(1-p_C.rmle)/(n/2)+p_T.rmle*(1-p_T.rmle)/(n/2)),
           ci.u = phat.d + qnorm(1-alpha)*sqrt(p_C.rmle*(1-p_C.rmle)/(n/2)+p_T.rmle*(1-p_T.rmle)/(n/2)),
           reject.h0 = case_when(ci.u < M2 ~ 1, TRUE ~ 0))
}

# Summarizes number of rejections of H0
reject.H0 <- function(df){
  df%>%summarise(sum=sum(reject.h0)/n())
}


# Calculated Wilson-Newcombe CI
wn.CI <- function(df, N_C, N_T, M2, alpha){
  
  df%>%
    group_by(sim.id, trt)%>%
    summarise(phat = mean(y))%>%
    dcast(sim.id ~ trt, value.var = "phat")%>%
    dplyr::rename(phat.C = C, phat.T = T)%>%
    dplyr::mutate(z = qnorm(1-alpha),
                  l.C = (phat.C + z^2/(2*N_C) - z*sqrt((phat.C*(1-phat.C)+z^2/(4*N_C))/N_C))/(1+z^2/N_C),
                  u.C = (phat.C + z^2/(2*N_C) + z*sqrt((phat.C*(1-phat.C)+z^2/(4*N_C))/N_C))/(1+z^2/N_C),
                  
                  l.T = (phat.T + z^2/(2*N_T) - z*sqrt((phat.T*(1-phat.T)+z^2/(4*N_T))/N_T))/(1+z^2/N_T),
                  u.T = (phat.T + z^2/(2*N_T) + z*sqrt((phat.T*(1-phat.T)+z^2/(4*N_T))/N_T))/(1+z^2/N_T),
                  
                  ci.l = phat.C-phat.T-sqrt((phat.C-l.C)^2+(u.T-phat.T)^2),
                  ci.u = phat.C-phat.T+sqrt((u.C-phat.C)^2+(phat.T-l.T)^2),
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
add.X <- function(df, rho=0.3){
  df1 <- df%>%
    mutate(X = rnorm(length(trt),4,1))%>%
    mutate(X = case_when(X<0 ~ 0, 
                         X>10 ~ 10, 
                         TRUE ~ as.numeric(X)))
  
  df2 <- bind_cols(df1%>%select(-X)%>%arrange(y), df1%>%select(-y)%>%arrange(X)%>%select(X))
  rho.bound <-cor(df2$y, df2$X)
  
  t.order.C <- df1%>%
    filter(trt=="C")%>%
    slice(1:floor(rho*n()/rho.bound))
  t.asis.C  <- df1%>%
    filter(trt=="C")%>%
    slice(floor(rho*n()/rho.bound)+1:n())
  
  t.order.T <- df1%>%
    filter(trt=="T")%>%
    slice(1:floor(rho*n()/rho.bound))
  t.asis.T  <- df1%>%
    filter(trt=="T")%>%
    slice(floor(rho*n()/rho.bound)+1:n())
  
  
  t.ordered.C <- bind_cols(t.order.C%>%select(-X)%>%arrange(y), 
                           t.order.C%>%select(-y)%>%arrange(X)%>%select(X))
  t.ordered.T <- bind_cols(t.order.T%>%select(-X)%>%arrange(y), 
                           t.order.T%>%select(-y)%>%arrange(X)%>%select(X))
  
  
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

# Function that checks simulateed correlation

check_rho <- function(df){
  df%>%
    tidyr::nest(-sim.id)%>%
    dplyr::mutate(cor = map(data , .f = function(df){
      cor(df$y,df$X, method = "spearman")
    }))%>%
    dplyr::select(-data)%>%
    tidyr::unnest()%>%
    dplyr::summarise(mean.cor = round(mean(cor),2), sd = round(sqrt(var(cor)),2))
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