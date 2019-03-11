biv.function<-function(datavector1, datavector2, desired.cor, pearson=TRUE, 
                       spearman=FALSE)  {
  #create a bivariate dataset with desired correlation input
  #datavetor1=marginal distribution 1
  #datavetor2=marginal distribution 2
  #desired.cor=desired correlation for dataset
  #pearson=want pearson correlation, default is TRUE
  #spearman=want spearman correlation, default is FALSE
  
  if(desired.cor==0) {
    stop('Induced correlation cannot be 0!')
  }
  if(length(datavector1)!=length(datavector2)) {
    stop('datavector1 and datavector2 must be the same length!')
  }
  if(length(datavector1)<100000) {
    stop('Data must have at least 100,000 observations for finding the correlation bounds accurately!')
  }
  if(pearson==TRUE & spearman==TRUE | pearson==FALSE & spearman==FALSE) {
    stop('Either specify Pearson or Spearman correlation, but not both!')
  }
  
  N<-length(datavector1)
  
  UB.dat<-cbind(sort(datavector1), sort(datavector2))
  LB.dat<-cbind(sort(datavector1), sort(datavector2, decreasing=TRUE))
  
  if(pearson==TRUE) {
    LB<-cor(LB.dat)[1,2]
    UB<-cor(UB.dat)[1,2]
  } else if(spearman==TRUE) {
    LB<-cor(LB.dat, method='spearman')[1,2]
    UB<-cor(UB.dat, method='spearman')[1,2]    
  }
  
  
  if(desired.cor>UB | desired.cor<LB) {
    stop('Desired correlation is not within feasible correlation bounds for data!')
  }
  
  if(desired.cor>0) {
    sort.perc<-desired.cor/UB
    Nsort<-ceiling(N*sort.perc)
    
    new.dat<-rbind(cbind(sort(datavector1[1:Nsort]), sort(datavector2[1:Nsort])),
                   cbind(datavector1[(Nsort+1):N], datavector2[(Nsort+1):N]))
    
  } else if(desired.cor<0) {
    sort.perc<-desired.cor/LB
    Nsort<-ceiling(N*sort.perc)
    new.dat<-rbind(cbind(sort(datavector1[1:Nsort]), sort(datavector2[1:Nsort], 
                                                          decreasing=TRUE)),
                   cbind(datavector1[(Nsort+1):N], datavector2[(Nsort+1):N]))
  }
  
  if(pearson==TRUE) {
    emp.cor<-paste0(round(cor(new.dat, method='pearson')[1,2],4))
    cor.type<-'Pearson'
  } else if(spearman==TRUE) {
    emp.cor<-paste0(round(cor(new.dat, method='spearman')[1,2],4))
    cor.type<-'Spearman'
  }
  
  print(paste0('Bounds=(', paste(round(c(LB, UB), 4), 
                                 collapse=','), '), Desired Cor=', desired.cor, ', Empirical Cor=', emp.cor))
  
  return(list(bounds=c(LB, UB),
              cor.type=cor.type,
              desired.cor=desired.cor,
              empirical.cor=emp.cor))
}
