 Trip <- d$code
    
 MaxTicks<-653
    
 Yield <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 Distance <-matrix(NA,ncol=max(Trip),nrow=MaxTicks) 
 Ang <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 AngDiff <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 Sex <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)
 Vert <-matrix(NA,ncol=max(Trip),nrow=MaxTicks)

 N<-c()
 for(j in 1:max(Trip)){
 X <- d$x[which(Trip==j)]
 Y <- d$y[which(Trip==j)]
 Z <- d$z[which(Trip==j)]
 SexT <- ifelse(d$sex[which(Trip==j)]=="H",1,2)
 N[j] <- length(X)
 Yieldq<-d$sporo[which(Trip==j)]
 Yieldq[is.na(Yieldq)]<-0         # Only encounters were marked. Non-encounter are now coded as zeros.
 Yield[1:N[j],j]<-Yieldq     
 
 
  Dist<-c()  
  ang1<-c()
  angdif<-c()
  VertZ <- c()
  
 for(i in 2:N[j]){
 Dist[i]<- dist(X[i],X[i-1],Y[i],Y[i-1]);
 VertZ[i]<- Z[i] - Z[i-1];
 ang1[i]<- ang(X[i],X[i-1],Y[i],Y[i-1])
 if(i>3){
 angdif[i]<- ang.dif(ang1[i],ang1[i-1])
         }}    
         
  Distance[1:N[j],j]<-Dist
  Ang[1:N[j],j]<-ang1       
  AngDiff[1:N[j],j]<-angdif
  Sex[1:N[j],j]<-SexT
  Vert[1:N[j],j]<-VertZ
 }

     min(Distance[which(Distance>0)],na.rm=TRUE)
 MD<-min(Distance[which(Distance>0)],na.rm=TRUE)
 max(Distance,na.rm=TRUE)
 
for(i in 1:MaxTicks){
for(j in 1:max(Trip)){
Distance[i,j] <- ifelse(Distance[i,j]==0,runif(1,0,MD),Distance[i,j])
      }}

Distance[is.na(Distance)] <- 999999
Yield <- ifelse(Yield>=1,1,0)
Yield[is.na(Yield)] <- 999999
Sex[is.na(Sex)] <- 999999
Vert[is.na(Vert)] <- 999999

EmptyAngle <-  fitdistr( ScaleBeta(c(na.omit(c(AngDiff)))),start=list(shape1=1,shape2=1),"beta")$estimate

for(i in 1:MaxTicks){
for(j in 1:max(Trip)){
AngDiff[i,j] <- ifelse(is.nan(AngDiff[i,j]), rbeta(1,EmptyAngle[1], EmptyAngle[2])*pi,AngDiff[i,j])
      }}
      
AngDiff <- ScaleBeta(AngDiff)
AngDiff[is.na(AngDiff)] <- 999999
Lags<-30
    
AngDiff <- AngDiff[c(-1,-2,-3),]
Distance <- Distance[c(-1,-2,-3),]
Yield <- Yield[c(-1,-2,-3),]
Sex <- Sex[c(-1,-2,-3),]
Vert <- Vert[c(-1,-2,-3),]
N <- N-3
MaxTicks<-MaxTicks-3
