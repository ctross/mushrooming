model_dat=list(
 Distance=Distance, 
 AngDiff=AngDiff,
 Yield=Yield, 
 Sex=Sex,
 N=N,
 Lags=Lags,
 MaxTrip=max(Trip),
 MaxTicks=MaxTicks)


model_code3 <- '
functions{
//######################################### Quick Gaussian Process Cholesky Factor
matrix Quick_GP_L(int SIZE, real COR, real DECAY, real SCALE){
 matrix[SIZE,SIZE] Rho;                       //# Cholesky factor to be made
 real RealSIZE;                               //# Used to normalize distance
 real G;                                      //# Intermediate storage

 RealSIZE = SIZE;                             //# Convert Int to Real

 for(i in 1:(SIZE-1)){
 for(j in (i+1):SIZE){
  G = ((j-i) * (j-i))/(RealSIZE*RealSIZE);    //# Calculate normalized sq distance
  Rho[i,j] = COR * exp(-DECAY * G);           //# Estimate correlations
  Rho[j,i] = Rho[i,j];                        //# Fill other triangle
                      }}

 for (i in 1:SIZE){
  Rho[i,i] = 1;                               //# Fill diagonal
                  }

  Rho = SCALE*cholesky_decompose(Rho);        //# Decompose Rho

return Rho;
}
}

data{
int MaxTrip;
int MaxTicks;
int N[MaxTrip];
int Lags;

real Distance[MaxTicks,MaxTrip];
real AngDiff[MaxTicks,MaxTrip];
int Yield[MaxTicks,MaxTrip];    
int Sex[MaxTicks,MaxTrip];
}

parameters {
real AlphaDist[2];
real MuBetaDist[2];
real<lower=0> SDDist[2]; 

real AlphaAngle[2];
real MuBetaAngle[2];
real<lower=0> DAngle[2]; 

matrix<lower=0, upper=1>[2,2] Rho;
matrix<lower=0>[2,2] Decay;
matrix<lower=0>[2,2] Scale;

vector[Lags] Delta[2,2];
}

transformed parameters{
vector[Lags] BetaDist[2];
vector[Lags] BetaAngle[2];

BetaDist[1] = MuBetaDist[1] + Delta[1,1];
BetaAngle[1] = MuBetaAngle[1] + Delta[2,1];

BetaDist[2] = MuBetaDist[2] + Delta[1,2];
BetaAngle[2] = MuBetaAngle[2] + Delta[2,2];
}


model{
to_vector(Rho) ~ beta(12,2);
to_vector(Decay) ~ normal(0,5);
to_vector(Scale) ~ normal(0,5);

MuBetaDist ~ normal(0,2.5);
MuBetaAngle ~ normal(0,2.5);

Delta[1,1] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[1,1], Decay[1,1], Scale[1,1]));
Delta[2,1] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[2,1], Decay[2,1], Scale[2,1]));

Delta[1,2] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[1,2], Decay[1,2], Scale[1,2]));
Delta[2,2] ~ multi_normal_cholesky(rep_vector(0,Lags), Quick_GP_L(Lags, Rho[2,2], Decay[2,2], Scale[2,2]));

AlphaDist ~ normal(0,5);
SDDist ~ cauchy(0,1);  

AlphaAngle ~ normal(0,5);
DAngle ~ cauchy(0,1);   

for(j in 1:MaxTrip){
{

 vector[N[j]-Lags] PredAngle;
 vector[N[j]-Lags] PredDist;
 vector[N[j]-Lags] Dist;
 vector[N[j]-Lags] AngleDifference;
  
 for(i in (Lags+1):N[j]){
  PredDist[i-Lags] = AlphaDist[Sex[1,j]]; 
     for(k in 1:Lags){
    PredDist[i-Lags] = PredDist[i-Lags] + BetaDist[Sex[1,j],k]*Yield[i-k,j];  
        }} 
        
  for(i in (Lags+1):N[j]){
  PredAngle[i-Lags] = AlphaAngle[Sex[1,j]]; 
     for(k in 1:Lags){
    PredAngle[i-Lags] = PredAngle[i-Lags] + BetaAngle[Sex[1,j],k]*Yield[i-k,j];  
        }}       
              
   for(i in (Lags+1):N[j]){            
Dist[i-Lags] = Distance[i,j];              
AngleDifference[i-Lags] = AngDiff[i,j];
              }
Dist ~ lognormal(PredDist,SDDist[Sex[1,j]]);
AngleDifference ~ beta(inv_logit(PredAngle)*DAngle[Sex[1,j]], (1-inv_logit(PredAngle))*DAngle[Sex[1,j]]);
          }}    

}

'

m3 <- stan( model_code=model_code3, data=model_dat,refresh=1,chains=2,control=list(max_treedepth=13))
    