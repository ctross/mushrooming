#make caterpillar plot
m4a<-rstan::extract(m4,pars="BetaAngle")
sample_eff<-apply(m4a$BetaAngle,2,quantile,probs=c(0.05,0.5,0.95))
df_angle<-data.frame(Lags=c(1:Lags),Group="Turning-Angle",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
m4d<-rstan::extract(m4,pars="BetaDist")
sample_eff<-apply(m4d$BetaDist,2,quantile,probs=c(0.05,0.5,0.95))
df_dist<-data.frame(Lags=c(1:Lags),Group="Step-Size",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
df_all<-rbind(df_angle,df_dist)

g1 <- ggplot(df_angle,aes(x=Lags,y=Median))+geom_point()+ annotation_compass('A','NE') +
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
 labs(y="Effect Size, \U03C8") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
        
g2 <- ggplot(df_dist,aes(x=Lags,y=Median))+geom_point()+   annotation_compass('B','NW') +
 geom_linerange(aes(ymin=LI,ymax=HI))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+
 labs(y="Effect Size, \U03D5") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
        
CairoPDF("CaterAngleVert.pdf",width=4.5,height=4.5)
print(g1)
dev.off()

CairoPDF("CaterStepVert.pdf",width=4.5,height=4.5)
print(g2)
dev.off()
                 
print(m4,digits_summary=3)

