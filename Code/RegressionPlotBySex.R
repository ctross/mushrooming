
#make caterpillar plot
m3a<-rstan::extract(m3,pars="BetaAngle")
sample_eff<-apply(m3a$BetaAngle[,1,],2,quantile,probs=c(0.05,0.5,0.95))
df_angle1<-data.frame(Lags=c(1:Lags),Group="Turning-Angle", Sex="Male",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
m3d<-rstan::extract(m3,pars="BetaDist")
sample_eff<-apply(m3d$BetaDist[,1,],2,quantile,probs=c(0.05,0.5,0.95))
df_dist1<-data.frame(Lags=c(1:Lags),Group="Step-Size", Sex="Male",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])


#make caterpillar plot
m3a<-rstan::extract(m3,pars="BetaAngle")
sample_eff<-apply(m3a$BetaAngle[,2,],2,quantile,probs=c(0.05,0.5,0.95))
df_angle2<-data.frame(Lags=c(1:Lags)+0.33,Group="Turning-Angle",  Sex="Female",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      
m3d<-rstan::extract(m3,pars="BetaDist")
sample_eff<-apply(m3d$BetaDist[,2,],2,quantile,probs=c(0.05,0.5,0.95))
df_dist2<-data.frame(Lags=c(1:Lags)+0.33,Group="Step-Size", Sex="Female",
                      LI=sample_eff[1,],
                      Median=sample_eff[2,],
                      HI=sample_eff[3,])
                      




g1 <- ggplot(rbind(df_angle1,df_angle2),aes(x=Lags,y=Median))+geom_point(aes(color=Sex))+ annotation_compass('A','NE') +
 geom_linerange(aes(ymin=LI,ymax=HI,color=Sex))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+  scale_color_manual(values=c("black","darkred"))+
 labs(y="Effect Size, \U03C8") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
        
g2 <- ggplot(rbind(df_dist1,df_dist2),aes(x=Lags,y=Median))+geom_point(aes(color=Sex))+   annotation_compass('B','NW') +
 geom_linerange(aes(ymin=LI,ymax=HI,color=Sex))+facet_wrap(~Group,scales="free")+
 geom_hline(aes(yintercept=0),color="blue",linetype="dashed")+  scale_color_manual(values=c("black","darkred"))+
 labs(y="Effect Size, \U03D5") + theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
        
CairoPDF("CaterAngleSex.pdf",width=4.5,height=4.5)
print(g1)
dev.off()

CairoPDF("CaterStepSex.pdf",width=4.5,height=4.5)
print(g2)
dev.off()
                








