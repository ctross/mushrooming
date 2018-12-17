###############################################################################
  x <- 1:(Lags)
  y <- 1/(x+1)
   
  lm_eqn = function(m) {
l <- list(
      b = format(as.vector(coef(m)[2]), digits = 2),
      z = format(confint(m)[2,1], digits = 2),
      h = format(confint(m)[2,2], digits = 2),
      r2 = format(summary(m)$r.squared, digits = 2))
      if (coef(m)[2] >= 0)  {
      eq <- substitute(beta == b*" ("*z*", "~h*"),"~~italic(r)^2~"="~r2,l)
      } else {
      eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
      }
      as.character(as.expression(eq));                 
      }




  resn<-apply(m1d$BetaDist,2,quantile,probs=c(0.05,0.5,0.95))          
  df<-data.frame(x=log(y[1:13]),y=log(abs(resn[2,1:13])))        # Fit over the lagged parameters with reliable effects
  
  p1<- ggplot(df,aes(x=x,y=y))+geom_point(size=3) +   geom_smooth(method='lm',colour="indianred") +   annotation_compass('B','SE') +
           labs(x="Log return-rate since last encounter", y = "Log decrement in step-size") +
           geom_text(aes(x = -1.5, y = log(1), label = lm_eqn(lm(y ~ x, df))), parse = TRUE,size=5)+ 
           theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))   

          
  resn<-apply(m1a$BetaAngle,2,quantile,probs=c(0.05,0.5,0.95))
  dfb<-data.frame(x=log(y[1:9]),y=log((resn[2,1:9])))       # Fit over the lagged parameters with reliable effects, 
  
   p2<-ggplot(dfb,aes(x=x,y=y))+geom_point(size=3) +   geom_smooth(method='lm',colour="indianred") + annotation_compass('A','SE') +
           labs(x="Log return-rate since last encounter", y = "Log increment in turning-angle") +
           geom_text(aes(x = -1.5, y = -1.6, label = lm_eqn(lm(y ~ x, dfb))), parse = TRUE,size=5)+
           theme(strip.text.x = element_text(size=14,face="bold"),axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))   
          
CairoPDF("ElastStep.pdf",width=4.5,height=4.5)
print(p1)
dev.off()

CairoPDF("ElastAngle.pdf",width=4.5,height=4.5)
print(p2)
dev.off()
                      