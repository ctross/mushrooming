  x <- 0:30
  y <- 1/(x+1)
  
  df<-data.frame(x,y)

  

  ggplot(df,aes(x=x,y=y))+
           geom_rect(aes(xmin=0, xmax=30, ymin=PCI(Tab1[which(Sex[1,]==1),7],0.5)[1],ymax=PCI(Tab1[which(Sex[1,]==1),7],0.5)[2]), alpha=0.025, fill="black") +
           geom_rect(aes(xmin=0, xmax=30, ymin=PCI(Tab1[which(Sex[1,]==2),7],0.5)[1],ymax=PCI(Tab1[which(Sex[1,]==2),7],0.5)[2]), alpha=0.025, fill="darkred") +
           geom_point(size=3)+ geom_line(linetype="dotted") + 
           #geom_hline(yintercept = mean(Tab1[,7]),col="darkred") +
           geom_hline(aes(yintercept = mean(Tab1[which(Sex[1,]==1),7]),linetype = "Male"),color="black") +
           geom_hline(aes(yintercept = mean(Tab1[which(Sex[1,]==2),7]), linetype = "Female"),color="darkred") +     
           xlim(0,30) +
           labs(y="Return-rate since last encounter", x = "Time-steps since last encounter") + theme(strip.text.x = 
           element_text(size=14,face="bold"),axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))  +      scale_linetype_manual(name = "Sex", values = c(1, 1),labels=c("Male","Female"), 
                      guide = guide_legend(override.aes = list(color = c("black", "darkred")))) + theme(legend.text=element_text(size=12))
          ggsave("MushTheorySex.pdf",width=8.5,height=8.5)
                            