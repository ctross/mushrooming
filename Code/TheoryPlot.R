  x <- 0:30
  y <- 1/(x+1)
  
  df<-data.frame(x,y)



  ggplot(df,aes(x=x,y=y))+geom_rect(aes(xmin=0, xmax=30, ymin=PCI(Tab1[,7],0.5)[1],ymax=PCI(Tab1[,7],0.5)[2]), alpha=0.025, fill="indianred") +
           geom_point(size=3)+ geom_line(linetype="dotted") + 
           geom_hline(yintercept = mean(Tab1[,7]),col="darkred") +
           #geom_hline(yintercept = mean(Tab1[which(Sex[1,]==1),7]),col="blue") +
           #geom_hline(yintercept = mean(Tab1[which(Sex[1,]==2),7]),col="green") +     
           xlim(0,30) +
           labs(y="Return-rate since last encounter", x = "Time-steps since last encounter") + theme(strip.text.x = 
           element_text(size=14,face="bold"),axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
          ggsave("MushTheory.pdf",width=8.5,height=8.5)
          