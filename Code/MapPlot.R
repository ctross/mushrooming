                              
############################################################################### Map
# Omit one trip from plot that is far from others
d2 <- d[which(d$code != "32"),]
 df2c <- data.frame(Longitude=d$x,Latitude=d$y, Trip=factor(d$code))
  df2c2 <- data.frame(Longitude=d$x[which(d$sporo>=1)],Latitude=d$y[which(d$sporo>=1)])
   ddd<- -1300
p1 <- ggplot() +
 geom_path(data=df2c, aes(x=Latitude, y=Longitude, group=Trip, colour=Trip),size=1)   +  xlim(2124000,2131500) +  ylim(594000,607000)+
   geom_rect(aes(xmin=2129000, xmax=2131000, ymin=596000+ddd,ymax=596400+ddd), fill="black", colour="black") +
  geom_rect(aes(xmin=2129000,  xmax=2130000, ymin=596000+ddd,ymax=596400+ddd), fill="white", colour="black") +
  annotate("text", x = 2129000, y = 595800+ddd, label = "0 Km") +
    annotate("text", x = 2130000, y = 595800+ddd, label = "1 Km") +
      annotate("text", x = 2131000, y = 595800+ddd, label = "2 Km") +
 geom_point(data=df2c2,aes(x=Latitude, y=Longitude), size=1, position = position_jitter(w = 100, h = 100),alpha=0.25, col="black" )+
 theme( axis.text.x=element_blank(),
 axis.title.x=element_blank(),
 axis.ticks.x=element_blank(),
 axis.text.y=element_blank(),
 axis.title.y=element_blank(),
 axis.ticks.y=element_blank())  + 
 geom_rect(aes(xmin=2127900, xmax=2128500, ymin=605700,ymax=606300), alpha=0.4, fill="indianred")

  ddd2<-35
 p2 <- ggplot() +
 geom_path(data=df2c, aes(x=Latitude, y=Longitude, group=Trip, colour=Trip),size=1)   +  xlim(2127900,2128500) +  ylim(605700,606300)+
   geom_rect(aes(xmin=2128000, xmax=2128200, ymin=605700+ddd2,ymax=605720+ddd2), fill="black", colour="black") +
  geom_rect(aes(xmin=2128000,  xmax=2128100, ymin=605700+ddd2,ymax=605720+ddd2), fill="white", colour="black") +
  annotate("text", x = 2128000, y = 605700+ddd2-11, label = "0 Km") +
    annotate("text", x = 2128100, y = 605700+ddd2-11, label = "0.1 Km") +
      annotate("text", x = 2128200, y = 605700+ddd2-11, label = "0.2 Km") +
 geom_point(data=df2c2,aes(x=Latitude, y=Longitude), size=1, position = position_jitter(w = 5, h = 5),alpha=0.95, col="black" )+
 theme( axis.text.x=element_blank(),
 axis.title.x=element_blank(),
 axis.ticks.x=element_blank(),
 axis.text.y=element_blank(),
 axis.title.y=element_blank(),
 axis.ticks.y=element_blank()) +
  geom_rect(aes(xmin=2127950, xmax=2128125, ymin=606125,ymax=606225), alpha=0.4, fill="#9999FF")

  GGG<-arrangeGrob(p2, p1,layout_matrix=matrix(c(2,1),nrow=1,ncol=2) ) 
 ggsave("MushroomMap.pdf",GGG,width=12,height=6)
                                                                                 
