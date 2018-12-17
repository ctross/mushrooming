 
###################################################################### Plot maps                      
d$Hunt<-factor(d$code)
d$Latitude<-(d$y)
d$Longitude<-(d$x)

d$encountered2<-ifelse(d$sporo>0,1,0)
d$encountered2[is.na(d$sporo)]<-0

for( j in 1:34){
d$encountered2[min(which(d$Hunt==j))] <-1
}


d$timesince <- NA
d$timesince[1] <- 0
for( i in 1:length(d$timesince)){
d$timesince[i] <- ifelse(d$encountered2[i]==1,0,d$timesince[i-1]+1)
}

d$timesince <- (30*ifelse(as.numeric(d$timesince)>20,20, as.numeric(d$timesince)))/60

for( j in 1:34){
d$encountered2[min(which(d$Hunt==j))] <-0
}

d2<-d[which(d$encountered2==1),]

for( j in 1:34){
 i<-0
 while(i>=0){
if(d$timesince[min(which(d$Hunt==j))+i]<10 & d$encountered2[min(which(d$Hunt==j))+i]==0){
   d$timesince[min(which(d$Hunt==j))+i]<-10
   i<-i+1}  else{ 
i<- -1}
   }}

 ddd<- -1300
 
# xmin=2126100 - 1400
# xmax=2127200 - 1400
# ymin=595600 - 500
# ymax=597500 - 500
 
 bbb<-450
 vvv<-0
 
 xmin=2127700 
 xmax=2128500 
 ymin=605100
 ymax=606600
 
(p1 <- ggplot() +
 geom_path(data=d, aes(x=Latitude, y=Longitude, group=Hunt, colour=timesince),size=0.5)  +  
 geom_point(data=d2,aes(x=Latitude, y=Longitude ), size=0.5,shape=20, position = position_jitter(w = 100, h = 100),alpha=0.25, col="black" )+  scale_colour_gradientn(name="Minutes since \n last encounter",
 colours = terrain.colors(10)[1:8],breaks=c(0,2.5,5,7.5,10),labels=c(0,2.5,5,7.5,"\u2265 10"),guide=FALSE)  +
 theme( axis.text.x=element_blank(),
 axis.title.x=element_blank(),
 axis.ticks.x=element_blank(),
 axis.text.y=element_blank(),
 axis.title.y=element_blank(),
 axis.ticks.y=element_blank())  +    annotation_compass('A','NE') +
 xlim(2124000,2131500) +  ylim(594000,607000)+
 geom_rect(aes(xmin=2129000, xmax=2131000, ymin=596000+ddd,ymax=596400+ddd), fill="black", colour="black") +
 geom_rect(aes(xmin=2129000,  xmax=2130000, ymin=596000+ddd,ymax=596400+ddd), fill="white", colour="black") +
 annotate("text", x = 2128750, y = 595800+ddd-100, label = "0km") +
 annotate("text", x = 2130000, y = 595800+ddd-100, label = "1km") +
 annotate("text", x = 2131250, y = 595800+ddd-100, label = "2km") +     
 geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin,ymax=ymax), alpha=0.5, fill="indianred")    )
    
    ddd2 <- -100                                              
(p2 <- ggplot() +
 geom_path(data=d, aes(x=Latitude, y=Longitude, group=Hunt, colour=timesince),size=0.5) +
 xlim(xmin,xmax-vvv) +  ylim(ymin+bbb,ymax-300)+
 geom_point(data=d2,aes(x=Latitude, y=Longitude ),size=1.5,shape=20 )+   scale_colour_gradientn(name="Minutes since \n last encounter",
 colours = terrain.colors(10)[1:8],breaks=c(0,2.5,5,7.5,10),labels=c(0,2.5,5,7.5,"\u2265 10"))  +
 theme(axis.text.x=element_blank(),
 axis.title.x=element_blank(),
 axis.ticks.x=element_blank(),
 axis.text.y=element_blank(),
 axis.title.y=element_blank(),
 axis.ticks.y=element_blank()) +       annotation_compass('B','NE') +
  geom_rect(aes(xmin=2128000, xmax=2128200, ymin=605700+ddd2,ymax=605725+ddd2), fill="black", colour="black") +
  geom_rect(aes(xmin=2128000,  xmax=2128100, ymin=605700+ddd2,ymax=605725+ddd2), fill="white", colour="black") +
  annotate("text", x = 2127950, y = 605700+ddd2-31, label = "0km") +
    annotate("text", x = 2128100, y = 605700+ddd2-31, label = "0.1km") +
      annotate("text", x = 2128250, y = 605700+ddd2-31, label = "0.2km") 
   )

CairoPDF("Map1.pdf",width=4.5,height=4.5)
print(p1)
dev.off()

CairoPDF("Map2.pdf",width=4.5,height=4.5)
print(p2)
dev.off()







