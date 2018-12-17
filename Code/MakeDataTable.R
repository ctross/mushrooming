############################################################ Load Functions
ScaleBeta <- function(X){
 a<-0
 b<-pi
 y<-X
 Samp<-50

 y2 <- (y-a)/(b-a)
 y3<-(y2*(Samp - 1) + 0.5)/Samp
 y3}

rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}
ang.dif <- function(x,y) {min((2 * pi) - abs(x - y), abs(x - y))}

dist<-function(x2a,x1a,y2a,y1a){
   return(sqrt((x2a-x1a)^2 + (y2a-y1a)^2))}  

ang<-function(x2,x1,y2,y1){
  Theta<-seq(0,2*pi,length.out=100)
  DB<-cbind(cos(Theta),sin(Theta))
   r <- dist(x2,x1,y2,y1)
   x0<-(x2-x1)/r
   y0<-(y2-y1)/r 
   
   (atan2(y0,x0) + pi )
   } 
   


scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
	laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
	# First rectangle
	rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)
	
	# Second rectangle
	rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)
	
	# Legend
	scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x = laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size = legend.size, colour = legend.colour)
	
	res <- list(rectangle1, rectangle2, scaleBarLegend)
	
	return(res)
}

            

 d<-read.csv("Data/clusters_by_site.csv")
 d$code<-   as.numeric(d$code)
 
 # Remove some corrupted GPS tracks
 d <- d[-which(d$code==1 | d$code==2 | d$code==7 | d$code==8 | d$code==9 | d$code==10 | d$code==11 | d$code==38 | d$code==41 | d$code==42),] 
 d$code<-as.numeric(factor(d$code))
 
 ############################################################ Table 1
 Tab1 <- matrix(NA,ncol=10,nrow=34)
 colnames(Tab1) <- c("GPS Points", "Distance","mdist","mspeed", "Time", "Average Speed", "Elevation Change", "Encounters","Prop", "Recovered")
 
 for(i in 1:max(d$code)){
 scrap.d<-c()
 for( j in 2:length(d$x[which(d$code==i)])){
 scrap.d[j]<-dist(d$x[which(d$code==i)][j], d$x[which(d$code==i)][j-1],d$y[which(d$code==i)][j], d$y[which(d$code==i)][j-1])
               }
 Tab1[i,] <- c( length(d$z[which(d$code==i)]),
                round(sum(scrap.d,na.rm=TRUE)/1000,2),
                round(max(scrap.d,na.rm=TRUE),2) ,
                round(max(scrap.d,na.rm=TRUE)/30,2) ,
                round((length(d$z[which(d$code==i)])*30)/(60*60),2),
                round(mean( (sum(scrap.d,na.rm=TRUE)/1000)/30   )*3.6,2),
                (max(as.numeric(as.character(d$z[which(d$code==i)])))-min(as.numeric(as.character(d$z[which(d$code==i)])))),
                sum(ifelse(d$sporo[which(d$code==i)]>=1,1,0),na.rm=TRUE),
                   sum(ifelse(d$sporo[which(d$code==i)]>=1,1,0),na.rm=TRUE)/length(d$z[which(d$code==i)]),
                sum(d$sporo[which(d$code==i)],na.rm=TRUE)
                                            )
                                            }
 Tab1 <- Tab1[,c(1,2,5,6,7,8,9,10)]
 Tab2 <- matrix(NA,ncol=8,nrow=41)
  A <- boxplot(Tab1)
mytable <- A$stats
colnames(mytable)<-A$names
rownames(mytable)<-c('Min','Lower quartile','Median','Upper quartile','Max')
       
 colnames(Tab2) <- c("GPS Points", "Distance", "Time", "Average Speed", "Elevation Change", "Encounters", "Prop", "Recovered")
  Tab2[1:34,] <- Tab1
  Tab2[35,] <- colSums(Tab1,na.rm=TRUE)
  Tab2[36,] <- colMeans(Tab1,na.rm=TRUE)
  Tab2[37:41,] <- mytable
 print(xtable(Tab2, digits=c(0,0,2,2,2,1,0,2,0)) )
         
         

         
         
         
         
         
       