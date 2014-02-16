png("img_%d.png")
to_POSIXct <- function(number)
{
  str = as.character(number)
  datetime = substr(str,1,14)
  ms = substr(str,15,17)
  r = as.POSIXct(x=paste(datetime,ms,sep='.'),format="%Y%m%d%H%M%OS")
  return(r)
}

op <- options(digits.secs=3)
df<-read.csv("stats17.txt",sep=",",head=T)
df <- na.omit(df)
df$timestamp = to_POSIXct(df$timestamp)
df$threshold <- df$mean+10*df$sd
df$q <- 0

# for ( r in seq(1,nrow(df))){
#   df$q[r]<-quantile(df[seq(1,r),"maximum"],c(.99))
# }

# res <- sapply(seq(nrow(df)), function(x) 
#   quantile(df[seq(x), "maximum"], c(.99)))
# df <- setNames(cbind(df, res), c(names(df), "q"))

library(ggplot2)
plot(ggplot(df) + 
  geom_line(aes(timestamp, maximum)) + 
  geom_line(aes(timestamp,threshold)) + 
  geom_line(aes(timestamp,q)) + 
  ylab("grey") +  
  scale_x_datetime(breaks = "1 hour", minor_breaks="15 min") + 
  theme(text = element_text(size=20),axis.text.x = element_text(angle=90, vjust=1)))

df3=df[df$maximum>df$threshold,]
plot(ggplot(df3) + 
  geom_point(aes(timestamp, maximum)) + 
  ylab("grey") +  
  scale_x_datetime(breaks = "1 hour", minor_breaks="15 min") + 
  theme(text = element_text(size=20),axis.text.x = element_text(angle=90, vjust=1)))


library(reshape)
df2=df
df2$minimum <- NULL
df2$q <- NULL
df2$sd <- NULL
df2$mean_img <- NULL
df2$sd_img <- NULL
df2 <- melt(df2 ,  id = 'timestamp', variable_name = 'measurement')
# df2$series=as.character(df2$series)
# df2$series[which(df2$series=="mean")]=expression(M[i])
plot(ggplot(df2, aes(timestamp,value)) + geom_line(aes(colour = measurement))+
      ylab("grey value")+
  scale_x_datetime(breaks = "1 hour", minor_breaks="15 min") + 
  theme(text = element_text(size=20),axis.text.x = element_text(angle=90, vjust=1))+
      scale_color_hue(breaks=c("maximum","mean","threshold"),label=c(expression(M[i]),expression(bar(M)[i]),expression(bar(M)[i]+10%.%sigma[M][i])))
)
     
which(is.na(df2$timestamp))
intervals=data.frame(intervals=as.numeric(diff(df2$timestamp)))
which(is.na(intervals))
intervals=na.omit(intervals)
intervals=intervals[intervals>0]
print(mean(intervals))
print(sd(intervals))
hist(intervals[intervals>0])

  
