# Alessandro Gentilini - February 2014

png(filename="img_%d.png",width=15,height=10,units="cm",res=600)

library(ggplot2)
library(reshape)
library(scales)

to_POSIXct <- function(number)
{
  str = as.character(number)
  datetime = substr(str,1,14)
  ms = substr(str,15,17)
  r = as.POSIXct(x=paste(datetime,ms,sep='.'),format="%Y%m%d%H%M%OS")
  return(r)
}

generate_graph <- function(filename)
{
  df<-read.csv(filename,sep=",",head=T)
  df <- na.omit(df)
  df$timestamp = to_POSIXct(df$timestamp)
  df$threshold <- df$mean+10*df$sd
  
  n=nrow(df[df$maximum > df$threshold,])
  N=nrow(df)
  dt=difftime(df$timestamp[N],df$timestamp[1],unit="secs")
  title = sprintf("%s, %d events in %s",as.Date(df$timestamp[1]),n,format(.POSIXct(dt,tz="GMT"), "%H:%M:%S"))
  
  df2=df
  df2$minimum <- NULL
  df2$q <- NULL
  df2$sd <- NULL
  df2$mean_img <- NULL
  df2$sd_img <- NULL
  df2 <- melt(df2 ,  id = 'timestamp', variable_name = 'measurement')
  plot(ggplot(df2, aes(timestamp,value)) + geom_line(aes(colour = measurement))+
         ylab("pixel grey value")+
         ggtitle(title)+
         scale_x_datetime(breaks = "1 hour", minor_breaks="15 min",labels = date_format("%H")) + 
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
}

op <- options(digits.secs=3)
#generate_graph("stats8.txt")
generate_graph("stats9.txt")
generate_graph("stats10.txt")
#generate_graph("stats12.txt")
generate_graph("stats13.txt")
generate_graph("stats14.txt")
generate_graph("stats15.txt")
generate_graph("stats16.txt")
generate_graph("stats17.txt")
#generate_graph("short.txt")