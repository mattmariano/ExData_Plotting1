# Author: Matthew Mariano
# File: plot3.R 
#
# clean up environment
rm(list=ls())
library(lubridate)
library(ggplot2)
dir='data'
df2=data.frame()
df=data.frame();
#
# a function for creating the small subset of data we are interested in
#
processlarge<-function(fname){
  df=read.table(file=fname ,header = T,sep = ';')
  d1=as.Date("01/02/2007","%d/%m/%Y")
  d2=as.Date("02/02/2007","%d/%m/%Y")
  dr=c(d1,d2)
  x=as.Date(df$Date,"%d/%m/%Y") %in% dr
  df2=df[x,]
  datetime=as.Date(df2$Date,"%d/%m/%Y") + hms(df2$Time)
  df2=cbind(datetime,df2)
  df2$Date=as.Date(df2$Date,"%d/%m/%Y")
  df2$Global_active_power=as.numeric(df2$Global_active_power)
  df2$Global_reactive_power=as.numeric(df2$Global_reactive_power)
  df2$Voltage=as.numeric(df2$Voltage)
  df2$Global_intensity=as.numeric(df2$Global_intensity)
  df2$Sub_metering_1=as.numeric(df2$Sub_metering_1)
  df2$Sub_metering_2=as.numeric(df2$Sub_metering_2)
  df2$Sub_metering_3=as.numeric(df2$Sub_metering_3)
  df2
}
labels_format=function(x){
  weekdays.POSIXt(x)
}
# check if the subset file exists. if not recreate it
small=paste(dir,"/small.txt",sep="")
if(!file.exists(small))
{
  fname<- paste(dir,"/household_power_consumption.txt",sep="")
  df2=processlarge(fname);
  rm(df)
  write.table(x=df2,file=small,col.names = T,sep=";")
}else{
  fname=paste(dir,"/small.txt",sep="")
  df2=read.table(file=fname ,header = T,sep = ';')
}


datetime=as.POSIXct(df2$datetime)
sm=df2$Sub_metering_1
df3=data.frame(datetime)
type=rep('Sub_metering_1',length(datetime))
df3=cbind(df3,sm,type)

sm=df2$Sub_metering_2
df4=data.frame(datetime)
type=rep('Sub_metering_2',length(datetime))
df4=cbind(df4,sm,type)

sm=df2$Sub_metering_3
df5=data.frame(datetime)
type=rep('Sub_metering_3',length(datetime))
df5=cbind(df5,sm,type)

df6=rbind(df3,df4,df5)
df6$type=as.factor(df6$type)

# show it to the screen and save as png
#plot(y=df2$Global_active_power,x=df2$datetime,col="red",ylab="Global Active Power (kilowatts)",main="",xlab="")
ggplot(df6,aes(x=as.POSIXct(datetime),y=sm,colour=type))+geom_line()+scale_x_datetime(labels=labels_format)+xlab("")+ylab("Energy sub metering")
png(filename="plot3.png",width=480,height = 480,type=c("quartz"))
ggplot(df6,aes(x=as.POSIXct(datetime),y=sm,colour=type))+geom_line()+scale_x_datetime(labels=labels_format)+xlab("")+ylab("Energy sub metering")
dev.off();
