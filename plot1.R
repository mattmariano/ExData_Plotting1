# Author: Matthew Mariano
# File: plot1.R 
#
# clean up environment
rm(list=ls())
library(lubridate)
dir='data'
df2=data.frame()
df=data.frame();
#
# a functrion for creating the small subset of data we are interested in
#
processlarge<-function(fname){
  df=read.table(file=fname ,header = T,sep = ';')
  d1=as.Date("01/02/2007","%d/%m/%Y")
  d2=as.Date("02/02/2007","%d/%m/%Y")
  dr=c(d1,d2)
  x=as.Date(df$Date,"%d/%m/%Y") %in% dr
  df2=df[x,]
  datetime=as.Date(df2$Date) + hms(df2$Time)
  df2=cbind(datetime,df2)
  df2$Global_active_power=as.numeric(df2$Global_active_power)
  df2$Global_reactive_power=as.numeric(df2$Global_reactive_power)
  df2$Voltage=as.numeric(df2$Voltage)
  df2$Global_intensity=as.numeric(df2$Global_intensity)
  df2$Sub_metering_1=as.numeric(df2$Sub_metering_1)
  df2$Sub_metering_2=as.numeric(df2$Sub_metering_2)
  df2$Sub_metering_3=as.numeric(df2$Sub_metering_3)
  df2
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
# show it to the screen and save as png
hist(df2$Global_active_power,col="red",xlab="Global Active Power (kilowatts)",main="Global Active Power",ylab="Frequency")
png(filename="plot1.png",width=480,height = 480,type=c("quartz"))
hist(df2$Global_active_power,col="red",xlab="Global Active Power (kilowatts)",main="Global Active Power",ylab="Frequency")
dev.off();
