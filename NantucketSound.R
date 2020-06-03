## ---------------------------
##
## Script name: NantucketSound.R
##
## Purpose of script: Plots sea surface temperature at Buoy 44020 in Nantucket
##    Sound https://www.ndbc.noaa.gov/station_page.php?station=44020
##
## Author: George A. Maynard
##
## Date Created: 2020-06-02
##
## Copyright (c) George Alphonse Maynard, 2020
## Email: galphonsemaynard@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory
if(Sys.getenv()[[8]]=="HOOK-05"){
  setwd("C:/Users/George/Desktop/Autotask Workplace/Common/Research/Seals/Deterrence/BuoyPlot/")
} else {
  setwd(choose.dir())
}
## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(rnoaa)
library(lubridate)
library(dplyr)
library(weathermetrics)
library(data.table)
## ---------------------------

## load up our functions into memory

## ---------------------------

## Select buoy (default is 44020, Nantucket Sound)
b=44020
## Identify years of interest (default is last decade)
y2=year(Sys.Date())
y1=y2-10
## Create a color spectrum for plotting
grays=gray.colors(
  length(seq(y1,y2,1)), 
  start = 0, 
  end = 1
)
grays[1:length(grays)]=grays[seq(length(grays),1,-1)]
## Create an empty dataframe to store all results
fullData=data.frame(
  TIMESTAMP=character(),
  TEMP=double(),
  ORD=integer()
)
for(i in seq(y1,y2,1)){
  ## Download the data from buoy 44020
  data=buoy(
    dataset='stdmet', ## Standard meteorological data
    buoyid=b,
    year=i
    )$data
  ## Remove unnecessary columns and clean up the formatting
  data=select(data,time,sea_surface_temperature)
  data$TIMESTAMP=ymd_hms(as.character(data$time))
  data$TEMP=as.numeric(as.character(data$sea_surface_temperature))
  data$time=NULL
  data$sea_surface_temperature=NULL
  ## Subset out just April, May, and June observatiosn
  data=subset(data,month(data$TIMESTAMP)%in%c(4,5,6))
  ## Convert from Celsius to Farenheit
  data$TEMP=celsius.to.fahrenheit(data$TEMP,round=1)
  ## Add the ordinal date for each observation
  data$ORD=yday(data$TIMESTAMP)+(hour(data$TIMESTAMP)/24+minute(data$TIMESTAMP)/(24*60))
  ## Send year i's data into the full data frame
  fullData=rbind(fullData,data)
  ## If it's the first year, open a graphics device and set up a plot
  if(i==y1){
    jpeg(
      filename=paste("Buoy_",b,"_SST_",Sys.Date(),".jpg",sep=""),
      width=500,
      height=500,
      units="px"
      )
    plot(
      data$TEMP~data$ORD,
      type='n',
      ylim=c(40,80),
      xlim=c(85,185),
      ylab=expression(paste("Sea Surface Temp (",~degree~F,")")),
      xlab="Date",
      main=paste("Buoy: ",b)
      )
  }
  ## Add a line plotting temperature by ordinal date, using darker colors for 
  ## more recent data
  lines(data$TEMP~data$ORD,col=grays[i-y1-1])
  if(i==y2){
    lines(data$TEMP~data$ORD,col='black',lwd=2)
  }
}
## Add the most up to date information (not QAQC'd by NOAA yet)
data=fread(
  "https://www.ndbc.noaa.gov/data/realtime2/44020.txt"
  )
data=data[-1,]
data$TIMESTAMP=ymd_hm(paste(data$`#YY`,data$MM,data$DD,data$hh,data$mm,sep="-"))
data$ORD=yday(data$TIMESTAMP)+hour(data$TIMESTAMP)/24
data$TEMP=data$TEMP=celsius.to.fahrenheit(as.numeric(as.character(data$WTMP)),round=1)
lines(data$TEMP~data$ORD,col='black',lwd=2)
## Add blue month dividers
abline(v=c(91,121,152),lty=2,col='blue')
## Add a red "you are here" line
abline(v=yday(Sys.Date()),lty=2,col='red')
## Close the graphics device to save the plot
dev.off()
## Point the user to the new plot
cat(paste("Your new file for Buoy ",b," is saved at ",getwd(),"/Buoy_",b,"_SST_",Sys.Date(),".jpg",sep=""))
## Read the new data into the fullData frame
data=select(data,TIMESTAMP,ORD,TEMP)
fullData=rbind(fullData,data)
## Assign each observation a whole number ordinal date
fullData$DAY=yday(ymd_hms(as.character(fullData$TIMESTAMP)))
## Assign each observation a year
fullData$YEAR=year(ymd_hms(as.character(fullData$TIMESTAMP)))
## Subset out only those observations not from this year
HIST=subset(fullData,fullData$YEAR!=y2&is.na(fullData$TEMP)==FALSE)
CURR=subset(fullData,fullData$YEAR==y2&is.na(fullData$TEMP)==FALSE)
## Calculate daily averages
HIST=aggregate(
  HIST$TEMP,
  by=list(HIST$DAY),
  FUN='mean'
)
colnames(HIST)=c("ORD","TEMP")
CURR=aggregate(
  CURR$TEMP,
  by=list(CURR$DAY),
  FUN='mean'
)
colnames(CURR)=c("ORD","temp")
CURR=CURR[-1,]
## Merge the historic and current values into one matrix
temps=merge(CURR,HIST)
## Calculate differences
temps$DIFF=temps$temp-temps$TEMP
## Open a new graphics device to create a 
## plot of differences
jpeg(
  filename=paste("Buoy_",b,"_TempDiff_",Sys.Date(),".jpg",sep=""),
  width=500,
  height=500,
  units="px"
)
plot(temps$DIFF~temps$ORD,
  type='l',
  main=paste("Buoy: ",b),
  xlab="Ordinal Date",
  ylab=expression(paste(~Delta,"SST (",~degree~F,")"))
  )
abline(h=0,
  lty=2,
  col='black')
abline(v=yday(Sys.Date()),
  lty=3,
  col='red')
abline(v=c(91,121,152),lty=2,col='blue')
dev.off()