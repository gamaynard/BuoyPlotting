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
## Working directory should already be set to the github repository
## ---------------------------

## Set options
options(scipen = 6, digits = 4) # eliminate scientific notation
## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(rnoaa)
library(lubridate)
library(dplyr)
library(weathermetrics)
## ---------------------------

## load up our functions into memory

## ---------------------------
grays=gray.colors(
  length(seq(2009,2020,1)), 
  start = 0, 
  end = 1
  )
grays[1:length(grays)]=grays[seq(length(grays),1,-1)]
for(i in seq(2009,2019,1)){
  ## Download the data from buoy 44020
  data=buoy(
    dataset='stdmet', ## Standard meteorological data
    buoyid=44020, ## The Nantucket Sound buoy
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
  data$ORD=yday(data$TIMESTAMP)+hour(data$TIMESTAMP)/24
  ## If it's the first year, set up a plot
  if(i==2009){
    plot(
      data$TEMP~data$ORD,
      type='n',
      ylim=c(40,80),
      xlim=c(85,185),
      ylab=expression(paste("Sea Surface Temp (",~degree~F,")")),
      xlab="Date",
      main="Buoy 44020: Nantucket Sound"
      )
  }
  ## Add a line plotting temperature by ordinal date, using darker colors for 
  ## more recent data
  lines(data$TEMP~data$ORD,col=grays[i-2008])
}
## Add the most up to date information (not QAQC'd by NOAA yet)

## Add blue month dividers
abline(v=c(91,121,152),lty=2,col='blue')
## Add a red "you are here" line
abline(v=yday(Sys.Date()),lty=2,col='red')
