---
title: "PA2_4thTemplate"
output:
  html_document:
    pandoc_args: [
      "+RTS", "-K64m",
      "-RTS"
    ]
---
#Reproducible Research: Peer Assessment 2
##Gian Balsamo
###August 15, 2015

##Impact of Severe Weather Events on Population Health, Properties, and Crops in the United States from 1989 to 2011
###(With a "California Intermission"" Devoted to the Effects of the 2010 El Niño)

###Synopsis

1. This study analyzes the U.S. National Oceanic and Atmospheric Administration's storm database, which can be retrived at [Weather Events](http://www.ncdc.noaa.gov/stormevents/ftp.jsp), on the website of the National Climatic Data Center of the NOAA.  
2. The storm database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property and crop damages. 
3. As will soon be explained, the collection of data made available to us, ranging from 1950 to 1988, was considered too sporadic to have its results included in the present study. Hence, the above two questions were tackled by means of the time series ranging from 1989 to 2011.  
4. The present study answers the following two basic questions about the health-related and economic impact of such events: (1) across the United States, which types of events are most harmful with respect to population health? (2) across the United States, which types of events have the greatest economic consequence?  
5. As will soon be explained, the frequency of data collection between 1950 and 1988 was considered too sporadic to be included in the present study; hence, the above two questions were tackled by means of the time series ranging from 1989 to 2011, this latter year being the last one for which data were made available to the author of this study.  
6. In the California Intermission, which consists of a speculative addendum with respect to the overarching, cross-state aim of this study, special attention was devoted to the year 2010 in California because of its relative outlier status: in 2010 a wave of storms and floods hit California owing to El Niño conditions.  
7. Since the El Niño-Southern Oscillation is being forecasted for the coming fall and winter season, it is urgent to start a reflection on the effects which the 2010 El Niño had on California, a state which for the last few years has suffered from a constant drought, and which can be, moreover, as will be seen in regards to 2010, quite vulnerable to floods and flash floods.  
8. Our closing considerations will take advantage of the California Intermission to make some basic comparisons between the 2010 results for California and the previous aggregate results across the United States from 1989 to 2011. 
9. The reader will take notice that on some occasions extra code was provided in addition to the code used in the Results section of this report, to enable him/her to push the analysis further.  


##Data Processing
The data for this study come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size.  
Our first priority at this stage of data processing is to read and load the basic data.  



```r
library(knitr)
opts_chunk$set(echo=TRUE, results='hide',fig.align='center')
```


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
fileUrl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile='./stormData.csv.bz2',method='curl')
stormData <- read.csv("./stormData.csv.bz2", header = TRUE)
```
  
  
Let us verify if the magnitude and thoroughness of data collection justifies using the whole time series in the present study.  



```r
dataFrequency<-as.numeric(format(as.Date(stormData[,"BGN_DATE"],format = "%m/%d/%Y %H:%M:%S"),"%Y"))
```

The following table of frequency of data collection by year indicates that for each decade since 1950, the data collection has dramatically increased in numbers of observations.  



```r
library(xtable)
a<-as.matrix((table(dataFrequency)[c(1,12,22,32,42,52,62)]))
print(xtable(a),type="html")
```

<!-- html table generated in R 3.1.3 by xtable 1.7-4 package -->
<!-- Mon Aug 24 06:36:33 2015 -->
<table border=1>
<tr> <th>  </th> <th> x </th>  </tr>
  <tr> <td align="right"> 1950 </td> <td align="right"> 223 </td> </tr>
  <tr> <td align="right"> 1961 </td> <td align="right"> 2246 </td> </tr>
  <tr> <td align="right"> 1971 </td> <td align="right"> 3471 </td> </tr>
  <tr> <td align="right"> 1981 </td> <td align="right"> 4517 </td> </tr>
  <tr> <td align="right"> 1991 </td> <td align="right"> 12522 </td> </tr>
  <tr> <td align="right"> 2001 </td> <td align="right"> 34962 </td> </tr>
  <tr> <td align="right"> 2011 </td> <td align="right"> 62174 </td> </tr>
   </table>

  
Since 2011 was, in the available data set, the year with the highest number of observations, the following arbitrary threshold of data usage was established: the data of interest to this study would be those pertaining to years when the frequency of data collection was at least greater than 1/6 compared with that of 2011.  



```r
library(xtable)
c<-table(dataFrequency)
b<-(c[(c/c[length(c)])>1/6])
```


Therefore, the above code helped us select the first year of our adopted time series as:
1989.  

On such a premise, let us create a data table functional to the analysis of the health-related and economic impacts of weather events across the United States between 1989 and 2011.


```r
library(dplyr)
rows<-filter(stormData,as.numeric(format(as.Date(stormData[,"BGN_DATE"],format = "%m/%d/%Y %H:%M:%S"),"%Y"))==c(1989:2011))
```

```
## Warning in filter_impl(.data, dots): longer object length is not a multiple
## of shorter object length
```

```r
myStormData<-select(rows,c(EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP))
myStormData<-tbl_df(myStormData)
```

We must now shape up our data table so that it will permit the calculations of damages to crop and properties. In order to achieve this goal, we must operate on two categorical variables, namely, PROPDMG and CROPDMG. These two variables report damages in various orders of units. To be quantified into $ amounts, these units must be multiplied by the values, respectively, of PROPDMGEXP and CROPDMGEXP. However, the values of the two latter variables are not numeric but symbolic, and must therefore be properly manipulated. 



```r
library(dplyr)
library(ggplot2)
unique_prop2_measure<-unique(myStormData$PROPDMGEXP)
unique_crop2_measure<-unique(myStormData$CROPDMGEXP)
lunghezza_prop<-length(unique_prop2_measure)
lunghezza_crop<-length(unique_crop2_measure)
```

PROPDMGEXP includes 8 values, namely:  
, M, K, 0, m, 5, 3, B. You will have noticed that one value is left blank in this list. In $ amounts, these values correspond, respectively, to the following vector of values:  
[(0,1000000,1000,0, 1000000,100000,1000,1000000000].  
The value zero was attributes to PROPDMGEXP's indeterminate value, namely, the blank one in the list.   
In turn, CROPDMGEXP includes 5 values, namely: , K, M, k, 0. Here too we find a blank value. In $ amounts, these values correspond, respectively, to the following vector of values: [0,1000,1000000,1000,0].  
In this case as well, the value zero was attributes to the indeterminate value in the list.  
The following code manipulates the above data in order to add two columns to our data table. These two columns will be instrumental in quantifying the weather-inflicted damages to properties and crops, as we will multiply their respective values by the pertinent values of PROPDMG and CROPDMG.



```r
x<-c(0,1000000,1000,0, 1000000,100000,1000,1000000000)
vettore1<-as.vector(x)
y<-c(0,1000,1000000,1000,0)
vettore2<-as.vector(y)
lungo<-dim(myStormData)[1]
vettore<-rep(0,lungo)
myStormData2<-mutate(myStormData, multiplier_prop=vettore, multiplier_crop=vettore)
for (i in 1:lunghezza_prop) {myStormData2[myStormData2$PROPDMGEXP==unique_prop2_measure[i],"multiplier_prop"]<-vettore1[i]}
for (i in 1:lunghezza_crop) {myStormData2[myStormData2$CROPDMGEXP==unique_crop2_measure[i],"multiplier_crop"]<-vettore2[i]}
myStormData3<-mutate(myStormData2, Property_Damage=PROPDMG * multiplier_prop, Crop_Damage= CROPDMG * multiplier_crop)
```

Let us now make sure that missing data won't be an obstacle to our forecoming analyses.  


```r
a<-is.na(myStormData3)
b<-sum(a)
```

There are 0 NA values in our final data table, so missing values will be of no concern.

Let us create our four relevant variables, namely, "Fatalities", "Injuries", "propDamgs" (i.e., Damages to Properties), and "cropDmgs" (i.e., Damages to Crops), each one grouped by type of weather event, or "EVTYPE".



```r
myStormData_grouped<-group_by(myStormData3,EVTYPE)
finalData<-summarize(myStormData_grouped,Fatalities=sum(FATALITIES), Injuries=sum(INJURIES),propDmgs=sum(Property_Damage), cropDmgs=sum(Crop_Damage))
```

At this stage of data processing, our priority is to extract the data required to create a data table functional to the analysis of weather events' impacts on California in 2010.  
The methodology will be the same as the one adopted thus far for the manipulation of aggregate data.



```r
library(lubridate)
CaliforniaData<-select(stormData,c(STATE,BGN_DATE, EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP))
CaliforniaData<-CaliforniaData[CaliforniaData$STATE=="CA",]
CaliforniaData<-tbl_df(CaliforniaData)
a<-CaliforniaData$BGN_DATE
b<-as.character(a)
date_column<-mdy_hms(b)
date_column_by_year<-year(date_column)
true_date_column_2010<-date_column_by_year==2010
CaliforniaData_2010<-CaliforniaData[true_date_column_2010,]
CaliforniaData_2010<-select(CaliforniaData_2010,c(EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP))
unique_prop2_measure_2010<-unique(CaliforniaData_2010$PROPDMGEXP)
unique_crop2_measure_2010<-unique(CaliforniaData_2010$CROPDMGEXP)
lunghezza_prop_2010<-length(unique_prop2_measure_2010)
lunghezza_crop_2010<-length(unique_crop2_measure_2010)
j<-c(1000,1000000)
vettoreJ<-as.vector(j)
lungo_2010<-dim(CaliforniaData_2010)[1]
vettore<-rep(0,lungo_2010)
CaliforniaData2_2010<-mutate(CaliforniaData_2010, multiplier_prop=vettore, multiplier_crop=vettore)
for (i in 1:lunghezza_prop_2010) {CaliforniaData2_2010[CaliforniaData2_2010$PROPDMGEXP==unique_prop2_measure_2010[i],"multiplier_prop"]<-vettoreJ[i]}
for (i in 1:lunghezza_crop_2010) {CaliforniaData2_2010[CaliforniaData2_2010$CROPDMGEXP==unique_crop2_measure_2010[i],"multiplier_crop"]<-vettoreJ[i]}
grouped_CaliforniaData2_2010<-group_by(CaliforniaData2_2010,EVTYPE)
CaliforniaData3_2010<-mutate(grouped_CaliforniaData2_2010, propDmgs=PROPDMG * multiplier_prop, cropDmgs= CROPDMG * multiplier_crop)
finalCalifornia_2010<-summarize(CaliforniaData3_2010,Fatalities=sum(FATALITIES), Injuries=sum(INJURIES),propDmgs=sum(propDmgs), cropDmgs=sum(cropDmgs))
```

As usual, let us check for missing values.

```r
a<-is.na(head(finalCalifornia_2010))
b<-sum(a)
```
There are 0 NA values in our table for 2010, so missing values will be of no concern in our California Intermission.

##Results

```r
max_disaster_fatalities<-finalData[which.max(finalData$Fatalities),]
max_disaster_injuries<-finalData[which.max(finalData$Injuries),]
```

From running the above code, we learn that across the United States, the weather events classified as HEAT were responsible for the highest number of fatalities from 1989 to 2011, for a total of 609 victims. The events classified as FLOOD were responsible for the highest number of injuries, for a total of 1319 victims.  

The following code will enable us to draw two helpful figures.  



```r
library(ggplot2)
finalDataFat<-arrange(finalData,desc(Fatalities))
finalDataInj<-arrange(finalData,desc(Injuries))
finalDataPropDmgs<-arrange(finalData,desc(propDmgs))
finalDataCropDmgs<-arrange(finalData,desc(cropDmgs))

plot1<-ggplot(finalDataFat[1:10,c("EVTYPE","Fatalities")], aes(x = EVTYPE, y = Fatalities)) + geom_bar(stat = "identity",col="red", aes(fill=Fatalities),binwidth=1.5) + ylab("Fatalities") + xlab("Weather Event")+ ggtitle("Ten Highest Fatality Causes: \n  1989-2011") + scale_fill_continuous("Fatalities", low = "red", high = "yellow") + theme(axis.text.x=element_text(angle=55,hjust=1.2))

plot2<-ggplot(finalDataInj[1:10,c("EVTYPE","Injuries")], aes(x = EVTYPE, y = Injuries)) + geom_bar(stat = "identity",col="red", aes(fill=Injuries),binwidth=1.5) + ylab("Injuries") + xlab("Weather Event")+ ggtitle("Ten Highest Injury Causes: \n  1989-2011") + scale_fill_continuous("Injuries", low = "green", high = "blue") + theme(axis.text.x=element_text(angle=55,hjust=1.2))

plot3<-ggplot(finalDataPropDmgs[1:10,c("EVTYPE","propDmgs")], aes(x = EVTYPE, y = propDmgs)) + geom_bar(stat = "identity",col="red", aes(fill=propDmgs),binwidth=1.5) + ylab("Property Damage") + xlab("Weather Event")+ ggtitle("Largest Property-Damage Causes: \n  1989-2011") + scale_fill_continuous("propDmgs", low = "red", high = "yellow") + theme(axis.text.x=element_text(angle=55,hjust=1.2))

plot4<-ggplot(finalDataCropDmgs[1:10,c("EVTYPE","cropDmgs")], aes(x = EVTYPE, y = cropDmgs)) + geom_bar(stat = "identity",col="red", aes(fill=cropDmgs),binwidth=1.5) + ylab("Crop Damage") + xlab("Weather Event")+ ggtitle(" Largest Crop-Damage Causes: \n  1989-2011") + scale_fill_continuous("cropDmgs", low = "green", high = "blue")+ theme(axis.text.x=element_text(angle=55,hjust=1.2))  
```


In the light of the following figure, containing two panels, it is easy to confirm the above remark that the events classified as HEAT were by far the leading cause of fatalities in the time period under examination, followed by the events classified as EXCESSIVE HEAT and as TORNADO.
As to injuries, the events classified as FLOOD hold the first place by a small margin, followed, at a virtually equal order of magnitude, by those classified as TORNADO.


```r
library(gridExtra)
ratio<-finalDataPropDmgs[1,"propDmgs"]/finalDataCropDmgs[1,"cropDmgs"]
grid.arrange(plot1, plot2, ncol = 2)
```

<img src="figure/do graph 1-1.png" title="plot of chunk do graph 1" alt="plot of chunk do graph 1" style="display: block; margin: auto;" />

In the light of the following figure, containing two panels, it is easy to see that the events classified as FLOOD were by far the worst cause of damages to properties, followed, on a proportionally microscopic order of magnitude, by the events classified as TORNADO and as FLASH FLOOD.  
As to damages to crops, the events classified as DROUGHT were the leading cause, followed by the events classified as FLOOD and as EXCESSIVE WETNESS.
But the difference in the order of magnitude between the largest damages to properties and to crops is impressive: the largest damages to properties total a figure which is 152.043416 greater than the largest damages to crops. Here are the graphs that tell this story:


```r
grid.arrange(plot3, plot4, ncol = 2)
```

<img src="figure/do graph 2-1.png" title="plot of chunk do graph 2" alt="plot of chunk do graph 2" style="display: block; margin: auto;" />

##CALIFORNIA INTERMISSION

As explained in the synopsis, we devoted special attention to 2010 in the light of its peculiarities. Owing to the El-Niño forecasted weather conditions for the coming fall and winter seasons, the analysis of the 2010 El-Niño's impact on health-related and economic variables in a state such as Califonia, which is vulnerable to floods and going moreover through a long-lasting drought, is of doubtless interest. 
The following code will complete our data manipulation in order to draw graphs of, respectively, the ten weather events that caused the highest numbers of injuries and the largest damages to properties in California in 2010. Moreover, the following code will supply us with the data to illustrate some the El-Niño's impact on fatalities. Damages to crops cannot be discussed in reliable detail since it turns out that the pertinent California data for 2010 were poorly collected. (As explained in the synopsis, supplementary code is being provided in case the reader wants to investigate further aspects of the matter under discussion.)



```r
finalCalifornia_2010Fat<-arrange(finalCalifornia_2010,desc(Fatalities))
finalCalifornia_2010Inj<-arrange(finalCalifornia_2010,desc(Injuries))
finalCalifornia_2010Prop<-arrange(finalCalifornia_2010,desc(propDmgs))
finalCalifornia_2010Crop<-arrange(finalCalifornia_2010,desc(cropDmgs))
ten_top_fat<-finalCalifornia_2010Fat[1:10,c(1,2)]
ten_top_inj<-finalCalifornia_2010Inj[1:10,c(1,3)]
ten_top_propDmgs<-finalCalifornia_2010Prop[1:10,c(1,4)]
ten_top_cropDmgs<-finalCalifornia_2010Crop[1:10,c(1,5)]
```


```r
library(ggplot2)
plot5<-ggplot(ten_top_fat, aes(x = EVTYPE, y = Fatalities)) + geom_bar(stat = "identity",col="red", aes(fill=Fatalities),binwidth=1.5) + ylab("Fatalities")+ xlab("Weather Event")+ ggtitle("Top Fatalities in California: 2010") + scale_fill_continuous("Fatalities", low = "red", high = "yellow") + theme(axis.text.x=element_text(angle=55,hjust=1.2))
plot6<-ggplot(ten_top_inj, aes(x = EVTYPE, y = Injuries)) + geom_bar(stat = "identity",col="red", aes(fill=Injuries),binwidth=1.5) + ylab("Injuries") + xlab("Weather Event")+ ggtitle("Top Injuries in California: 2010") + scale_fill_continuous("Injuries", low = "green", high = "blue") + theme(axis.text.x=element_text(angle=55,hjust=1.2))
plot7<-ggplot(ten_top_propDmgs, aes(x = EVTYPE, y = propDmgs)) + geom_bar(stat = "identity",col="red", aes(fill=propDmgs),binwidth=1.5) + ylab("Property Damage") + xlab("Weather Event")+ ggtitle("Top Property Damages in California: 2010") + scale_fill_continuous("propDmgs", low = "red", high = "yellow") + theme(axis.text.x=element_text(angle=55,hjust=1.2))
plot8<-ggplot(ten_top_cropDmgs, aes(x = EVTYPE, y = cropDmgs)) + geom_bar(stat = "identity",col="red", aes(fill=cropDmgs),binwidth=1.5) + ylab("Crop Damage") + xlab("Weather Event")+ ggtitle("Top Crop Damages in California: 2010") + scale_fill_continuous("cropDmgs", low = "green", high = "blue") + theme(axis.text.x=element_text(angle=55,hjust=1.2))
massimoPropDmgs<-ten_top_propDmgs$propDmgs[1]
secondPropDmgs<-ten_top_propDmgs$propDmgs[2]
thirdPropDmgs<-ten_top_propDmgs$propDmgs[3]
fourthdPropDmgs<-ten_top_propDmgs$propDmgs[4]
massimocropDmgs<-ten_top_cropDmgs$cropDmgs[1]
secondCropDmgs<-ten_top_cropDmgs$cropDmgs[2]
thirdCropDmgs<-ten_top_cropDmgs$cropDmgs[3]
massimoFat<-ten_top_fat$Fatalities[1]
secondFat<-ten_top_fat$Fatalities[2]
thridFat<-ten_top_fat$Fatalities[3]
fouthFat<-ten_top_fat$Fatalities[4]
massimoInj<-ten_top_inj$Injuries[1]
secondInj<-ten_top_inj$Injuries[2]
thirdInj<-ten_top_inj$Injuries[3]
```

As seen in the following figure, containing two plots, the highest number of injuries in California was caused by the events classified as HIGH SURF in 2010, followed by the events classified as WILDFIRE and as STRONG WIND.  
In turn, the worst damages to properties were caused by the evenys classified as FLOOD, followed by those classified as FLASH FLOOD and as HIGH SURF.    
The differences with respect to our previous results across the US and on the time series 1950-2011 are not insignificant, and oughtn't to be neglected by policy makers.   



```r
s<-CaliforniaData_2010$CROPDMG>0
availableCropDmgs<-sum(s)
grid.arrange(plot6, plot7, ncol = 2)
```

<img src="figure/do multiplot-1.png" title="plot of chunk do multiplot" alt="plot of chunk do multiplot" style="display: block; margin: auto;" />

##CLOSING CONSIDERATIONS
Let us take a brief look at the differences between the overall data across the US and the 2010 data for California.  
Across the US from 1989 to 2010, floods were a major cause among weather events as regards injuries and property damages, while their virtual counterparts, heat and drought, were the major causes in fatalities and crop damages respectively.  
In 2010, California saw high surfs as the major source of fatalities and injuries, while floods and/or flash floods were the major sources of economic damages.  
This seems to confirm our initial hypothesis that the 2010 El-Niño's impact on health-related and economic variables in a state such as Califonia was statistically significant. Furthermore, it suggests that the current, long-lasting drought in California could be reversed by the new, imminent El-Niño, provided proper measures of water conservation be put rapidly into operation.  
With more time and space available, as a matter of course, one would want to make punctual comparisons between the 2010 data and the whole 1989-2010 time series for Californa, as well as analyze the El-Niño's effects on aggregate data for some groups of El-Niño-wise relevant states: for instance, the whole of the US West Coast.  
A final caveat: It would seem that the 2010 California data for damages to crops are scarcely reliable, as the entire data set for California contains only 1 datum. According to this datum, as we said, flash floods caused $ 3 &times; 10<sup>6</sup> in damages to crops. Yet, the fact that regarding damages to crops in 2010 in California, data are not available for any other weather event suggests that we are dealing here with an unreliable source. 
