---
title: "Cyclist"
author: "Parveen Kaur"
date: "2023-02-15"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Problem to solve

A bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data.

## Analysis
How casual riders and annual member use the bikes?

## Get 12 months data from <https://divvy-tripdata.s3.amazonaws.com/index.html>
Download all the 12 .csv files on local system than use R to combine data into single file for further use.


```{r installing packages }

install.packages('tidyverse')
install.packages(c("plyr","readr"))
```
```{r load packages}

library(plyr)
library(readr)
library(tidyverse)
library( lubridate )
library(ggplot2)
library(data.table)
library(scales)
library(gtable)
```
 Combine all the csv into single csv on local system and than upload the complete data here
 in file name New_Combined_File.csv


```{r readfiles}
# Read all the files from folder
df<-read_csv("../input/12-months-merged-data/New_Combined_File.csv",show_col_types = FALSE)

```
```{r check current data frame}
# check is the current data is data frame
print(is.data.frame(df))
print(ncol(df))
print(nrow(df))
```
Add columns ride_length,day_of_week,month to the current data frame
```{r Add columns}
df$ride_length <- abs(difftime(df$ended_at,df$started_at,units="mins"))
df$day_of_week <- weekdays(df$started_at)

df$month <- month(df$started_at)
print(head(df,10))
```

```{r}
# remove this data with 0 min ride_length and calculate max,min ride_length again
delete_Rows_with_zero_mins <- df%>%
filter(!if_any(ride_length,~.==0))
# Calculate max, min and mean rider length
max <- max(df$ride_length)
print(max)
min<- min(df$ride_length)
print(min)
mean <- mean(df$ride_length)
print(mean)

```

```{r After deletion check casual and annual member}
#Create table for casual and annual members
table(delete_Rows_with_zero_mins["member_casual"] )

```
```{r count classic, docked,electric bike}
# table for rideable_type
table(delete_Rows_with_zero_mins["rideable_type"])

```
```{r  rename the data frame}
#assing new data fram to rename it
df_clear_data<- delete_Rows_with_zero_mins

```
## Including Plots

Show total number of rider per weekday

```{r pressure}
# plot Number of rides per day

ggplot(data=df_clear_data,aes(x= day_of_week))+
  geom_bar(mapping= aes (fill=rideable_type))+
stat_count(geom="text",aes(label=after_stat(count)),size= 2.5,position= position_stack(vjust=1.1))+
labs(title="Number of rides per week day with different bike types", x="Day of Week",y="Total Riders",fill="Bike Type")+
  scale_x_discrete(limits= c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
