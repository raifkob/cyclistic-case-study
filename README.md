# cyclistic-case-study

---
title: "Cyclistic - Case study"
author: "Mohammed Raif"
date: "2022-09-26"
output: "html_document"
subtitle: "*Google-data-analytics-capstone-project*"
---
<br>

#### **Introduction**

As a part of Google's Data Analytics certification course by Coursera, I have chosen the Cyclistic dataset for my capstone project. For this case study, and to answer all key business questions asked in this project, I will follow the six steps of Google data analysis process which are below:

* Ask
* Prepare
* Process
* Analyze
* Share
* Act

##### **The Scenario**

As a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago, my task is to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, my team will design a new marketing strategy to convert casual riders into annual members.

##### **Characters and Teams**

* Cyclistic: A bike-share program that features more than 5,800 bicycles and 600 docking stations. Cyclistic sets itself apart by also offering reclining bikes, hand tricycles, and cargo bikes, making bike-share more inclusive to people with disabilities and riders who can’t use a standard two-wheeled bike. The majority of riders opt for traditional bikes; about 8% of riders use the assistive options. Cyclistic users are more likely to ride for leisure, but about 30% use them to commute to work each day.

* Lily Moreno: The director of marketing and your manager. Moreno is responsible for the development of campaigns and initiatives to promote the bike-share program. These may include email, social media, and other channels.

* Cyclistic marketing analytics team: A team of data analysts who are responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy. 

* Cyclistic executive team: The notoriously detail-oriented executive team will decide whether to approve the recommended marketing program.

<br>

##### The **Ask** Phase

Business Task: To come up with a marketing strategy and campaigns to convert casual riders to annual members, as annual members are more profitable to the company.

Key stakeholders: 

* Lily Moreno: The director of marketing and my manager.
* Cyclistic executive team: The team which will decide whether to approve the recommended marketing program which will be presented by our marketing team.

<br>

##### The **Prepare** Phase

The main data set has been downloaded and stored in .csv files.

* Data sets have been named following the naming convention.

* Data is current; data from the last twelve months will be used for this analysis i.e. from 09-2021 to 08-2022. 

* Data is complete and accurate for the analysis. However, it requires thorough cleaning as inspected each file in spreadsheet.

* Data is collected and provided by Motivate International Inc.- Primary source

* The data has full credibility because it's reliable, original, comprehensive, current and cited(ROCCC).

<br>

##### The **Process** Phase

##### *Tools used*

* Spreadsheet - All 12 data sets needed inspection to check if all of the data is accurate and reliable.

* R - For data cleaning and manipulation,to analyze the data and for data visualization.

##### *Setting up the environment*

Loading necessary packages:


```{r}
library(tidyverse)
library(ggplot2)
library(lubridate)
```


```{r echo=FALSE}
setwd("C:\\Users\\Aysha\\Downloads\\divvy_tripdata_CSV")

data_01<-read.csv("202109-divvy-tripdata.csv")
data_02<-read.csv("202110-divvy-tripdata.csv")
data_03<-read.csv("202111-divvy-tripdata.csv")
data_04<-read.csv("202112-divvy-tripdata.csv")
data_05<-read.csv("202201-divvy-tripdata.csv")
data_06<-read.csv("202202-divvy-tripdata.csv")
data_07<-read.csv("202203-divvy-tripdata.csv")
data_08<-read.csv("202204-divvy-tripdata.csv")
data_09<-read.csv("202205-divvy-tripdata.csv")
data_10<-read.csv("202206-divvy-tripdata.csv")
data_11<-read.csv("202207-divvy-tripdata.csv")
data_12<-read.csv("202208-divvy-tripdata.csv")
```
**To change working directory and Import raw data .csv files**

setwd("C:\\Users\\Raif\\Downloads\\divvy_tripdata_CSV")

Imported data:

data_01<-read.csv("202109-divvy-tripdata.csv")<br>
data_02<-read.csv("202110-divvy-tripdata.csv")<br>
data_03<-read.csv("202111-divvy-tripdata.csv")<br>
data_04<-read.csv("202112-divvy-tripdata.csv")<br>
data_05<-read.csv("202201-divvy-tripdata.csv")<br>
data_06<-read.csv("202202-divvy-tripdata.csv")<br>
data_07<-read.csv("202203-divvy-tripdata.csv")<br>
data_08<-read.csv("202204-divvy-tripdata.csv")<br>
data_09<-read.csv("202205-divvy-tripdata.csv")<br>
data_10<-read.csv("202206-divvy-tripdata.csv")<br>
data_11<-read.csv("202207-divvy-tripdata.csv")<br>
data_12<-read.csv("202208-divvy-tripdata.csv")<br>

<br>

##### *Data Wrangling and Manipulation*

Binding individual monthly data frames into one big data frame

```{r}
all_data <- rbind(data_01,data_02,data_03,data_04,data_05,data_06,
                  data_07,data_08,data_09,data_10,data_11,data_12)
```
<br>
Inspecting all_data table by using, str,head,glimpse,colnames,summary etc
```{r}
str(all_data) 
head(all_data)
glimpse(all_data)
```
<br>
Rename column names for better understanding
```{r}

all_data <- rename(all_data,
                   user_id=ride_id,
                   bike_type=rideable_type,
                   start_time=started_at,
                   end_time=ended_at,
                   start_station=start_station_name,
                   end_station=end_station_name,
                   membership_type=member_casual)
```
<br>
Verify and delete unnecessary columns
```{r}
colnames(all_data)
all_data<- all_data %>%
  select(-c(start_station_id,end_station_id:end_lng))
```
<br>
Delete all blanks and NA cells
```{r}
all_data[all_data==""]<- NA
clean_data <- na.omit(all_data) 
```
<br>
Add new columns required for the analysis
```{r}
clean_data$start_date <- as.Date(clean_data$start_time)
clean_data$start_month <- format(as.Date(clean_data$start_date),"%B")
clean_data$start_day <- format(as.Date(clean_data$start_date),"%d")
clean_data$start_year <- format(as.Date(clean_data$start_date),"%Y")
clean_data$day_of_week <- format(as.Date(clean_data$start_date),"%A")
```
<br>
Change data type to numeric for start year and start day
```{r}
clean_data$start_day <- as.numeric(as.character(clean_data$start_day))
clean_data$start_year <- as.numeric(as.character(clean_data$start_year))
```
<br>
Reorder columns for better data frame format
```{r}
clean_data_v2 <- clean_data[, c(1, 2, 7, 3, 4, 5, 6, 8, 9, 10, 11, 12)]

```
<br>
Additional column required - length of the ride named as *ride_time* in numeric format
```{r}
clean_data_v2$ride_time <- difftime(clean_data_v2$end_time,clean_data_v2$start_time)

clean_data_v2$ride_time <- as.numeric(as.character(clean_data_v2$ride_time))

```
<br>
Check and delete if there are any negative values in *ride_time* 
```{r}
clean_data_v2 <- arrange(clean_data_v2,ride_time)
clean_data_v2[clean_data_v2 < 0] <- NA  #convert negative values to NA
cleaned_data <- na.omit(clean_data_v2) #deleting NA values
```
<br>

##### The **Analyze** Phase

##### *Descriptive analysis on ride_time*

```{r}
mean(cleaned_data$ride_time) #straight average
median(cleaned_data$ride_time) 
max(cleaned_data$ride_time) #longest ride
min(cleaned_data$ride_time) #shortest ride
```
<br>
All above analysis can be done in one line using summary()
```{r}
summary(cleaned_data$ride_time)
```
*The Average length of the ride is 1080 secs.*
<br>

Compare members and casual users
```{r}
aggregate(cleaned_data$ride_time ~ cleaned_data$membership_type, FUN = mean)
aggregate(cleaned_data$ride_time ~ cleaned_data$membership_type, FUN = median)
aggregate(cleaned_data$ride_time ~ cleaned_data$membership_type, FUN = max)
aggregate(cleaned_data$ride_time ~ cleaned_data$membership_type, FUN = min)
```
*The Average length of the ride for*
casual     -        1547.3404
member     -        752.7599
<br>

Rearrange weekdays and months as per order to do weekly analysis
```{r}
cleaned_data$day_of_week <- ordered(cleaned_data$day_of_week, levels
=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

cleaned_data$start_month <- ordered(cleaned_data$start_month, levels 
= c("January", "February", "March", "April", "May", "June", "July","August","September","October" , "November","December"))
```
<br>

Average ride time by each day and month for members vs casual users
```{r}
aggregate(cleaned_data$ride_time ~ cleaned_data$membership_type +
          cleaned_data$day_of_week, FUN = mean)
aggregate(cleaned_data$ride_time ~ cleaned_data$membership_type +
           cleaned_data$start_month, FUN = mean)
```
*For both as per day and month analysis the average length of the length is much higher for casual riders than members. It is shown in below chart as well.*

<br>
*Weekly analysis*
```{r}
#checking ride_time on weekday basis
cleaned_data %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n())

#Plot-1 Average_duration for every day of the week for members and casual riders. 
cleaned_data%>% 
  group_by(membership_type, day_of_week) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_time)) %>% 
  arrange(membership_type, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill=membership_type))+
  labs(title = "Average duration vs day in a week", subtitle = "Ride time", caption = "Data collected from Divvy")+ 
  geom_col(position = "dodge")

#Plot-2 Number of riders each day of the week for members and casual riders. 
cleaned_data%>% 
  group_by(membership_type, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(membership_type, day_of_week) %>% 
  ggplot(aes(x = day_of_week, y=number_of_rides, fill=membership_type))+
  labs(title = "Number of Rides vs Day in a week", subtitle = "Ride time",caption = "Data collected from Divvy")+
  geom_col(position = "dodge")

```
<br>
*Monthly analysis*
```{r}
#checking ride_time on monthly basis
cleaned_data %>% 
  group_by(start_month) %>%  
  summarise(number_of_rides = n())

#Plot-3 Number of riders per month for members and casual riders. 
cleaned_data %>% 
  group_by(membership_type, start_month) %>%  
  summarise(number_of_rides = n()) %>%
  arrange(membership_type, start_month) %>% 
  ggplot(aes(x = start_month, y = number_of_rides, fill=membership_type))+
  labs(title = "Number of Rides vs Months", subtitle = "Ride time",caption =  
         "Data collected from Divvy")+
  geom_col(position = "dodge")
           
#PLot-4 Average_duration for each month for members and casual riders.
cleaned_data%>% 
  group_by(membership_type, start_month) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_time)) %>% 
  arrange(membership_type, start_month) %>% 
  ggplot(aes(x = start_month, y = average_duration, fill=membership_type))+
  labs(title = "Average duration vs Month", subtitle = "Ride Time",
       caption = "Data collected from Divvy")+ 
  geom_col(position = "dodge")

#Plot-5 Number of rides monthly(time series)
cleaned_data %>% 
  ggplot(mapping = aes(x = start_date, color = membership_type))+
  geom_bar()+
  labs(title = "Number of rides for 12 months", subtitle = "Casual vs Member", 
       caption = "Data collected from Divvy")
```
<br>
```{r}
#Plot-6 The overall rider count based on membership type
cleaned_data %>%
  group_by(membership_type) %>%
  summarise(rider_count = n()) %>% 
  ggplot(aes(x = membership_type, y = rider_count,fill=membership_type )) +
  labs(title = "Number of members and casuals", subtitle = "Casual vs Member", 
       caption = "Data collected from Divvy")+
  geom_col()

```

##### The **Share** Phase

##### *Conclusions/Summary of Insights* 

There is huge difference between members and casual in-terms of how bikes are rented, when bikes are rented and which days the bike are rented.

* Casual rides average duration almost doubles during all week (plot-1) whereas the number of rides is similar to casual and members in weekends i.e on Saturdays and Sundays (plot-2). However during weeks days number of rides shows high increase for the members compare to casuals.
<br>
There is a high probability that the residents / tourists visiting and sightseeing the city is high during weekends and as per chart in plot-2 that most members use the bikes to commute on workdays. 

* Number of rides start to pick up (plot-3) from March and start to decrease in November for Members where as for casuals its from May to September. This correlation is due to the seasonal changes. If you look at (plot-4) the Average duration for casual is double to the average duration of members through out the year with slight variation in middle of the year.
<br>
It shows that the number of rides is high for members due to consistent and short rides on daily basis basically to commute to work, whereas for casuals the rides are longer through out the year.

* Plot-5 shows the variations in increase of number of riders for members and casual riders over a period of year. Most of the subscription happen during the period from June to October where the rest of the months shows huge decrease in the subscriptions.

* More than 50% of the riders are annual members (Plot-6), suggesting that the company has been increasing members gradually however average duration for members are very less than casual riders, which indicates members use the service for specific purpose and doesn't go beyond that.

* Further study to this data can give more insights based on which stations are used by members and casual rider regularly. Hourly based analysis can give more accurate insight for members average ride time.

##### *Recommendations*

* Company can come up with a referral program for its members, where for every new annual membership from their referral gets few minutes of free usage of the rides.

* Giving additional benefits or discounts to the members for using during weekends and for longer rides than their usual rides.

* The marketing campaign should be launched between October to May, as the number of conversion is low during this period and a campaign can give a boost.

* Campaign for weekends should be done to promote longer rides and to increase the usage for members, such as weekends promotions and benefits for members.


##### The **Act** Phase

Taking into consideration each team members' insights and recommendations, Lily Moreno, the director of marketing, will come up with marketing strategies and campaigns which will be presented to the Cyclistic Executive Team, who, in turn, will decide whether to act on it or not.


**_END_**
