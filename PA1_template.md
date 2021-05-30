---
title: "Reproducible Research - Project 1"
author: "Angus Mackenzie"
date: "30/05/2021"
output: 
  html_document:
    keep_md: true
---


```r
## Setup
options(scipen=999)
knitr::opts_chunk$set(echo = TRUE)

## Libraries
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.3     v purrr   0.3.4
## v tibble  3.1.1     v dplyr   1.0.6
## v tidyr   1.1.3     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggrepel)
library(RColorBrewer)
```

## Loading and preprocessing the data

Here is the data, downloaded and unzipped from the course website


```r
## Data
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", "steps.zip")
unzip(zipfile = "steps.zip")

dfstep <- read.csv("activity.csv") %>%
    mutate(date = ymd(date))
```

## What is mean total number of steps taken per day?


```r
## Steps per day
dfstep_pd <- dfstep %>%
  group_by(date) %>%
  summarise(daily_steps = sum(steps, na.rm = TRUE))

daily_steps_mean <- mean(dfstep_pd$daily_steps, na.rm = TRUE)
daily_steps_median <- median(dfstep_pd$daily_steps, na.rm = TRUE)

## Number of intervals
n_interval <- length(unique(dfstep$interval))

## Graph
dfstep_pd %>%
  ggplot(aes(x = daily_steps)) +
  geom_histogram(fill = 'lightblue', binwidth = n_interval) +
  theme_bw()
```

<img src="PA1_template_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

* The mean number of steps per day is 9354.2295082
* The median number of steps per day is 10395

## What is the average daily activity pattern?

```r
## Time series
dfstep_ts <- dfstep %>%
  group_by(interval) %>%
  summarise(interval_steps = mean(steps, na.rm = TRUE))

interval_steps_max <- dfstep_ts$interval[which.max(dfstep_ts$interval_steps)]

## Graph
dfstep_ts %>% 
  ggplot(aes(x = interval, y = interval_steps)) +
  geom_line(color = 'blue') +
  theme_bw()
```

<img src="PA1_template_files/figure-html/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

The 5 minute interval with the greatest average steps is 835

## Imputing missing values

```r
## Missing values
dfstep_na <- dfstep %>%
  filter(is.na(steps))

missing_step_count <- nrow(dfstep_na)

## Fill with mean
dfstep_fill <- dfstep %>%
  mutate(fill_steps = ifelse(is.na(steps), dfstep_ts$interval_steps[match(dfstep_ts$interval, interval)], steps)) %>%
  group_by(date) %>%
  summarise(fill_steps = sum(fill_steps, na.rm = TRUE))

fill_steps_mean <- mean(dfstep_fill$fill_steps, na.rm = TRUE)
fill_steps_median <- median(dfstep_fill$fill_steps, na.rm = TRUE)
fill_total_diff <- sum(dfstep_fill$fill_steps) - sum(dfstep_pd$daily_steps)

## Graph
dfstep_fill %>%
  ggplot(aes(x = fill_steps)) +
  geom_histogram(fill = 'lightblue', binwidth = n_interval) +
  theme_bw()
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

* Filling in missing values with the interval mean gives a different mean number of steps per day of 10766.1886792
* Filling in missing values with the interval mean gives a different median number of steps per day of 10766.1886792
* The total steps taken after filling in missing values is 86129.509434 more than before.

## Are there differences in activity patterns between weekdays and weekends?


```r
## Separate weekdays
vweekday <- c("Mon","Tue","Wed","Thu","Fri")

dfstep_week <- dfstep %>%
  mutate(iday = ifelse(weekdays(date, abbreviate = TRUE) %in% vweekday, "Weekday", "Weekend")) %>%
  group_by(interval, iday) %>%
  summarise(interval_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
## Graph
dfstep_week %>% 
  ggplot(aes(x = interval, y = interval_steps)) +
  facet_wrap(~iday, ncol = 1) +
  geom_line(color = 'blue') +
  theme_bw()
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

