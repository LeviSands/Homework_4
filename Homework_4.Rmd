---
title: "Homework_four"
author: "Levi Sands"
date: "9/28/2021"
output: github_document
---

```{r}
library(nycflights13)
library(tidyverse)
```

```{r}
not_canceled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
```

# What time of day should you fly if you want to avoid delays as much as possible?

## From my analysis we can see that generally flying in the early morning reduces delay time with delays increasing steadily until 10pm. Season was found to have moderate affects on the average delay of flights as summer has the greatest average delays followed by spring, winter, and lastly fall with the lowest average delays. The airport of origin for the flights has minimal affect where EWR has slightly greater delays compared to JFK and LGA. Lastly the airline chosen for the flight where some have greater delays compared to others. Some airlines have less delays compared to the time of day however certain airlines have less flights than others. 

## First we look at the average delay based on the time of day without any other factors.

```{r}
lowest_delay <- not_canceled %>%
  mutate(dep_hour = sched_dep_time %/% 100) %>%
  group_by(dep_hour) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  arrange(avg_delay)
lowest_delay
```

```{r}
ggplot(lowest_delay, aes(x = dep_hour, y = avg_delay)) +
  geom_point() +
  xlab("Hour of Departure") +
  ylab("Average Flight Delay (min)")
```

## Looking at the data and the graph we can say that you want to fly in the morning as early as you can to avoid delays.


## Here I seperate the data points by season to see if that has an affect on flight delay.

```{r}
seasons <- not_canceled %>%
  mutate(season = case_when(month == 12 | month == 1 | month == 2 ~ "Winter",
                            month == 3 | month == 4 | month == 5 ~ "Spring",
                            month == 6 | month == 7 | month == 8 ~ "Summer",
                            month == 9 | month == 10 | month == 11 ~ "Fall"))
seasons %>%
  group_by(season) %>%
  summarize(avg_delay = mean(dep_delay))
```

```{r}
seasons %>%
  mutate(dep_hour = sched_dep_time %/% 100) %>%
  group_by(dep_hour, season) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(x = dep_hour, y = avg_delay, fill = season)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Hour of Departure (24hr)") +
  ylab("Average Flight Delay (min)") +
  ggtitle("Average Flight Delay of the Hour by Season")
```

## We look at the airport of origin to in this section.

```{r}
seasons %>%
  mutate(dep_hour = sched_dep_time %/% 100) %>%
  group_by(origin, dep_hour) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(x = dep_hour, y = avg_delay, fill = origin)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.7) +
  xlab("Hour of Departure (24hr)") +
  ylab("Average Flight Delay (min)") +
  ggtitle("Average Flight Delay of New York City Airports")
```

## This section we look over the average delay time with each ariline carrier.

```{r}
seasons %>%
  group_by(carrier) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  arrange(avg_delay) %>%
  ggplot(aes(x = carrier, y = avg_delay)) +
  geom_bar(stat = "identity") +
  xlab("Airline Carrier") +
  ylab("Average Delay (min)") +
  ggtitle("Average Flight Delay by Airline Carrier")
```

```{r}
seasons %>%
  mutate(dep_hour = sched_dep_time %/% 100) %>%
  group_by(carrier, dep_hour) %>%
  summarize(avg_delay = mean(dep_delay))
```

```{r}
time_of_day <- seasons %>%
  mutate(dep_hour = sched_dep_time %/% 100) %>%
  mutate(time_of_day = case_when(dep_hour <= 10 ~ "Morning",
                                 dep_hour > 10 & dep_hour <= 17 ~ "Midday",
                                 dep_hour > 17 ~ "Evening")) %>%
  group_by(carrier, time_of_day) %>%
  summarize(avg_delay = mean(dep_delay))

ggplot(time_of_day, aes(x = carrier, y = avg_delay, fill = time_of_day)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.7) +
  xlab("Airline Carrier") +
  ylab("Average Flight Delay (min)") +
  ggtitle("Average Flight Delay of Airline Carrier by the Time of Day")
```

```{r}
seasons %>%
  group_by(carrier) %>%
  summarize(number_of_flights = n())
```