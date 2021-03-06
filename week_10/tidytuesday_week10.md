bike trips
================
DanielH
June 9, 2018

-   [read, mutate, transform data](#read-mutate-transform-data)
-   [EDA](#eda)
-   [type of trips](#type-of-trips)

``` r
library(tidyverse)
library(purrrlyr)
library(ggsci)
library(ggthemes)
library(dichromat)
library(wesanderson)
library(gridExtra)
library(lubridate)
library(hms)
```

read, mutate, transform data
----------------------------

``` r
# read data into r, merge into sigle df/tbl
public_trip_tbl <-
  list.files("data", full.names = TRUE) %>%
  map(read_csv, progress = FALSE) %>%
  map(mutate_all, as.character) %>%
  map_dfr(bind_rows)


# create date_time objects where needed
public_trip_tbl <-
  public_trip_tbl %>%
  drop_na(StartDate, EndDate) %>% 
  mutate(StartDate = str_replace_all(StartDate, "\\/", "-"),
         EndDate = str_replace_all(EndDate, "\\/", "-"),
         Duration = parse_time(Duration)) %>%
  unite(StartDate, StartTime, col = "StartTime", sep = " ") %>%
  unite(EndDate, EndTime, col = "Endtime", sep = " "  ) %>% 
  mutate(StartTime = mdy_hms(StartTime),
         Endtime = mdy_hms(Endtime))


# mutate column names and set to lower
colnames(public_trip_tbl)  <- c("Route_ID", "payment_plan", "Start_hub",
                                "Start_latitude", "Start_longitude", 
                                "Start_time", "End_hub", "End_latitude",
                                "End_longitude", "End_time", "Trip_type",
                                "Bike_ID", "Bike_name", "Distance_miles", 
                                "Duration", "Rental_access_path",
                                "Multiple_rental") %>% str_to_lower()


# mutate column types
public_trip_tbl <-
  public_trip_tbl %>%
  dmap_at(11, parse_character,
          na =c("", "NA")) %>%
  dmap_at(11, factor) %>% 
  dmap_at(c(2, 13), str_to_lower) %>% 
  dmap_at(c(4,5,8,9,14), as.double) %>%
  dmap_at(17, as.logical) %>%
  dmap_at(c("payment_plan", "rental_access_path"), 
          factor) 


# create 3 new cols: weekday, month, week
public_trip_tbl <-
  public_trip_tbl %>%
  mutate(weekday = wday(start_time, label = TRUE, 
                        abbr = TRUE),
         weekday = fct_relevel(weekday, 
                               c("Mon", "Tue",
                                 "Wed", "Thu",
                                 "Fri", "Sat",
                                 "Sun")),
         month = month(start_time, label = TRUE,
                       abbr = TRUE),
         week = week(start_time)) %>%
  select(1:5, weekday, month, week, everything())  
```

EDA
---

We can create some plots to see whether there are some patterns in our data

``` r
# plot days of the week
public_trip_tbl %>% 
  group_by(weekday) %>% 
  summarize(tot_trip = n()) %>%
  arrange(desc(tot_trip)) %>%
  ggplot(aes(weekday, tot_trip)) +
  geom_bar(stat = "identity", fill = "darkolivegreen4",
           color = "white", alpha = .5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0,
                                  lineheight = 1),
        plot.subtitle = element_text(size = 11,
                                     face = "italic",
                                     hjust = 0)) +
  
  ylim(0, 100000) +
  labs(title = "Trips peak during the weekend",
       subtitle = "Saturday the busiest day",
       x = "", y = "")
```

![](tidytuesday_week10_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# plot month of the year & plot
public_trip_tbl %>% 
  group_by(month) %>% 
  summarize(tot_trip = n()) %>%
  arrange(desc(tot_trip)) %>%
  ggplot(aes(month, tot_trip)) +
  geom_bar(stat = "identity", fill = "firebrick",
           alpha = .5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 17.88,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0,
                                  lineheight = 1),
        plot.subtitle = element_text(size = 11,
                                     face = "italic",
                                     hjust = 0)) +
  
  labs(title = "Trips peak during Summer Months",
       subtitle= "Winter months are the slowest",
       x = "", y = "")
```

![](tidytuesday_week10_files/figure-markdown_github/unnamed-chunk-3-2.png)

``` r
# average duration by month
public_trip_tbl %>% 
  group_by(month) %>%
  drop_na(duration) %>% 
  summarize(avg_duration = mean(duration)) %>%
  ggplot(aes(month, avg_duration)) +
  geom_bar(stat = "identity", color = "white",
           fill = "darkslategray", alpha = .5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0,
                                  lineheight = 1),
        plot.subtitle = element_text(size = 11,
                                     face = "italic",
                                     hjust = 0)) +
  labs(title = "Longest Trips during Summer",
       subtitle = "Average duration in h:m:s",
       x = "", y = "")
```

![](tidytuesday_week10_files/figure-markdown_github/unnamed-chunk-3-3.png)

type of trips
-------------

``` r
public_trip_tbl %>%
  na.omit() %>% 
  group_by(weekday, trip_type) %>%
  tally() %>% 
  ggplot(aes(weekday, n, fill = trip_type)) +
  geom_bar(stat = "identity", alpha = .78) +
  scale_fill_manual(values=c("firebrick", "steelblue4", "azure4", "orange")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold",
                                  size = 18),
        legend.title = element_blank()) +
  labs(title = "Trips weekday by type", x = "", y ="")
```

![](tidytuesday_week10_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
public_trip_tbl %>%
  na.omit() %>%
  ggplot(aes(trip_type, distance_miles, fill = trip_type)) +
  geom_boxplot(show.legend = F, alpha = .8) +
  scale_fill_manual(values=c("firebrick", "steelblue4", "azure4", "orange")) +
  ylim(0, 16) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = 0,
                                  lineheight = 1),
        plot.subtitle = element_text(size = 11,
                                     face = "italic",
                                     hjust = 0),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.spacing.x = unit(0.2, 'cm')) +
  labs(title = "Distribution of miles by trip type",
       x = "", y ="")
```

![](tidytuesday_week10_files/figure-markdown_github/unnamed-chunk-4-2.png)
