Untitled
================
DanielH
December 13, 2018

-   [load packages data](#load-packages-data)
-   [data wrangling](#data-wrangling)
-   [EDA](#eda)

load packages data
------------------

``` r
# load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(ggalt)
library(ggthemes)
library(scales)
library(purrrlyr)
library(readxl)
library(devtools)

# load raw data
URL <-
  "https://data.cityofnewyork.us/api/views/43nn-pn8j/rows.csv?accessType=DOWNLOAD"

inspection_dat_raw <-
  URL %>%
  read_csv()
```

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 1)

    ## Warning: 4 parsing failures.
    ## row # A tibble: 4 x 5 col      row col     expected  actual file                                     expected    <int> <chr>   <chr>     <chr>  <chr>                                    actual 1   3084 ZIPCODE an integ~ N/A    'https://data.cityofnewyork.us/api/view~ file 2  17365 ZIPCODE an integ~ N/A    'https://data.cityofnewyork.us/api/view~ row 3  77489 ZIPCODE an integ~ N/A    'https://data.cityofnewyork.us/api/view~ col 4 215766 ZIPCODE an integ~ N/A    'https://data.cityofnewyork.us/api/view~

``` r
# problems
inspection_dat_raw %>%
  problems()
```

    ## # A tibble: 4 x 5
    ##      row col     expected  actual file                                    
    ##    <int> <chr>   <chr>     <chr>  <chr>                                   
    ## 1   3084 ZIPCODE an integ~ N/A    'https://data.cityofnewyork.us/api/view~
    ## 2  17365 ZIPCODE an integ~ N/A    'https://data.cityofnewyork.us/api/view~
    ## 3  77489 ZIPCODE an integ~ N/A    'https://data.cityofnewyork.us/api/view~
    ## 4 215766 ZIPCODE an integ~ N/A    'https://data.cityofnewyork.us/api/view~

data wrangling
--------------

``` r
# clean variables names
inspection_dat <-
  inspection_dat_raw %>%
  clean_names() 

# data manipulation
inspection_dat <-
  inspection_dat %>%
  mutate(zipcode = parse_character(zipcode, na = c("", "NA"))) %>%  # fix the 4 problems
  dmap_at(c(3, 5), str_to_title) %>%
  mutate_at(c(9,16,17), mdy) %>%
  select(-c(phone, grade_date, record_date, building, street))
```

EDA
---

We can start by looking at the number of inspections per restaurant

``` r
inspection_dat %>%
  count(dba, sort = T)
```

    ## # A tibble: 21,374 x 2
    ##    dba                                       n
    ##    <chr>                                 <int>
    ##  1 DUNKIN' DONUTS                         4026
    ##  2 SUBWAY                                 3132
    ##  3 MCDONALD'S                             2289
    ##  4 STARBUCKS                              2110
    ##  5 KENNEDY FRIED CHICKEN                  1338
    ##  6 DUNKIN' DONUTS, BASKIN ROBBINS         1316
    ##  7 DOMINO'S                               1104
    ##  8 CROWN FRIED CHICKEN                    1022
    ##  9 BURGER KING                             973
    ## 10 GOLDEN KRUST CARIBBEAN BAKERY & GRILL   878
    ## # ... with 21,364 more rows

As we can see a lot of the restaurants a aboeve are chains, so we want to investigate at the single restaurant level

``` r
inspection_dat %>%
  count(dba, camis, sort = T)
```

    ## # A tibble: 26,876 x 3
    ##    dba                                        camis     n
    ##    <chr>                                      <int> <int>
    ##  1 RESTAURANTE & PANADERIA GUATELINDA      50001789    94
    ##  2 MAX BAKERY & RESTAURANT                 41683816    91
    ##  3 J J NOODLE                              41630632    81
    ##  4 PARTY WELL RESTAURANT & ORIENTAL BAKERY 50033122    79
    ##  5 BIG WONG RESTAURANT                     50035784    77
    ##  6 GOLDEN HOUSE RESTAURANT                 50012165    75
    ##  7 HUNAN BISTRO                            50045240    74
    ##  8 NEW SHANGHAI TIDE RESTAURANT            50036890    74
    ##  9 CAFE EXCHANGE                           41046492    73
    ## 10 242 CAFE BAKERY                         41642333    72
    ## # ... with 26,866 more rows

What period of time (year) are we looking at?

``` r
inspection_dat %>%
  count(year(inspection_date), sort = T)
```

    ## # A tibble: 9 x 2
    ##   `year(inspection_date)`      n
    ##                     <dbl>  <int>
    ## 1                    2018 144718
    ## 2                    2017 100015
    ## 3                    2016  94478
    ## 4                    2015  43617
    ## 5                    1900   1019
    ## 6                    2014    609
    ## 7                    2013      9
    ## 8                    2012      2
    ## 9                    2011      1

It looks like most of the inspections are relative to years 2015-2018 The difference could be due simply to selection bias, in other words, there could be more data available.

We now look at year and month

``` r
inspection_dat %>%
  count(year(inspection_date), month(inspection_date), sort = T) %>% 
  rename(year = "year(inspection_date)",
         month = "month(inspection_date)")
```

    ## # A tibble: 63 x 3
    ##     year month     n
    ##    <dbl> <dbl> <int>
    ##  1  2018    10 15103
    ##  2  2018     3 13931
    ##  3  2018     5 13645
    ##  4  2018     6 13319
    ##  5  2018     8 12932
    ##  6  2018     4 12709
    ##  7  2018    11 12204
    ##  8  2018     2 12113
    ##  9  2018     1 11940
    ## 10  2018     9 11473
    ## # ... with 53 more rows

Let's take a look at grades received

``` r
inspection_dat %>%
  count(grade, sort = T)
```

    ## # A tibble: 8 x 2
    ##   grade               n
    ##   <chr>           <int>
    ## 1 <NA>           190421
    ## 2 A              153379
    ## 3 B               24627
    ## 4 C                7573
    ## 5 Z                4144
    ## 6 P                2332
    ## 7 Not Yet Graded   1991
    ## 8 G                   1

We now look at `violation_description`

``` r
inspection_dat %>%
  count(violation_code, violation_description, sort = T) 
```

    ## # A tibble: 100 x 3
    ##    violation_code violation_description                                  n
    ##    <chr>          <chr>                                              <int>
    ##  1 10F            Non-food contact surface improperly constructed. ~ 53768
    ##  2 08A            Facility not vermin proof. Harborage or condition~ 41293
    ##  3 04L            Evidence of mice or live mice present in facility~ 27751
    ##  4 06D            Food contact surface not properly washed, rinsed ~ 25922
    ##  5 06C            Food not protected from potential source of conta~ 25451
    ##  6 02G            Cold food item held above 41Âº F (smoked fish and~ 22906
    ##  7 10B            Plumbing not properly installed or maintained; an~ 22651
    ##  8 04N            "Filth flies or food/refuse/sewage-associated (FR~ 21096
    ##  9 02B            Hot food item not held at or above 140Âº F.        19742
    ## 10 04H            Raw, cooked or prepared food is adulterated, cont~  8270
    ## # ... with 90 more rows

Can an inspection lead to multiple violations?

We now look
