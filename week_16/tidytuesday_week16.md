week16
================
DanielH
July 22, 2018

-   [data import](#data-import)
-   [data wrangling](#data-wrangling)
-   [EDA](#eda)

data import
-----------

``` r
# load packages
library(tidyverse)
library(ggalt)
library(ggthemes)
library(scales)
library(purrrlyr)
library(readxl)
library(devtools)

# set theme
theme_set(theme_minimal())


# load raw data
exercise_dat_raw <-
  read_xlsx("data/week16_exercise.xlsx")
```

data wrangling
--------------

``` r
# tidy data
exercise_dat <-
  exercise_dat_raw %>%
  select(-1) %>%
  dmap_at(c(2:9), as.numeric) %>%
  dmap_at(1, as_factor) %>% 
  gather("category", "value", -state) %>%
  mutate(value = value / 100) %>% 
  na.omit()

# check
exercise_dat %>%
  sample_n(5)
```

    ## # A tibble: 5 x 3
    ##   state    category      value
    ##   <fct>    <chr>         <dbl>
    ## 1 Montana  women          0.2 
    ## 2 Georgia  women_working  0.15
    ## 3 Georgia  men            0.27
    ## 4 Virginia women_working  0.2 
    ## 5 Vermont  men            0.35

EDA
---

Here we want to explore our data and create some plots

### exercise levels for US adults

``` r
# plot adults
exercise_dat %>%
  dplyr::filter(category == "adults") %>% 
  ggplot(aes(fct_reorder(state, value), value, 
             color = category, group = state)) +
  geom_lollipop(show.legend = F, colour = "firebrick",
                point.size = 1.25) +
  scale_y_continuous(labels = percent_format(), breaks = c(seq(0, 0.35, 0.05))) +
  labs(title = "Exercise Levels for US Adults",
       subtitle = "% of Americans meeting recommended federal guidelines \nfor both aerobic and muscle-strengthening activities",
       x = "", 
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = .45,
                                  lineheight = 1),
        plot.subtitle = element_text(size = 8.5,
                                     face = "italic",
                                     hjust = .45),
        legend.position = 'top',
        legend.text = element_text(colour="black", 
                                   size = 7,
                                   face = "italic",
                                   hjust = 1),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 7.3),
        axis.text.x = element_text(size = 7.5)) +
  coord_flip()
```

<img src="tidytuesday_week16_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

### exercise levels by gender

``` r
# plot men vs women
exercise_dat %>%
  dplyr::filter(category %in% c("men", "women"),
                !str_detect(state, "average")) %>%  # remove national avg
  spread(category, value)  %>%
  mutate(value = (men + women) / 2) %>% 
  ggplot(aes(y = fct_reorder(state, value),
             group = state)) +
  scale_color_tableau(palette = "Tableau 10",
                      labels = c("men", "women")) +
  geom_point(data = exercise_dat %>%
               dplyr::filter(category %in% c("men", "women"),
                             !str_detect(state, "average")) %>%
               mutate(category = factor(category,
                                        levels = c("women", "men"))),
             aes(x = value, color = category),
             show.legend = TRUE) +
  geom_dumbbell(aes(x = women, xend = men), color = "darkgrey", 
                colour_x = "darkorange", colour_xend = "cadetblue4",
                size_x = 1.3, size_xend = 1.3, show.legend = FALSE)  +
  expand_limits(x = 0) +
  scale_x_continuous(labels = percent_format(), breaks = c(seq(0, 0.4, 0.05))) +
  labs(title = "Exercise Levels for US Adults",
       subtitle = "Percentage meeting recommended federal guidelines for \nboth aerobic and muscle-strengthening activities",
       x = "", 
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = .45,
                                  lineheight = 1),
        plot.subtitle = element_text(size = 8.5,
                                     hjust = .45,
                                     face = "italic"),
        legend.position = 'right',
        legend.text = element_text(colour="black", 
                                   size = 7),
        legend.box.spacing = unit(0.01, "cm"),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 7.3),
        axis.text.x = element_text(size = 7.5)) 
```

<img src="tidytuesday_week16_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

### exericise levels for working men and women

``` r
# plot working men vs working women
exercise_dat %>%
  dplyr::filter(category %in% c("men_working", "women_working"),
                !str_detect(state, "average")) %>%  # remove national avg
  spread(category, value)  %>%
  mutate(value = (men_working + women_working) / 2) %>% 
  ggplot(aes(y = fct_reorder(state, value),
             group = state)) +
  scale_color_tableau(palette = "Tableau 10",
                      labels = c("working \nmen", "working \nwomen")) +
  geom_point(data = exercise_dat %>%
               dplyr::filter(category %in% c("men_working", "women_working"),
                             !str_detect(state, "average")) %>%
               mutate(category = factor(category,
                                        levels = c("women_working", "men_working"))),
             aes(x = value, color = category),
             show.legend = TRUE) +
  geom_dumbbell(aes(x = women_working, xend = men_working), color = "darkgrey", 
                colour_x = "darkorange", colour_xend = "cadetblue4",
                size_x = 1.3, size_xend = 1.3, show.legend = FALSE) +
  expand_limits(x = 0) +
  scale_x_continuous(labels = percent_format(), breaks = c(seq(0, 0.4, 0.05))) +
  labs(title = "Exercise Levels for US Adults",
       subtitle = "% meeting recommended federal guidelines for both aerobic \nand muscle-strengthening activities. Working population",
       x = "", 
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = .45,
                                  lineheight = 1),
        plot.subtitle = element_text(size = 8.5,
                                     hjust = .45,
                                     face = "italic"),
        legend.position = 'right',
        legend.text = element_text(colour="black", 
                                   size = 7),
        legend.box.spacing = unit(0.01, "cm"),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 7.3),
        axis.text.x = element_text(size = 7.5))
```

<img src="tidytuesday_week16_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

### exercise levels for non-working men and non-working women

``` r
# plot working men vs working women
exercise_dat %>%
  dplyr::filter(category %in% c("men_nonworking", "women_nonworking"),
                !str_detect(state, "average")) %>%  # remove national avg
  spread(category, value)  %>%
  mutate(value = (men_nonworking + women_nonworking) / 2) %>% 
  ggplot(aes(y = fct_reorder(state, value),
             group = state)) +
  scale_color_tableau(palette = "Tableau 10",
                      labels = c("non-working \nmen", "non-working \nwomen")) +
  geom_point(data = exercise_dat %>%
               dplyr::filter(category %in% c("men_nonworking", "women_nonworking"),
                             !str_detect(state, "average")) %>%
               mutate(category = factor(category,
                                        levels = c("women_nonworking",
                                                   "men_nonworking"))),
             aes(x = value, color = category),
             show.legend = TRUE) +
  geom_dumbbell(aes(x = women_nonworking, xend = men_nonworking), color = "darkgrey", 
                colour_x = "darkorange", colour_xend = "cadetblue4",
                size_x = 1.3, size_xend = 1.3, show.legend = FALSE) +
  expand_limits(x = 0) +
  scale_x_continuous(labels = percent_format(), breaks = c(seq(0, 0.45, 0.05))) +
  labs(title = "Exercise Levels for US Adults",
       subtitle = "% meeting recommended federal guidelines for both aerobic \nand muscle-strengthening activities. Non-working population",
       x = "", 
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18,
                                  family = "Times",
                                  face = "bold",
                                  color = "black",
                                  hjust = .45,
                                  lineheight = 1),
        plot.subtitle = element_text(size = 8.5,
                                     hjust = .45,
                                     face = "italic"),
        legend.position = 'right',
        legend.text = element_text(colour="black", 
                                   size = 7),
        legend.box.spacing = unit(0.01, "cm"),
        legend.title = element_blank(),
        axis.text.y = element_text(size = 7.3),
        axis.text.x = element_text(size = 7.5))
```

<img src="tidytuesday_week16_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />
