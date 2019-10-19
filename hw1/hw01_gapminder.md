---
title: "Exploring Gapminder Dataset (HW1 - STAT 545)"
author: "Phuong (Sam) Can"
date: "12/09/2019"
output: 
  pdf_document:
    toc: yes
    keep_md: yes
  toc_float: yes
---



First, we load a dataset called 'gapminder' in R.


```
##         country        continent        year         lifeExp     
##  Afghanistan:  12   Africa  :624   Min.   :1952   Min.   :23.60  
##  Albania    :  12   Americas:300   1st Qu.:1966   1st Qu.:48.20  
##  Algeria    :  12   Asia    :396   Median :1980   Median :60.71  
##  Angola     :  12   Europe  :360   Mean   :1980   Mean   :59.47  
##  Argentina  :  12   Oceania : 24   3rd Qu.:1993   3rd Qu.:70.85  
##  Australia  :  12                  Max.   :2007   Max.   :82.60  
##  (Other)    :1632                                                
##       pop              gdpPercap       
##  Min.   :6.001e+04   Min.   :   241.2  
##  1st Qu.:2.794e+06   1st Qu.:  1202.1  
##  Median :7.024e+06   Median :  3531.8  
##  Mean   :2.960e+07   Mean   :  7215.3  
##  3rd Qu.:1.959e+07   3rd Qu.:  9325.5  
##  Max.   :1.319e+09   Max.   :113523.1  
## 
```

# Examine the dataset overall

The dataset contains `1704` rows and `6` columns.

We have the following variables:


```
## [1] "country"   "continent" "year"      "lifeExp"   "pop"       "gdpPercap"
```

The number of countries in the dataset is `142`.
The number of years in the dataset is `12`.

# Find answers to specific questions related to life expectancy and GDP per capita

1. What was the mean life expectancy across all countries throughout the years in the dataset? 

It is:


```
## [1] 59.47444
```

2. What was the change of the median GDP per capita over time?

We need to summarize the median GDP per capita within each continent within each year and plot the results to show the answer.

![](hw01_gapminder_files/figure-latex/unnamed-chunk-3-1.pdf)<!-- --> 

Based on the plot, we see that Oceania and Europe had the highest rate of increase in median GDP per capita, followed by Americas, Asia, and Africa. 
