Untitled
================

``` r
library(tidyr)   #Create tidy data (each column is a variable)
library(forcats) #Reorder factor levels
library(dplyr)   #Grammar Data Manipulation
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2) #Create Elegant Data Visualizations

setwd("C:/Users/cvill/OneDrive/Escritorio/RStudio Projects/Actividad 1")
datos <- read.csv("pokemon.csv")
```

``` r
data <- select(datos, X., Name, Type.1, Type.2, HP, Attack)
data <- filter(data, Type.1 == "Ghost" | Type.2 == "Ghost")
```

``` r
attach(data)
boxplot(Attack, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Boxplot%20de%20variable%20data-1.png)<!-- -->

``` r
boxplot.stats(Attack)
```

    ## $stats
    ## [1]  30  55  72  95 150
    ## 
    ## $n
    ## [1] 46
    ## 
    ## $conf
    ## [1] 62.68167 81.31833
    ## 
    ## $out
    ## [1] 165

``` r
boxplot(HP, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Boxplot%20de%20variable%20data-2.png)<!-- -->

``` r
boxplot.stats(HP)
```

    ## $stats
    ## [1] 30 49 59 65 89
    ## 
    ## $n
    ## [1] 46
    ## 
    ## $conf
    ## [1] 55.27267 62.72733
    ## 
    ## $out
    ## [1]   1  20  90 150 150 150 100

``` r
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

``` r
data <- filter(data, Attack < 165 & HP > 20)
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

``` r
boxplot(Attack, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Primera%20limpieza-1.png)<!-- -->

``` r
boxplot.stats(Attack)
```

    ## $stats
    ## [1]  30.0  57.5  70.0  93.5 124.0
    ## 
    ## $n
    ## [1] 43
    ## 
    ## $conf
    ## [1] 61.32588 78.67412
    ## 
    ## $out
    ## [1] 150

``` r
boxplot(HP, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Primera%20limpieza-2.png)<!-- -->

``` r
boxplot.stats(HP)
```

    ## $stats
    ## [1] 30.0 50.0 59.0 67.5 90.0
    ## 
    ## $n
    ## [1] 43
    ## 
    ## $conf
    ## [1] 54.78341 63.21659
    ## 
    ## $out
    ## [1] 150 150 150 100

``` r
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 5):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

``` r
data <- filter(data, Attack < 150 & HP < 100)
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 5):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 6):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

``` r
boxplot(Attack, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Segunda%20limpieza-1.png)<!-- -->

``` r
boxplot.stats(Attack)
```

    ## $stats
    ## [1]  30  50  68  90 124
    ## 
    ## $n
    ## [1] 38
    ## 
    ## $conf
    ## [1] 57.74761 78.25239
    ## 
    ## $out
    ## integer(0)

``` r
boxplot(HP, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Segunda%20limpieza-2.png)<!-- -->

``` r
boxplot.stats(HP)
```

    ## $stats
    ## [1] 38.0 49.0 58.5 60.0 75.0
    ## 
    ## $n
    ## [1] 38
    ## 
    ## $conf
    ## [1] 55.68059 61.31941
    ## 
    ## $out
    ## [1] 30 90 89 85 85 80

``` r
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 5):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 6):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 7):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

``` r
data <- filter(data, Attack < 150 & HP > 30)
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 5):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 6):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 7):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 8):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

``` r
boxplot(Attack, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Tercera%20limpieza-1.png)<!-- -->

``` r
boxplot.stats(Attack)
```

    ## $stats
    ## [1]  30  55  70  90 124
    ## 
    ## $n
    ## [1] 37
    ## 
    ## $conf
    ## [1] 60.90874 79.09126
    ## 
    ## $out
    ## integer(0)

``` r
boxplot(HP, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Tercera%20limpieza-2.png)<!-- -->

``` r
boxplot.stats(HP)
```

    ## $stats
    ## [1] 38 50 59 60 75
    ## 
    ## $n
    ## [1] 37
    ## 
    ## $conf
    ## [1] 56.4025 61.5975
    ## 
    ## $out
    ## [1] 90 89 85 85 80

``` r
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 5):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 6):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 7):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 8):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 9):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

``` r
data <- filter(data, Attack < 150 & HP < 80)
attach(data)
```

    ## The following objects are masked from data (pos = 3):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 4):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 5):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 6):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 7):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 8):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 9):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

    ## The following objects are masked from data (pos = 10):
    ## 
    ##     Attack, HP, Name, Type.1, Type.2, X.

``` r
boxplot(Attack, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Cuarta%20limpieza-1.png)<!-- -->

``` r
boxplot.stats(Attack)
```

    ## $stats
    ## [1]  30.0  52.5  66.0  82.5 115.0
    ## 
    ## $n
    ## [1] 32
    ## 
    ## $conf
    ## [1] 57.62078 74.37922
    ## 
    ## $out
    ## integer(0)

``` r
boxplot(HP, horizontal = TRUE)
```

![](Actividad1_files/figure-gfm/Cuarta%20limpieza-2.png)<!-- -->

``` r
boxplot.stats(HP)
```

    ## $stats
    ## [1] 38 47 55 60 75
    ## 
    ## $n
    ## [1] 32
    ## 
    ## $conf
    ## [1] 51.36901 58.63099
    ## 
    ## $out
    ## integer(0)
