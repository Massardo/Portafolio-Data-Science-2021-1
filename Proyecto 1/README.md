Receta de Sandwich
================

## Carga de librerías y lectura de archivo .csv

``` r
library(quanteda)
```

    ## Package version: 2.1.2

    ## Parallel computing: 2 of 12 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

``` r
library(dplyr)
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
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v stringr 1.4.0
    ## v tidyr   1.1.3     v forcats 0.5.1
    ## v readr   1.4.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(utf8)
library(ggplot2)
library(stringr)

setwd("C:/Users/cvill/OneDrive/Escritorio/RStudio Projects/Proyecto 1")
datos <- read.csv("sanguchez.csv", sep=";")

#str(datos)
#attach(datos)
#summary(datos)
```

## Limpieza de datos

``` r
datos <- na.omit(datos)
datos <- select(datos, Ingredientes:nota)
datos <- filter(datos, nota == "5")
data <- datos$Ingredientes
#data
```

## Análisis de datos

``` r
dfm_data <- dfm(data, remove = stopwords("spanish"), remove_punct = TRUE)
#dfm_data
```

## Resultado del análisis

``` r
final_data <- topfeatures(dfm_data,20)
final_data
```

    ##        queso      cebolla     mayonesa       tomate        salsa          pan 
    ##           33           22           19           18           17           14 
    ##       tocino      lechuga  hamburguesa        palta       morada        carne 
    ##           12           12           11            9            7            7 
    ##          ají          ajo caramelizada        crema       casera       verdes 
    ##            7            6            6            6            6            6 
    ##   mozzarella  champiñones 
    ##            6            5

## Grafico circular

``` r
colores = rainbow(length(final_data))
rotulos <- round(final_data/sum(final_data)*100,1)
rotulos <- paste(rotulos,"%")

pie(final_data, main="Frecuencia de Ingredientes que tienen nota 5", col=colores, labels=rotulos, cex=0.8)


legend(1.2,1.0,cex=0.8,c("Queso","Cebolla","Mayonesa","Tomate","Salsa","Pan","Tocino","Lechuga","Hamburguesa","Palta","Morada","Carne","Aji","Ajo","Caramelizada","Crema","Casera","Verdes","Mozzarella","Champiñones"), fill=colores)
```

![](Proyecto1_files/figure-gfm/Se%20realiza%20un%20gráfico%20circular%20de%20los%2020%20ingredientes%20más%20frecuentes%20para%20conocer%20el%20porcentaje%20de%20participación%20de%20cada%20uno-1.png)<!-- -->

## Gráfico de barras

``` r
barplot(final_data,main="Ingredientes",xlab="Consumo",ylab="Total",border="blue",col=rainbow(5),beside=TRUE,density=c(5,10,15,20))
```

![](Proyecto1_files/figure-gfm/Se%20realiza%20un%20gráfico%20de%20barras%20para%20un%20resultado%20más%20visual%20sobre%20la%20frecuencia%20que%20tiene%20cada%20uno%20de%20los%20ingredientes%20que%20consiguieron%20una%20nota%205-1.png)<!-- -->
\#\# Conclusión final
