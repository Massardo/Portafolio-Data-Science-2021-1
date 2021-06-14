Ayudantia 10 Actividad
================

## Librerias

``` r
library(tidyverse)
library(e1071)
library(caret)
library(rstan)
library(rstanarm)
```

## Datos

``` r
setwd("C:/Users/cvill/OneDrive/Escritorio/RStudio Projects/Ayudantia 10")
banco <- read.csv("UCI_Credit_Card.csv")
```

``` r
glimpse(banco)
```

    ## Rows: 30,000
    ## Columns: 25
    ## $ ID                         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, ~
    ## $ LIMIT_BAL                  <dbl> 20000, 120000, 90000, 50000, 50000, 50000, ~
    ## $ SEX                        <int> 2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1~
    ## $ EDUCATION                  <int> 2, 2, 2, 2, 2, 1, 1, 2, 3, 3, 3, 1, 2, 2, 1~
    ## $ MARRIAGE                   <int> 1, 2, 2, 1, 1, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2~
    ## $ AGE                        <int> 24, 26, 34, 37, 57, 37, 29, 23, 28, 35, 34,~
    ## $ PAY_0                      <int> 2, -1, 0, 0, -1, 0, 0, 0, 0, -2, 0, -1, -1,~
    ## $ PAY_2                      <int> 2, 2, 0, 0, 0, 0, 0, -1, 0, -2, 0, -1, 0, 2~
    ## $ PAY_3                      <int> -1, 0, 0, 0, -1, 0, 0, -1, 2, -2, 2, -1, -1~
    ## $ PAY_4                      <int> -1, 0, 0, 0, 0, 0, 0, 0, 0, -2, 0, -1, -1, ~
    ## $ PAY_5                      <int> -2, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, -1, ~
    ## $ PAY_6                      <int> -2, 2, 0, 0, 0, 0, 0, -1, 0, -1, -1, 2, -1,~
    ## $ BILL_AMT1                  <dbl> 3913, 2682, 29239, 46990, 8617, 64400, 3679~
    ## $ BILL_AMT2                  <dbl> 3102, 1725, 14027, 48233, 5670, 57069, 4120~
    ## $ BILL_AMT3                  <dbl> 689, 2682, 13559, 49291, 35835, 57608, 4450~
    ## $ BILL_AMT4                  <dbl> 0, 3272, 14331, 28314, 20940, 19394, 542653~
    ## $ BILL_AMT5                  <dbl> 0, 3455, 14948, 28959, 19146, 19619, 483003~
    ## $ BILL_AMT6                  <dbl> 0, 3261, 15549, 29547, 19131, 20024, 473944~
    ## $ PAY_AMT1                   <dbl> 0, 0, 1518, 2000, 2000, 2500, 55000, 380, 3~
    ## $ PAY_AMT2                   <dbl> 689, 1000, 1500, 2019, 36681, 1815, 40000, ~
    ## $ PAY_AMT3                   <dbl> 0, 1000, 1000, 1200, 10000, 657, 38000, 0, ~
    ## $ PAY_AMT4                   <dbl> 0, 1000, 1000, 1100, 9000, 1000, 20239, 581~
    ## $ PAY_AMT5                   <dbl> 0, 0, 1000, 1069, 689, 1000, 13750, 1687, 1~
    ## $ PAY_AMT6                   <dbl> 0, 2000, 5000, 1000, 679, 800, 13770, 1542,~
    ## $ default.payment.next.month <int> 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0~

## Pre Procesamiento

``` r
banco$SEX <- as.factor(banco$SEX)
levels(banco$SEX) <- c("Male", "Female")

banco$EDUCATION <- as.factor(banco$EDUCATION)
levels(banco$EDUCATION) <- c("unknown", "graduate school","university","high school","others","unknown","unknown")

banco$MARRIAGE <- as.factor(banco$MARRIAGE)
levels(banco$MARRIAGE) <- c("Unknown","Married","Single","Others") #hay 4 etapas, (0,1,2,3)

#banco$default.payment.next.month <- as.factor(banco$default.payment.next.month)
#levels(banco$default.payment.next.month) <- c("Not default", "Default")
```

``` r
banco$PAY_0 <- as.factor(banco$PAY_0)
banco$PAY_2 <- as.factor(banco$PAY_2)
banco$PAY_3 <- as.factor(banco$PAY_3)
banco$PAY_4 <- as.factor(banco$PAY_4)
banco$PAY_5 <- as.factor(banco$PAY_5)
banco$PAY_6 <- as.factor(banco$PAY_6)
```

``` r
head(banco)
```

    ##   ID LIMIT_BAL    SEX       EDUCATION MARRIAGE AGE PAY_0 PAY_2 PAY_3 PAY_4
    ## 1  1     20000 Female      university  Married  24     2     2    -1    -1
    ## 2  2    120000 Female      university   Single  26    -1     2     0     0
    ## 3  3     90000 Female      university   Single  34     0     0     0     0
    ## 4  4     50000 Female      university  Married  37     0     0     0     0
    ## 5  5     50000   Male      university  Married  57    -1     0    -1     0
    ## 6  6     50000   Male graduate school   Single  37     0     0     0     0
    ##   PAY_5 PAY_6 BILL_AMT1 BILL_AMT2 BILL_AMT3 BILL_AMT4 BILL_AMT5 BILL_AMT6
    ## 1    -2    -2      3913      3102       689         0         0         0
    ## 2     0     2      2682      1725      2682      3272      3455      3261
    ## 3     0     0     29239     14027     13559     14331     14948     15549
    ## 4     0     0     46990     48233     49291     28314     28959     29547
    ## 5     0     0      8617      5670     35835     20940     19146     19131
    ## 6     0     0     64400     57069     57608     19394     19619     20024
    ##   PAY_AMT1 PAY_AMT2 PAY_AMT3 PAY_AMT4 PAY_AMT5 PAY_AMT6
    ## 1        0      689        0        0        0        0
    ## 2        0     1000     1000     1000        0     2000
    ## 3     1518     1500     1000     1000     1000     5000
    ## 4     2000     2019     1200     1100     1069     1000
    ## 5     2000    36681    10000     9000      689      679
    ## 6     2500     1815      657     1000     1000      800
    ##   default.payment.next.month
    ## 1                          1
    ## 2                          1
    ## 3                          0
    ## 4                          0
    ## 5                          0
    ## 6                          0

## Metodo Bayesiano

``` r
bayesiano <- stan_glm(default.payment.next.month ~ AGE + SEX + EDUCATION, data = banco, family = gaussian)
```

    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 1).
    ## Chain 1: 
    ## Chain 1: Gradient evaluation took 0 seconds
    ## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 1: Adjust your expectations accordingly!
    ## Chain 1: 
    ## Chain 1: 
    ## Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0.116 seconds (Warm-up)
    ## Chain 1:                2.15 seconds (Sampling)
    ## Chain 1:                2.266 seconds (Total)
    ## Chain 1: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 2).
    ## Chain 2: 
    ## Chain 2: Gradient evaluation took 0 seconds
    ## Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 2: Adjust your expectations accordingly!
    ## Chain 2: 
    ## Chain 2: 
    ## Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 2: 
    ## Chain 2:  Elapsed Time: 0.101 seconds (Warm-up)
    ## Chain 2:                2.162 seconds (Sampling)
    ## Chain 2:                2.263 seconds (Total)
    ## Chain 2: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 3).
    ## Chain 3: 
    ## Chain 3: Gradient evaluation took 0 seconds
    ## Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0 seconds.
    ## Chain 3: Adjust your expectations accordingly!
    ## Chain 3: 
    ## Chain 3: 
    ## Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 3: 
    ## Chain 3:  Elapsed Time: 0.133 seconds (Warm-up)
    ## Chain 3:                2.198 seconds (Sampling)
    ## Chain 3:                2.331 seconds (Total)
    ## Chain 3: 
    ## 
    ## SAMPLING FOR MODEL 'continuous' NOW (CHAIN 4).
    ## Chain 4: 
    ## Chain 4: Gradient evaluation took 0.001 seconds
    ## Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 10 seconds.
    ## Chain 4: Adjust your expectations accordingly!
    ## Chain 4: 
    ## Chain 4: 
    ## Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
    ## Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
    ## Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
    ## Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
    ## Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
    ## Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
    ## Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
    ## Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
    ## Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
    ## Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
    ## Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
    ## Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
    ## Chain 4: 
    ## Chain 4:  Elapsed Time: 0.103 seconds (Warm-up)
    ## Chain 4:                2.187 seconds (Sampling)
    ## Chain 4:                2.29 seconds (Total)
    ## Chain 4:

``` r
model_nb <- naiveBayes(default.payment.next.month ~ AGE + SEX + EDUCATION, banco, laplace=1)
```

``` r
bancotest <- banco[c(3,4,5:24)]



bancotest$sex <- NULL
bancotest$education <- NULL
bancotest$age <- NULL

str(bancotest)
```

    ## 'data.frame':    30000 obs. of  22 variables:
    ##  $ SEX      : Factor w/ 2 levels "Male","Female": 2 2 2 2 1 1 1 2 2 1 ...
    ##  $ EDUCATION: Factor w/ 5 levels "unknown","graduate school",..: 3 3 3 3 3 2 2 3 4 4 ...
    ##  $ MARRIAGE : Factor w/ 4 levels "Unknown","Married",..: 2 3 3 2 2 3 3 3 2 3 ...
    ##  $ AGE      : int  24 26 34 37 57 37 29 23 28 35 ...
    ##  $ PAY_0    : Factor w/ 11 levels "-2","-1","0",..: 5 2 3 3 2 3 3 3 3 1 ...
    ##  $ PAY_2    : Factor w/ 11 levels "-2","-1","0",..: 5 5 3 3 3 3 3 2 3 1 ...
    ##  $ PAY_3    : Factor w/ 11 levels "-2","-1","0",..: 2 3 3 3 2 3 3 2 5 1 ...
    ##  $ PAY_4    : Factor w/ 11 levels "-2","-1","0",..: 2 3 3 3 3 3 3 3 3 1 ...
    ##  $ PAY_5    : Factor w/ 10 levels "-2","-1","0",..: 1 3 3 3 3 3 3 3 3 2 ...
    ##  $ PAY_6    : Factor w/ 10 levels "-2","-1","0",..: 1 4 3 3 3 3 3 2 3 2 ...
    ##  $ BILL_AMT1: num  3913 2682 29239 46990 8617 ...
    ##  $ BILL_AMT2: num  3102 1725 14027 48233 5670 ...
    ##  $ BILL_AMT3: num  689 2682 13559 49291 35835 ...
    ##  $ BILL_AMT4: num  0 3272 14331 28314 20940 ...
    ##  $ BILL_AMT5: num  0 3455 14948 28959 19146 ...
    ##  $ BILL_AMT6: num  0 3261 15549 29547 19131 ...
    ##  $ PAY_AMT1 : num  0 0 1518 2000 2000 ...
    ##  $ PAY_AMT2 : num  689 1000 1500 2019 36681 ...
    ##  $ PAY_AMT3 : num  0 1000 1000 1200 10000 657 38000 0 432 0 ...
    ##  $ PAY_AMT4 : num  0 1000 1000 1100 9000 ...
    ##  $ PAY_AMT5 : num  0 0 1000 1069 689 ...
    ##  $ PAY_AMT6 : num  0 2000 5000 1000 679 ...

``` r
#pred_nb <- predict(model_nb, newdata = bancotest)
#confusionMatrix(data=pred_nb, reference = banco$default.payment.next.month)
```

``` r
#library(ROCR)

#pred_test_nb <- predict(model_nb, newdata = bancotest, type="raw")
#p_test_nb <- prediction(pred_test_nb[,2], banco$Survived)
#perf_nb <- performance(p_test_nb, "tpr", "fpr")
#plot(perf_nb, colorize=T)
#performance(p_test_nb, "auc")@y.values
```
