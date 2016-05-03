---
title: "R code for 'Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population'"
author: Daniel Bonnery
date: May 2d, 2016
output: md_document
---


# Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population

`pubBonneryBreidtCoquet2016` is a R package that contains the code to reproduce the simulations of ["Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population"](http://www.e-publications.org/ims/submission/BEJ/user/submissionFile/23537?confirm=3b2ff5b3) by Bonnery, Breidt and Coquet

## Execution
Execute in R:

```r
devtools::github("DanielBonnery/pubBonneryBreidtCoquet2016")
library(pubBonneryBreidtCoquet2016)
demo(table1)
demo(table2)
demo(table3)
```

##Output



```
## Warning in FUN(X[[i]], ...): restarting interrupted promise evaluation
```

```
## Warning in FUN(X[[i]], ...): internal error -3 in R_decompress1
```

```
## Error in FUN(X[[i]], ...): lazy-load database '/home/daniel/R/x86_64-pc-linux-gnu-library/3.2/pubBonneryBreidtCoquet2016/data/Rdata.rdb' is corrupt
```

```
## Warning in data(table1, table2, table3): data set 'table2' not found
```
                                                                                                
### Table 1


```
## [[1]]
## 
## 
## |Estimator  |Mean     |Bias        |Variance     |M.S.E.     |V        |Relative.Variance |
## |:----------|:--------|:-----------|:------------|:----------|:--------|:-----------------|
## |theta.bar  |1.333422 |-2.666578   |2.189399e-05 |7.110662   |NA       |0.001072398       |
## |theta.ht   |4.014403 |0.01440276  |0.03169084   |0.03189828 |NA       |1.552262          |
## |theta.hat  |4.006796 |0.006795801 |0.02041591   |0.02046209 |133470.7 |1                 |
## |theta.full |NA       |NA          |NA           |NA         |NULL     |NA                |
## 
## [[2]]
## 
## 
## |Estimator  |Mean     |Bias       |Variance     |M.S.E.     |V        |Relative.Variance |
## |:----------|:--------|:----------|:------------|:----------|:--------|:-----------------|
## |theta.bar  |1.333391 |-2.666609  |2.301469e-05 |7.110828   |NA       |0.0004390589      |
## |theta.ht   |4.043793 |0.04379349 |0.107299     |0.1092168  |NA       |2.046978          |
## |theta.hat  |4.010812 |0.01081167 |0.05241823   |0.05253512 |263102.7 |1                 |
## |theta.full |NA       |NA         |NA           |NA         |NULL     |NA                |
## 
## [[3]]
## 
## 
## |Estimator  |Mean     |Bias       |Variance     |M.S.E.     |V        |Relative.Variance |
## |:----------|:--------|:----------|:------------|:----------|:--------|:-----------------|
## |theta.bar  |1.333192 |-2.666808  |2.263686e-05 |7.111885   |NA       |0.0002961454      |
## |theta.ht   |4.051705 |0.05170518 |0.1376699    |0.1403433  |NA       |1.801058          |
## |theta.hat  |4.015815 |0.01581488 |0.07643834   |0.07668845 |403041.5 |1                 |
## |theta.full |NA       |NA         |NA           |NA         |NULL     |NA                |
```

### Table 2


```
## Error in lapply(table2, kable): object 'table2' not found
```


### Table 3


|Selection    |Estimator |Mean   |% Relative Bias |RMSE Ratio |Empirical Variance |Average Estimated Variance |Variance Ratio |
|:------------|:---------|:------|:---------------|:----------|:------------------|:--------------------------|:--------------|
|Unstratified |Naive     |36.949 |-7.286          |20.995     |0.188              |0.186                      |0.989          |
|Unstratified |Pseudo    |39.805 |-0.122          |1.106      |0.452              |0.419                      |0.926          |
|Unstratified |Sample    |39.827 |-0.065          |1          |0.41               |0.388                      |0.945          |
|Stratified   |Naive     |36.932 |-7.328          |113.911    |0.006              |0.188                      |30.271         |
|Stratified   |Pseudo    |39.858 |0.013           |2.448      |0.184              |0.169                      |0.923          |
|Stratified   |Sample    |39.848 |-0.012          |1          |0.075              |0.066                      |0.886          |
