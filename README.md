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
## Warning in data(table1, table2, table3): data set 'table2' not found
```
                                                                                                
### Table 1


|Estimator  |Mean     |X..Relative.Bias |RMSE.Ratio |Empirical.Variance |Asymptotic.Variance |
|:----------|:--------|:----------------|:----------|:------------------|:-------------------|
|theta.bar  |4.094384 |2.305211         |1.382965   |0.01938998         |NA                  |
|theta.ht   |4.014403 |0.3587772        |1.558896   |0.03169084         |NA                  |
|theta.hat  |4.006796 |0.1696069        |1          |0.02041591         |133470.7            |
|theta.full |NA       |NA               |NA         |NA                 |NA                  |



|Estimator  |Mean     |X..Relative.Bias |RMSE.Ratio |Empirical.Variance |Asymptotic.Variance |
|:----------|:--------|:----------------|:----------|:------------------|:-------------------|
|theta.bar  |4.854723 |17.60602         |14.52058   |0.03228804         |NA                  |
|theta.ht   |4.043793 |1.08298          |2.07893    |0.107299           |NA                  |
|theta.hat  |4.010812 |0.2695632        |1          |0.05241823         |263102.7            |
|theta.full |NA       |NA               |NA         |NA                 |NA                  |



|Estimator  |Mean     |X..Relative.Bias |RMSE.Ratio |Empirical.Variance |Asymptotic.Variance |
|:----------|:--------|:----------------|:----------|:------------------|:-------------------|
|theta.bar  |5.578945 |28.30186         |33.22031   |0.05454619         |NA                  |
|theta.ht   |4.051705 |1.276134         |1.830045   |0.1376699          |NA                  |
|theta.hat  |4.015815 |0.3938151        |1          |0.07643834         |403041.5            |
|theta.full |NA       |NA               |NA         |NA                 |NA                  |

### Table 2




### Table 3


|Selection    |Estimator |Mean   |% Relative Bias |RMSE Ratio |Empirical Variance |Average Estimated Variance |Variance Ratio |
|:------------|:---------|:------|:---------------|:----------|:------------------|:--------------------------|:--------------|
|Unstratified |Naive     |36.949 |-7.286          |20.995     |0.188              |0.186                      |0.989          |
|Unstratified |Pseudo    |39.805 |-0.122          |1.106      |0.452              |0.419                      |0.926          |
|Unstratified |Sample    |39.827 |-0.065          |1          |0.41               |0.388                      |0.945          |
|Stratified   |Naive     |36.932 |-7.328          |113.911    |0.006              |0.188                      |30.271         |
|Stratified   |Pseudo    |39.858 |0.013           |2.448      |0.184              |0.169                      |0.923          |
|Stratified   |Sample    |39.848 |-0.012          |1          |0.075              |0.066                      |0.886          |
