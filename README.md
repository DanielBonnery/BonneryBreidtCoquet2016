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
|theta.bar  |1.333422 |-199.9801        |347.5041   |2.189399e-05       |NA                  |
|theta.ht   |4.014403 |0.3587772        |1.558896   |0.03169084         |NA                  |
|theta.hat  |4.006796 |0.1696069        |1          |0.02041591         |133470.7            |
|theta.full |NA       |NA               |NA         |NA                 |NA                  |



|Estimator  |Mean     |X..Relative.Bias |RMSE.Ratio |Empirical.Variance |Asymptotic.Variance |
|:----------|:--------|:----------------|:----------|:------------------|:-------------------|
|theta.bar  |1.333391 |-199.9871        |135.3538   |2.301469e-05       |NA                  |
|theta.ht   |4.043793 |1.08298          |2.07893    |0.107299           |NA                  |
|theta.hat  |4.010812 |0.2695632        |1          |0.05241823         |263102.7            |
|theta.full |NA       |NA               |NA         |NA                 |NA                  |



|Estimator  |Mean     |X..Relative.Bias |RMSE.Ratio |Empirical.Variance |Asymptotic.Variance |
|:----------|:--------|:----------------|:----------|:------------------|:-------------------|
|theta.bar  |1.333192 |-200.0317        |92.73737   |2.263686e-05       |NA                  |
|theta.ht   |4.051705 |1.276134         |1.830045   |0.1376699          |NA                  |
|theta.hat  |4.015815 |0.3938151        |1          |0.07643834         |403041.5            |
|theta.full |NA       |NA               |NA         |NA                 |NA                  |

### Table 2




### Table 3


