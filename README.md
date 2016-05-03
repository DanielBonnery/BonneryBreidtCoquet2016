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


|Estimator  |Mean     |Bias        |Variance     |M.S.E.     |V        |Relative.Variance |
|:----------|:--------|:-----------|:------------|:----------|:--------|:-----------------|
|theta.bar  |1.333422 |-2.666578   |2.189399e-05 |7.110662   |NA       |0.001072398       |
|theta.ht   |4.014403 |0.01440276  |0.03169084   |0.03189828 |NA       |1.552262          |
|theta.hat  |4.006796 |0.006795801 |0.02041591   |0.02046209 |133470.7 |1                 |
|theta.full |NA       |NA          |NA           |NA         |NULL     |NA                |



|Estimator  |Mean     |Bias       |Variance     |M.S.E.     |V        |Relative.Variance |
|:----------|:--------|:----------|:------------|:----------|:--------|:-----------------|
|theta.bar  |1.333391 |-2.666609  |2.301469e-05 |7.110828   |NA       |0.0004390589      |
|theta.ht   |4.043793 |0.04379349 |0.107299     |0.1092168  |NA       |2.046978          |
|theta.hat  |4.010812 |0.01081167 |0.05241823   |0.05253512 |263102.7 |1                 |
|theta.full |NA       |NA         |NA           |NA         |NULL     |NA                |



|Estimator  |Mean     |Bias       |Variance     |M.S.E.     |V        |Relative.Variance |
|:----------|:--------|:----------|:------------|:----------|:--------|:-----------------|
|theta.bar  |1.333192 |-2.666808  |2.263686e-05 |7.111885   |NA       |0.0002961454      |
|theta.ht   |4.051705 |0.05170518 |0.1376699    |0.1403433  |NA       |1.801058          |
|theta.hat  |4.015815 |0.01581488 |0.07643834   |0.07668845 |403041.5 |1                 |
|theta.full |NA       |NA         |NA           |NA         |NULL     |NA                |

### Table 2




