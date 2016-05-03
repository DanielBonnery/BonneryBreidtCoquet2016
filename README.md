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
|theta.bar  |1.32943  |-200.8807        |1406.214   |8.664352e-06       |NA                  |
|theta.ht   |4.070556 |1.733317         |6.650471   |0.02875136         |NA                  |
|theta.hat  |4.016571 |0.4125647        |1          |0.004797144        |1                   |
|theta.full |NA       |NA               |NA         |NA                 |NA                  |



|Estimator  |Mean     |X..Relative.Bias |RMSE.Ratio |Empirical.Variance |Asymptotic.Variance |
|:----------|:--------|:----------------|:----------|:------------------|:-------------------|
|theta.bar  |1.32927  |-200.9171        |116.1432   |1.852971e-05       |NA                  |
|theta.ht   |4.005045 |0.1259766        |1.700214   |0.1043915          |NA                  |
|theta.hat  |4.098965 |2.414393         |1          |0.05161991         |1                   |
|theta.full |NA       |NA               |NA         |NA                 |NA                  |



|Estimator  |Mean     |X..Relative.Bias |RMSE.Ratio |Empirical.Variance |Asymptotic.Variance |
|:----------|:--------|:----------------|:----------|:------------------|:-------------------|
|theta.bar  |1.331841 |-200.3361        |419.8108   |3.010852e-05       |NA                  |
|theta.ht   |4.064297 |1.581987         |7.594252   |0.1246484          |NA                  |
|theta.hat  |4.1252   |3.035004         |1          |0.001282842        |1                   |
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
