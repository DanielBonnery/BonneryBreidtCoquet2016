---
output: md_document
---


# Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population

`pubBonneryBreidtCoquet2016` is a R package that contains the code to reproduce the simulations of ["Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population"](http://www.e-publications.org/ims/submission/BEJ/user/submissionFile/23537?confirm=3b2ff5b3) by Bonnery, Breidt and Coquet

## Execution

```r
devtools::github("DanielBonnery/pubBonneryBreidtCoquet2016")
library(pubBonneryBreidtCoquet2016)
demo(table1)
demo(table2)
demo(table3)
```

##Output



                                                                                                
### Table 1


|Estimator |$\theta$ |$\xi$ |Other paramters |Mean |% Relative Bias |RMSE Ratio |Empirical Variance |Asymptotic Variance |
|:---------|:--------|:-----|:---------------|:----|:---------------|:----------|:------------------|:-------------------|
|Naive     |4        |0.1   |NULL            |4.1  |2.35            |1.35614    |0.0176681          |NA                  |
|Pseudo    |4        |0.1   |NULL            |4.01 |0.282           |1.57986    |0.0312076          |NA                  |
|Sample    |4        |0.1   |NULL            |4.01 |0.153           |1          |0.0197967          |0.0197794           |
|Full      |4        |0.1   |NULL            |NA   |NA              |NA         |NA                 |NA                  |



|Estimator |$\theta$ |$\xi$ |Other paramters |Mean |% Relative Bias |RMSE Ratio |Empirical Variance |Asymptotic Variance |
|:---------|:--------|:-----|:---------------|:----|:---------------|:----------|:------------------|:-------------------|
|Naive     |4        |1     |NULL            |4.85 |17.6            |15.5287    |0.031158           |NA                  |
|Pseudo    |4        |1     |NULL            |4.04 |1.1             |2.21818    |0.106808           |NA                  |
|Sample    |4        |1     |NULL            |4.02 |0.374           |1          |0.0488144          |0.0429614           |
|Full      |4        |1     |NULL            |NA   |NA              |NA         |NA                 |NA                  |



|Estimator |$\theta$ |$\xi$ |Other paramters |Mean |% Relative Bias |RMSE Ratio |Empirical Variance |Asymptotic Variance |
|:---------|:--------|:-----|:---------------|:----|:---------------|:----------|:------------------|:-------------------|
|Naive     |4        |2     |NULL            |5.58 |28.3            |33.2311    |0.0545275          |NA                  |
|Pseudo    |4        |2     |NULL            |4.05 |1.27            |1.83004    |0.137634           |NA                  |
|Sample    |4        |2     |NULL            |4.02 |0.391           |1          |0.0764089          |0.100191            |
|Full      |4        |2     |NULL            |NA   |NA              |NA         |NA                 |NA                  |

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
