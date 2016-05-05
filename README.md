# Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population

`pubBonneryBreidtCoquet2016` is a R package that contains the source code to reproduce the simulations of ["Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population"](http://www.e-publications.org/ims/submission/BEJ/user/submissionFile/23537?confirm=3b2ff5b3) by Bonnery, Breidt and Coquet

## Execution

```r
devtools::install_github("DanielBonnery/pubBonneryBreidtCoquet2016")
library(pubBonneryBreidtCoquet2016)
demo(table1)
demo(table2)
demo(table3)
```

##Output



                                                                                                
### Table 1


|Estimator |$\theta$ |$\xi$ |Conditional to |Mean |% Relative Bias |RMSE Ratio |Empirical Variance |Asymptotic Variance |
|:---------|:--------|:-----|:--------------|:----|:---------------|:----------|:------------------|:-------------------|
|Naive     |4        |0.1   |NULL           |4.1  |2.39            |1.35043    |0.0181758          |NA                  |
|Pseudo    |4        |0.1   |NULL           |4.01 |0.311           |1.53996    |0.0309899          |NA                  |
|Sample    |4        |0.1   |NULL           |4.01 |0.143           |1          |0.0201916          |0.0200028           |
|Full      |4        |0.1   |NULL           |NaN  |NaN             |NA         |NA                 |NA                  |



|Estimator |$\theta$ |$\xi$ |Conditional to |Mean |% Relative Bias |RMSE Ratio |Empirical Variance |Asymptotic Variance |
|:---------|:--------|:-----|:--------------|:----|:---------------|:----------|:------------------|:-------------------|
|Naive     |4        |1     |NULL           |4.86 |21.5            |15.3729    |0.031139           |NA                  |
|Pseudo    |4        |1     |NULL           |4.05 |1.19            |2.13148    |0.104135           |NA                  |
|Sample    |4        |1     |NULL           |4.02 |0.416           |1          |0.0496496          |0.0509886           |
|Full      |4        |1     |NULL           |NaN  |NaN             |NA         |NA                 |NA                  |



|Estimator |$\theta$ |$\xi$ |Conditional to |Mean |% Relative Bias |RMSE Ratio |Empirical Variance |Asymptotic Variance |
|:---------|:--------|:-----|:--------------|:----|:---------------|:----------|:------------------|:-------------------|
|Naive     |4        |2     |NULL           |5.59 |39.7            |33.8249    |0.0567029          |NA                  |
|Pseudo    |4        |2     |NULL           |4.06 |1.4             |1.87451    |0.139452           |NA                  |
|Sample    |4        |2     |NULL           |4.02 |0.419           |1          |0.0757842          |0.0771277           |
|Full      |4        |2     |NULL           |NaN  |NaN             |NA         |NA                 |NA                  |

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
