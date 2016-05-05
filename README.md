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


|Estimator |$\theta$      |$\xi$ |Conditional to |Mean                |% Relative Bias       |RMSE Ratio                 |Empirical Variance                 |Asymptotic Variance                |
|:---------|:-------------|:-----|:--------------|:-------------------|:---------------------|:--------------------------|:----------------------------------|:----------------------------------|
|Naive     |0.5, 1.0, 2.0 |2     |0.1, 1.0, 1.0  |2.270, 0.788, 1.780 |355.0, -21.2, -11.2   |89.45800, 3.22033, 6.34327 |0.0354351, 0.0148147, 0.0083901    |NA                                 |
|Pseudo    |0.5, 1.0, 2.0 |2     |0.1, 1.0, 1.0  |0.508, 1.000, 1.980 |1.550, 0.147, -0.810  |1.99281, 1.90524, 1.87630  |0.0707659, 0.0354644, 0.0171822    |NA                                 |
|Sample    |0.5, 1.0, 2.0 |2     |0.1, 1.0, 1.0  |0.512, 0.995, 1.990 |2.330, -0.538, -0.324 |1, 1, 1                    |0.03540450, 0.01858640, 0.00925542 |0.03223440, 0.01784980, 0.00885569 |
|Full      |0.5, 1.0, 2.0 |2     |0.1, 1.0, 1.0  |NaN, NaN, NaN       |NaN, NaN, NaN         |NA, NA, NA                 |NA, NA, NA                         |NA                                 |



|Estimator |$\theta$      |$\xi$ |Conditional to |Mean                |% Relative Bias         |RMSE Ratio                 |Empirical Variance                 |Asymptotic Variance             |
|:---------|:-------------|:-----|:--------------|:-------------------|:-----------------------|:--------------------------|:----------------------------------|:-------------------------------|
|Naive     |0.5, 1.0, 2.0 |2     |1, 1, 1        |2.220, 0.796, 1.790 |344.0, -20.4, -10.6     |68.79590, 3.05235, 5.05403 |0.03614270, 0.01484920, 0.00756908 |NA                              |
|Pseudo    |0.5, 1.0, 2.0 |2     |1, 1, 1        |0.50, 1.00, 1.98    |0.0543, 0.3690, -0.8880 |1.63399, 1.88679, 1.60761  |0.0712723, 0.0348923, 0.0163413    |NA                              |
|Sample    |0.5, 1.0, 2.0 |2     |1, 1, 1        |0.513, 0.993, 1.990 |2.600, -0.661, -0.330   |1, 1, 1                    |0.0434493, 0.0184565, 0.0103178    |0.0416175, 0.0177885, 0.0108431 |
|Full      |0.5, 1.0, 2.0 |2     |1, 1, 1        |NaN, NaN, NaN       |NaN, NaN, NaN           |NA, NA, NA                 |NA, NA, NA                         |NA                              |



|Estimator |$\theta$      |$\xi$ |Conditional to |Mean                |% Relative Bias         |RMSE Ratio                   |Empirical Variance                 |Asymptotic Variance                |
|:---------|:-------------|:-----|:--------------|:-------------------|:-----------------------|:----------------------------|:----------------------------------|:----------------------------------|
|Naive     |0.5, 1.0, 2.0 |2     |10, 1, 1       |1.140, 0.967, 1.960 |128.00, -3.30, -1.83    |8.821740, 0.997874, 1.050360 |0.03524990, 0.01565320, 0.00772124 |NA                                 |
|Pseudo    |0.5, 1.0, 2.0 |2     |10, 1, 1       |0.499, 1.000, 1.980 |-0.108, 0.233, -1.180   |1.64251, 2.37836, 2.27889    |0.0832800, 0.0398984, 0.0191108    |NA                                 |
|Sample    |0.5, 1.0, 2.0 |2     |10, 1, 1       |0.50, 1.00, 1.99    |0.0513, 0.1220, -0.5050 |1, 1, 1                      |0.05070290, 0.01677640, 0.00853041 |0.05223770, 0.01538530, 0.00909232 |
|Full      |0.5, 1.0, 2.0 |2     |10, 1, 1       |NaN, NaN, NaN       |NaN, NaN, NaN           |NA, NA, NA                   |NA, NA, NA                         |NA                                 |


### Table 3


|Selection    |Estimator |Mean   |% Relative Bias |RMSE Ratio |Empirical Variance |Average Estimated Variance |Variance Ratio |
|:------------|:---------|:------|:---------------|:----------|:------------------|:--------------------------|:--------------|
|Unstratified |Naive     |36.949 |-7.286          |20.995     |0.188              |0.186                      |0.989          |
|Unstratified |Pseudo    |39.805 |-0.122          |1.106      |0.452              |0.419                      |0.926          |
|Unstratified |Sample    |39.827 |-0.065          |1          |0.41               |0.388                      |0.945          |
|Stratified   |Naive     |36.932 |-7.328          |113.911    |0.006              |0.188                      |30.271         |
|Stratified   |Pseudo    |39.858 |0.013           |2.448      |0.184              |0.169                      |0.923          |
|Stratified   |Sample    |39.848 |-0.012          |1          |0.075              |0.066                      |0.886          |
