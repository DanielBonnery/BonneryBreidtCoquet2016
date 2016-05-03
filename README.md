Asymptotics for the maximum sample likelihood estimator under informative selection from a finite population
============================================================================================================

`pubBonneryBreidtCoquet2016` is a R package that contains the code to
reproduce the simulations of ["Asymptotics for the maximum sample
likelihood estimator under informative selection from a finite
population"](http://www.e-publications.org/ims/submission/BEJ/user/submissionFile/23537?confirm=3b2ff5b3)
by Bonnery, Breidt and Coquet

Execution
---------

Execute in R:

    devtools::github("DanielBonnery/pubBonneryBreidtCoquet2016")
    library(pubBonneryBreidtCoquet2016)
    demo(table1)
    demo(table2)
    demo(table3)

Output
------

    ## Warning in data(table1, table2, table3): data set 'table2' not found

### Table 1

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left">Mean</th>
<th align="left">X..Relative.Bias</th>
<th align="left">RMSE.Ratio</th>
<th align="left">Empirical.Variance</th>
<th align="left">Asymptotic.Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">theta.bar</td>
<td align="left">1.333422</td>
<td align="left">-199.9801</td>
<td align="left">347.5041</td>
<td align="left">2.189399e-05</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">theta.ht</td>
<td align="left">4.014403</td>
<td align="left">0.3587772</td>
<td align="left">1.558896</td>
<td align="left">0.03169084</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">theta.hat</td>
<td align="left">4.006796</td>
<td align="left">0.1696069</td>
<td align="left">1</td>
<td align="left">0.02041591</td>
<td align="left">133470.7</td>
</tr>
<tr class="even">
<td align="left">theta.full</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left">Mean</th>
<th align="left">X..Relative.Bias</th>
<th align="left">RMSE.Ratio</th>
<th align="left">Empirical.Variance</th>
<th align="left">Asymptotic.Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">theta.bar</td>
<td align="left">1.333391</td>
<td align="left">-199.9871</td>
<td align="left">135.3538</td>
<td align="left">2.301469e-05</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">theta.ht</td>
<td align="left">4.043793</td>
<td align="left">1.08298</td>
<td align="left">2.07893</td>
<td align="left">0.107299</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">theta.hat</td>
<td align="left">4.010812</td>
<td align="left">0.2695632</td>
<td align="left">1</td>
<td align="left">0.05241823</td>
<td align="left">263102.7</td>
</tr>
<tr class="even">
<td align="left">theta.full</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

<table>
<thead>
<tr class="header">
<th align="left">Estimator</th>
<th align="left">Mean</th>
<th align="left">X..Relative.Bias</th>
<th align="left">RMSE.Ratio</th>
<th align="left">Empirical.Variance</th>
<th align="left">Asymptotic.Variance</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">theta.bar</td>
<td align="left">1.333192</td>
<td align="left">-200.0317</td>
<td align="left">92.73737</td>
<td align="left">2.263686e-05</td>
<td align="left">NA</td>
</tr>
<tr class="even">
<td align="left">theta.ht</td>
<td align="left">4.051705</td>
<td align="left">1.276134</td>
<td align="left">1.830045</td>
<td align="left">0.1376699</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">theta.hat</td>
<td align="left">4.015815</td>
<td align="left">0.3938151</td>
<td align="left">1</td>
<td align="left">0.07643834</td>
<td align="left">403041.5</td>
</tr>
<tr class="even">
<td align="left">theta.full</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

### Table 2

### Table 3

<table>
<thead>
<tr class="header">
<th align="left">Selection</th>
<th align="left">Estimator</th>
<th align="left">Mean</th>
<th align="left">% Relative Bias</th>
<th align="left">RMSE Ratio</th>
<th align="left">Empirical Variance</th>
<th align="left">Average Estimated Variance</th>
<th align="left">Variance Ratio</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Unstratified</td>
<td align="left">Naive</td>
<td align="left">36.949</td>
<td align="left">-7.286</td>
<td align="left">20.995</td>
<td align="left">0.188</td>
<td align="left">0.186</td>
<td align="left">0.989</td>
</tr>
<tr class="even">
<td align="left">Unstratified</td>
<td align="left">Pseudo</td>
<td align="left">39.805</td>
<td align="left">-0.122</td>
<td align="left">1.106</td>
<td align="left">0.452</td>
<td align="left">0.419</td>
<td align="left">0.926</td>
</tr>
<tr class="odd">
<td align="left">Unstratified</td>
<td align="left">Sample</td>
<td align="left">39.827</td>
<td align="left">-0.065</td>
<td align="left">1</td>
<td align="left">0.41</td>
<td align="left">0.388</td>
<td align="left">0.945</td>
</tr>
<tr class="even">
<td align="left">Stratified</td>
<td align="left">Naive</td>
<td align="left">36.932</td>
<td align="left">-7.328</td>
<td align="left">113.911</td>
<td align="left">0.006</td>
<td align="left">0.188</td>
<td align="left">30.271</td>
</tr>
<tr class="odd">
<td align="left">Stratified</td>
<td align="left">Pseudo</td>
<td align="left">39.858</td>
<td align="left">0.013</td>
<td align="left">2.448</td>
<td align="left">0.184</td>
<td align="left">0.169</td>
<td align="left">0.923</td>
</tr>
<tr class="even">
<td align="left">Stratified</td>
<td align="left">Sample</td>
<td align="left">39.848</td>
<td align="left">-0.012</td>
<td align="left">1</td>
<td align="left">0.075</td>
<td align="left">0.066</td>
<td align="left">0.886</td>
</tr>
</tbody>
</table>
