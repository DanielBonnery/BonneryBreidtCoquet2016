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

### Table 3

    library(knitr)
    #kable(table1)
    kable(table3)

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
